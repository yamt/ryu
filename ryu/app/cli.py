# Copyright (C) 2013 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2013 YAMAMOTO Takashi <yamamoto at valinux co jp>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# a management cli application.

from __future__ import print_function

import cmd
import functools
import gevent
import gevent.server
import logging
import os
import paramiko
import pty
import select
import sys

from oslo.config import cfg

from ryu import version
from ryu.base import app_manager
from ryu.base import management
from ryu import call_via_pipe
from ryu.plogger import PrefixedLogger


CONF = cfg.CONF
CONF.register_opts([
    cfg.ListOpt('cli-transports', default=[], help='cli transports to enable'),
    cfg.StrOpt('cli-ssh-host', default='localhost',
               help='cli ssh listen host'),
    cfg.IntOpt('cli-ssh-port', default=4990, help='cli ssh listen port'),
    cfg.StrOpt('cli-ssh-hostkey', default=None, help='cli ssh host key file'),
    cfg.StrOpt('cli-ssh-username', default=None, help='cli ssh username'),
    cfg.StrOpt('cli-ssh-password', default=None, help='cli ssh password')
])


def command_log(f):
    @functools.wraps(f)
    def wrapper(self, params):
        name = wrapper.__name__
        assert(name.startswith('do_'))
        command_name = name[len('do_'):]
        self.logger.info("command %s %s" % (command_name, params))
        f(self, params)
    return wrapper


class CliCmd(cmd.Cmd):
    prompt = 'ryu-manager %s> ' % version

    def __init__(self, rpipe, wpipe, *args, **kwargs):
        cmd.Cmd.__init__(self, *args, **kwargs)
        # it's safe to use the same set of pipes as far as we are
        # single-threaded.
        self.management = call_via_pipe.CallViaPipe(rpipe, wpipe, "management")
        self.logger = call_via_pipe.CallViaPipe(rpipe, wpipe, "logger")

    @command_log
    def do_set_log_level(self, params):
        '''<logger> <level>
        set log level of the specified logger
        '''
        try:
            params = params.split()
            name = params[0]
            newlvl = int(params[1])
        except (ValueError, IndexError):
            print('invalid parameter')
            return
        try:
            oldlvl = self.management.get_log_level(name)
            self.management.set_log_level(name, newlvl)
        except LookupError:
            print('logger %s is unknown' % (name,))
            return
        print('logger %s level %s -> %s' % (name, oldlvl, newlvl))

    @command_log
    def do_show_bricks(self, params):
        '''
        show a list of configured bricks
        '''
        map(lambda b: print('%s' % (b,)), self.management.list_bricks())

    @command_log
    def do_show_loggers(self, params):
        '''
        show loggers
        '''
        map(lambda name: print('logger %s level %s' %
                               (name, self.management.get_log_level(name))),
            self.management.list_loggers())

    @command_log
    def do_show_options(self, params):
        '''
        show options
        '''
        # NOTE: this shows CONF of the child process.
        # currently it isn't a problem because we don't modify CONF
        # after startup.
        class MyLogger:
            def log(mylogger_self, lvl, fmt, *args):
                print(fmt % args)
        CONF.log_opt_values(MyLogger(), None)


class SshServer(paramiko.ServerInterface):
    def __init__(self, logger, *args, **kwargs):
        super(SshServer, self).__init__(*args, **kwargs)
        self._logger = logger

    def check_auth_password(self, username, password):
        print("check_auth_password", username, password)
        if username == CONF.cli_ssh_username and \
                password == CONF.cli_ssh_password:
            return paramiko.AUTH_SUCCESSFUL
        return paramiko.AUTH_FAILED

    def check_channel_request(self, kind, chanid):
        if kind == 'session':
            return paramiko.OPEN_SUCCEEDED
        return paramiko.OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED

    def check_channel_shell_request(self, chan):
        gevent.spawn(self.handle_shell_request)
        return True

    def check_channel_pty_request(self, chan, term, width, height,
                                  pixelwidth, pixelheight, modes):
        self.term = term
        return True

    def pty_loop(self, chan, fd, rpipe, wpipe):
        while True:
            rfds, wfds, xfds = select.select([chan.fileno(), fd, rpipe], [],[])
            if fd in rfds:
                data = os.read(fd, 1024)
                if len(data) == 0:
                    break
                chan.send(data)
            if chan.fileno() in rfds:
                data = chan.recv(1024)
                if len(data) == 0:
                    break
                os.write(fd, data)
            if rpipe in rfds:
                logger = self.logger
                call_via_pipe.serve(rpipe, wpipe, [locals(), globals()])
        chan.close()

    def handle_shell_request(self):
        self.logger.info("session start")
        chan = self.transport.accept(20)
        if not chan:
            self.logger.info("transport.accept timed out")
            return
        rpipe_request, wpipe_request = os.pipe()
        rpipe_reply, wpipe_reply = os.pipe()
        child_pid, master_fd = pty.fork()
        if not child_pid:
            os.close(rpipe_request)
            os.close(wpipe_reply)
            CliCmd(rpipe_reply, wpipe_request).cmdloop()
            return
        os.close(wpipe_request)
        os.close(rpipe_reply)
        self.pty_loop(chan, master_fd, rpipe_request, wpipe_reply)
        self.logger.info("session end")
        os.kill(child_pid)
        os.waitpid(child_pid)

    def streamserver_handle(self, sock, addr):
        self.logger = PrefixedLogger(self._logger, "CLI-SSH %s" % (addr,))
        transport = paramiko.Transport(sock)
        transport.load_server_moduli()
        host_key = paramiko.RSAKey.from_private_key_file(CONF.cli_ssh_hostkey)
        transport.add_server_key(host_key)
        self.transport = transport
        transport.start_server(server = self)


class Cli(app_manager.RyuApp):
    def __init__(self, *args, **kwargs):
        super(Cli, self).__init__(*args, **kwargs)
        something_started = False
        if 'ssh' in CONF.cli_transports:
            self.logger.info("starting ssh server at %s:%d",
                             CONF.cli_ssh_host, CONF.cli_ssh_port)
            gevent.spawn(self.ssh_thread)
            something_started = True
        if not something_started:
            self.logger.warn("cli app has no valid transport configured")
            self.logger.debug("cli-transports=%s", CONF.cli_transports)
            self.logger.debug("cli-ssh-hostkey=%s", CONF.cli_ssh_hostkey)

    def ssh_thread(self):
        logging.getLogger('paramiko')
        logging.getLogger('paramiko.transport')
        ssh_server = SshServer(self.logger)
        server = gevent.server.StreamServer((CONF.cli_ssh_host,
                                            CONF.cli_ssh_port),
                                            ssh_server.streamserver_handle)
        server.serve_forever()
