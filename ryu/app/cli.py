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
# this module requires telnetsrv.

import gevent
import gevent.server
import logging
import functools

from oslo.config import cfg

from telnetsrv.green import command
from telnetsrv.green import TelnetHandler

from ryu import version
from ryu.base import app_manager
from ryu.base import management


CONF = cfg.CONF
CONF.register_opts([
    cfg.ListOpt('cli-transports', default=[], help='cli transports to enable'),
    cfg.StrOpt('cli-telnet-host', default='localhost',
               help='cli telnet listen host'),
    cfg.IntOpt('cli-telnet-port', default=4989, help='cli telnet listen port'),
    cfg.StrOpt('cli-ssh-host', default='localhost',
               help='cli ssh listen host'),
    cfg.IntOpt('cli-ssh-port', default=4990, help='cli ssh listen port'),
    cfg.StrOpt('cli-ssh-hostkey', default=None, help='cli ssh host key file'),
    cfg.StrOpt('cli-ssh-username', default=None, help='cli ssh username'),
    cfg.StrOpt('cli-ssh-password', default=None, help='cli ssh password')
])


class PrefixedLogger(object):
    def __init__(self, logger, prefix):
        self.logger = logger
        self.prefix = prefix

    def __getattr__(self, name):
        basemethod = getattr(self.logger, name)
        if not name in ['debug', 'info', 'warn', 'error', 'critical',
                        'exception']:
            raise AttributeError

        def method(msg, *args, **kwargs):
            return basemethod("%s %s" % (self.prefix, msg), *args, **kwargs)
        return method


def command_log(*args, **kwargs):
    def _log(f):
        # XXX see the implementation of @command for command_name
        @functools.wraps(f)
        def wrapper(self, params):
            self.logger.info("command %s %s" % (wrapper.command_name, params))
            f(self, params)
        return wrapper
    return lambda f: command(*args, **kwargs)(_log(f))


class CliHandler(TelnetHandler):
    PROMPT = 'ryu-manager %s> ' % version

    def __init__(self, request, client_address, server):
        self.client_address = client_address
        logger = logging.getLogger("ryu.app.Cli")
        plogger = PrefixedLogger(logger, "CLI %s" % (client_address,))
        self.logger = plogger  # for us
        self.logging = plogger  # for TelnetHandler
        TelnetHandler.__init__(self, request, client_address, server)

    def session_start(self):
        self.logger.info("%s session start", self.transport)

    def session_end(self):
        # XXX due to a bug in telnetsrv 0.4, this isn't called on
        # a forcible disconnect.
        # see https://github.com/yamt/telnetsrvlib/tree/fix-disconnect
        # for a fix.
        self.logger.info("%s session end", self.transport)

    @command_log('set-log-level')
    def command_set_log_level(self, params):
        '''<logger> <level>
        set log level of the specified logger
        '''
        try:
            name = params[0]
            newlvl = int(params[1])
        except (ValueError, IndexError):
            self.writeerror('invalid parameter')
            return
        try:
            oldlvl = management.get_log_level(name)
            management.set_log_level(name, newlvl)
        except LookupError:
            self.writeerror('logger %s is unknown' % (name,))
            return
        self.writeresponse('logger %s level %s -> %s' %
                           (name, oldlvl, newlvl))

    @command_log('show-bricks')
    def command_show_bricks(self, params):
        '''
        show a list of configured bricks
        '''
        map(lambda b: self.writeresponse('%s' % (b,)),
            management.list_bricks())

    @command_log('show-loggers')
    def command_show_loggers(self, params):
        '''
        show loggers
        '''
        map(lambda name: self.writeresponse('logger %s level %s' %
                                            (name,
                                            management.get_log_level(name))),
            management.list_loggers())

    @command_log('show-options')
    def command_show_options(self, params):
        '''
        show options
        '''
        class MyLogger:
            def log(mylogger_self, lvl, fmt, *args):
                self.writeresponse(fmt % args)
        CONF.log_opt_values(MyLogger(), None)


class CliTelnetHandler(CliHandler):
    transport = 'telnet'


class CliSSHHandler(CliHandler):
    transport = 'ssh'


class Cli(app_manager.RyuApp):
    def __init__(self, *args, **kwargs):
        super(Cli, self).__init__(*args, **kwargs)
        something_started = False
        if 'telnet' in CONF.cli_transports:
            self.logger.info("starting telnet server at %s:%d",
                             CONF.cli_telnet_host, CONF.cli_telnet_port)
            gevent.spawn(self.telnet_loop)
            something_started = True
        if 'ssh' in CONF.cli_transports:
            self.logger.info("starting ssh server at %s:%d",
                             CONF.cli_ssh_host, CONF.cli_ssh_port)
            gevent.spawn(self.ssh_loop)
            something_started = True
        if not something_started:
            self.logger.warn("cli app has no valid transport configured")
            self.logger.debug("cli-transports=%s", CONF.cli_transports)
            self.logger.debug("cli-ssh-hostkey=%s", CONF.cli_ssh_hostkey)

    def telnet_loop(self):
        server = gevent.server.StreamServer((CONF.cli_telnet_host,
                                            CONF.cli_telnet_port),
                                            CliTelnetHandler.
                                            streamserver_handle)
        server.serve_forever()

    def ssh_loop(self):
        from telnetsrv import paramiko_ssh

        class Wrapper(paramiko_ssh.SSHHandler):
            host_key = paramiko_ssh.getRsaKeyFile(CONF.cli_ssh_hostkey)
            telnet_handler = CliSSHHandler

            # NOTE: paramiko_ssh from telnetsrv 0.4 allows none auth
            # by default.
            def authCallbackUsername(self, username, password):
                raise RuntimeError('reject none auth')

            # paramiko.ServerInterface
            def get_allowed_auths(self, usename):
                return 'password'

            def authCallback(self, username, password):
                if username != CONF.cli_ssh_username or password != \
                        CONF.cli_ssh_password:
                    raise RuntimeError('auth fail')

        server = gevent.server.StreamServer((CONF.cli_ssh_host,
                                            CONF.cli_ssh_port),
                                            Wrapper.streamserver_handle)
        server.serve_forever()
