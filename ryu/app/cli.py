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

from oslo.config import cfg

from telnetsrv.green import command
from telnetsrv.green import TelnetHandler

from ryu import version
from ryu.base import app_manager


CONF = cfg.CONF
CONF.register_opts([
    cfg.StrOpt('cli_host', default='localhost', help='cli listen host'),
    cfg.IntOpt('cli_port', default=4989, help='cli listen port')
])


class PrefixedLogger(object):
    def __init__(self, logger, prefix):
        self.logger = logger
        self.prefix = prefix

    def __getattr__(self, name):
        basemethod = getattr(self.logger, name)
        if not name in ['debug', 'info', 'warn', 'error', 'critical']:
            raise AttributeError

        def method(msg, *args, **kwargs):
            return basemethod("%s %s" % (self.prefix, msg), *args, **kwargs)
        return method


def command_log(*args, **kwargs):
    def _log(f):
        # XXX see the implementation of @command for command_name and __doc
        def wrapper(self, params):
            self.logger.info("command %s %s" % (wrapper.command_name, params))
            f(self, params)
        wrapper.__doc__ = f.__doc__
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
        self.logger.info("session start")

    def session_end(self):
        # XXX due to a bug in telnetsrv 0.4, this isn't called on
        # a forcible disconnect.
        # see https://github.com/yamt/telnetsrvlib/tree/fix-disconnect
        # for a fix.
        self.logger.info("session end")

    @command_log('set-log-level')
    def command_set_log_level(self, params):
        '''<logger> <level>
        set log level of the specified logger
        '''
        import ryu.logger
        try:
            name = params[0]
            newlvl = int(params[1])
        except (ValueError, IndexError):
            self.writeerror('invalid parameter')
            return
        if not name in ryu.logger.RyuLogger.loggers:
            self.writeerror('logger %s is unknown' % (name,))
            return
        logger = logging.getLogger(name)
        oldlvl = logger.getEffectiveLevel()
        logger.setLevel(newlvl)
        self.writeresponse('logger %s level %s -> %s' %
                           (name, oldlvl, newlvl))

    @command_log('show-bricks')
    def command_show_bricks(self, params):
        '''
        show a list of configured bricks
        '''
        from ryu.base.app_manager import SERVICE_BRICKS
        for b, x in SERVICE_BRICKS.iteritems():
            self.writeresponse('%s' % (b,))

    @command_log('show-loggers')
    def command_show_loggers(self, params):
        '''
        show loggers
        '''
        import ryu.logger

        def show_logger(name):
            logger = logging.getLogger(name)
            self.writeresponse('logger %s level %s' %
                               (name, logger.getEffectiveLevel()))
        map(show_logger, ryu.logger.RyuLogger.loggers)

    @command_log('show-options')
    def command_show_options(self, params):
        '''
        show options
        '''
        class MyLogger:
            def log(mylogger_self, lvl, fmt, *args):
                self.writeresponse(fmt % args)
        CONF.log_opt_values(MyLogger(), None)


class Cli(app_manager.RyuApp):
    def __init__(self, *args, **kwargs):
        super(Cli, self).__init__(*args, **kwargs)
        gevent.spawn(self.loop)

    def loop(self):
        server = gevent.server.StreamServer((CONF.cli_host, CONF.cli_port),
                                            CliHandler.streamserver_handle)
        server.serve_forever()
