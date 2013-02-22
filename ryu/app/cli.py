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


class CliHandler(TelnetHandler):
    PROMPT = 'ryu-manager %s> ' % version

    @command('show-bricks')
    def command_show_bricks(self, params):
        '''
        show a list of configured bricks
        '''
        from ryu.base.app_manager import SERVICE_BRICKS
        for b, x in SERVICE_BRICKS.iteritems():
            self.writeresponse('%s' % (b,))

    @command('show-options')
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
