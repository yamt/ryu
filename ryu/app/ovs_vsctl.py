# Copyright (C) 2012 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2012 Isaku Yamahata <yamahata at private email ne jp>
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


from ryu.lib import hub
from oslo.config import cfg
import logging
import sys

from ryu.base import app_manager
from ryu.lib.ovs import vsctl as lib_vsctl


CONF = cfg.CONF
ovs_vsctl_group = cfg.OptGroup(name='vsctl',
                               title='ovs_vsctl.py related options')
CONF.register_group(ovs_vsctl_group)
CONF.register_opts([
    cfg.StrOpt('remote', default='tcp:127.0.0.1:6634', help='remote'),
    cfg.StrOpt('command', default='list-br', help='command'),
    cfg.MultiStrOpt('args', default=[], help='args')
], group=ovs_vsctl_group)

LOG = logging.getLogger(__name__)


class OVSVSCtl(app_manager.RyuApp):
    _CONTEXTS = {}

    def __init__(self, *_args, **_kwargs):
        super(OVSVSCtl, self).__init__()
        hub.spawn(self.run)

    def run(self):
        remote = CONF.vsctl.remote
        command = CONF.vsctl.command
        args = CONF.vsctl.args
        LOG.info('remote %s command %s args %s', remote, command, args)
        vsctl = lib_vsctl.VSCtl(remote)
        LOG.info('calling run_command')
        vsctl_command = lib_vsctl.VSCtlCommand(command, args)
        vsctl.run_command(commands=[vsctl_command])
        LOG.info('run_command result %s', vsctl_command.result)
