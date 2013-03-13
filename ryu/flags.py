# Copyright (C) 2011 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2011 Isaku Yamahata <yamahata at valinux co jp>
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
"""
global flags
"""

from oslo.config import cfg

CONF = cfg.CONF

quantum_group = cfg.OptGroup(name='quantum',
                             title='Openstack Quantum related options')
CONF.register_group(quantum_group)
CONF.register_cli_opts([
    # app/quantum_adapter
    cfg.StrOpt('url', default='http://localhost:9696',
               help='URL for connecting to quantum'),
    cfg.IntOpt('url-timeout', default=30,
               help='timeout value for connecting to quantum in seconds'),
    cfg.StrOpt('admin-username', default='quantum',
               help='username for connecting to quantum in admin context'),
    cfg.StrOpt('admin-password', default='service_password',
               help='password for connecting to quantum in admin context'),
    cfg.StrOpt('admin-tenant-name', default='service',
               help='tenant name for connecting to quantum in admin context'),
    cfg.StrOpt('admin-auth-url', default='http://localhost:5000/v2.0',
               help='auth url for connecting to quantum in admin context'),
    cfg.StrOpt('auth-strategy', default='keystone',
               help='auth strategy for connecting to quantum in admin'
               'context'),
    cfg.StrOpt('controller-addr', default=None,
               help='openflow method:address:port to set controller of'
               'ovs bridge')
], group=quantum_group)
