# Copyright (C) 2013 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2013 Isaku Yamahata <yamahata at valinux co jp>
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
Usage:
PYTHONPATH=. ./bin/ryu-manager --verbose \
             ./ryu/services/vrrp/manager.py \
             ./ryu/services/vrrp/dumper.py \
             ./ryu/services/vrrp/sample_manager.py \
             ./ryu/tests/integrated/test_vrrp_linux_multi.py

./ryu/services/vrrp/dumper.py is optional.

                    ----------------
      /--<--veth0-->|              |
   Ryu              | linux bridge |<--veth2--> command to generate packets
      \--<--veth1-->|   (vrrpbr)   |
                    ----------------


# ip link add veth0 type veth peer name veth0-br
# ip link add veth1 type veth peer name veth1-br
# ip link add veth2 type veth peer name veth2-br

# brctl addbr vrrpbr
# brctl addif vrrpbr veth0-br
# brctl addif vrrpbr veth1-br
# brctl addif vrrpbr veth2-br


# ip link set veth0 up
# ip link set veth0-br up
# ip link set veth1 up
# ip link set veth1-br up
# ip link set veth2 up
# ip link set veth2-br up
# ip link set vrrpbr up

if you like, capture packets on each interfaces like
# tshark -i vrrpbr
# tshark -i veth0
# tshark -i veth1
# tshark -i veth2

virtual router mac address: 00:00:5E:00:01:{VRID} = 00:00:5E:00:01:07
during working, send packets destined to mac address 00:00:5E:00:01:07
from veth2 by packet generator like packeth

NOTE: vrid: 7 and ip address: 10.0.0.1... are hardcoded below
"""

import time

from ryu.base import app_manager
from ryu.lib import hub
from ryu.lib import mac as lib_mac
from ryu.lib import addrconv
from ryu.lib.packet import vrrp
from ryu.services.vrrp import api as vrrp_api
from ryu.services.vrrp import event as vrrp_event


_VRID = 7

_IFNAME0 = 'veth0'
_PRIMARY_IP_ADDRESS0 = addrconv.ipv4.text_to_bin('10.0.0.2')

_IFNAME1 = 'veth1'
_PRIMARY_IP_ADDRESS1 = addrconv.ipv4.text_to_bin('10.0.0.3')


class VRRPConfigApp(app_manager.RyuApp):
    def __init__(self, *args, **kwargs):
        super(VRRPConfigApp, self).__init__(*args, **kwargs)
        self.logger.info(
            'virtual router mac address = %s',
            addrconv.mac.bin_to_text(vrrp.vrrp_ipv4_src_mac_address(_VRID)))

    def start(self):
        hub.spawn(self._main)

    def _main(self):
        self._main_version(vrrp.VRRP_VERSION_V3)
        self._main_version(vrrp.VRRP_VERSION_V2)
        print "done!"

    def _main_version(self, vrrp_version):
        self._main_version_priority(vrrp_version,
                                    vrrp.VRRP_PRIORITY_ADDRESS_OWNER)
        self._main_version_priority(vrrp_version,
                                    vrrp.VRRP_PRIORITY_BACKUP_MAX)
        self._main_version_priority(vrrp_version,
                                    vrrp.VRRP_PRIORITY_BACKUP_DEFAULT)
        self._main_version_priority(vrrp_version,
                                    vrrp.VRRP_PRIORITY_BACKUP_MIN)

    def _main_version_priority(self, vrrp_version, priority):
        self._main_version_priority_sleep(vrrp_version, priority, False)
        self._main_version_priority_sleep(vrrp_version, priority, True)

    def _configure_vrrp_router(self, vrrp_version, priority,
                               primary_ip_address, ifname, vrid=_VRID):
        interface = vrrp_event.VRRPInterfaceNetworkDevice(
            lib_mac.DONTCARE, primary_ip_address, None, ifname)
        self.logger.debug('%s', interface)

        vip = addrconv.ipv4.text_to_bin('10.0.%d.1' % vrid)
        ip_addresses = [vip]
        config = vrrp_event.VRRPConfig(
            version=vrrp_version, vrid=vrid, priority=priority,
            ip_addresses=ip_addresses)
        self.logger.debug('%s', config)

        rep = vrrp_api.vrrp_config(self, interface, config)
        self.logger.debug('%s', rep)

        return rep

    def _main_version_priority_sleep(self, vrrp_version, priority, do_sleep):
        app_mgr = app_manager.AppManager.get_instance()
        self.logger.debug('%s', app_mgr.applications)
        vrrp_mgr = app_mgr.applications['VRRPManager']

        step = 5
        instances = {}
        for vrid in xrange(1, 256, step):
            if vrid == _VRID:
                continue
            print "vrid", vrid
            l = {}
            prio = max(vrrp.VRRP_PRIORITY_BACKUP_MIN,
                   min(vrrp.VRRP_PRIORITY_BACKUP_MAX, vrid))
            rep0 = self._configure_vrrp_router(vrrp_version,
                                               prio,
                                               _PRIMARY_IP_ADDRESS0, _IFNAME0,
                                               vrid)
            assert not rep0.instance_name is None
            l[0] = rep0
            prio = max(vrrp.VRRP_PRIORITY_BACKUP_MIN,
                   min(vrrp.VRRP_PRIORITY_BACKUP_MAX, 256 - vrid))
            rep1 = self._configure_vrrp_router(vrrp_version,
                                               prio,
                                               _PRIMARY_IP_ADDRESS1, _IFNAME1,
                                               vrid)
            assert not rep1.instance_name is None
            l[1] = rep1
            instances[vrid] = l

        print "vrid", _VRID
        l = {}
        rep0 = self._configure_vrrp_router(vrrp_version, priority,
                                           _PRIMARY_IP_ADDRESS0, _IFNAME0)
        assert not rep0.instance_name is None
        l[0] = rep0
        rep1 = self._configure_vrrp_router(
            vrrp_version, vrrp.VRRP_PRIORITY_BACKUP_DEFAULT,
            _PRIMARY_IP_ADDRESS1, _IFNAME1)
        assert not rep1.instance_name is None
        l[1] = rep1
        instances[_VRID] = l

        self.logger.debug('%s', vrrp_mgr._instances)

        if do_sleep:
            print "priority", priority
            print "waiting for instances starting"

            while True:
                while True:
                    rep = vrrp_api.vrrp_list(self)
                    if len(rep.instance_list) >= len(instances) * 2:
                        if any(i.state == vrrp_event.VRRP_STATE_INITIALIZE
                            for i in rep.instance_list):
                            continue
                        break
                    print len(rep.instance_list), '/', len(instances) * 2
                    time.sleep(1)

#                for i in rep.instance_list:
#                    print i.instance_name, i.monitor_name, i.config, \
#                          i.interface, i.state
                assert len(rep.instance_list) == len(instances) * 2
                num_of_master = 0
                d = dict(((i.instance_name, i) for i in rep.instance_list))
                bad = 0
                for i in rep.instance_list:
                    assert i.state in (vrrp_event.VRRP_STATE_MASTER,
                        vrrp_event.VRRP_STATE_BACKUP)
                    if i.state == vrrp_event.VRRP_STATE_MASTER:
                        num_of_master += 1

                    vr = instances[i.config.vrid]
                    if (vr[0].config.priority > vr[1].config.priority and \
                        i.instance_name == vr[1].instance_name) or \
                       (vr[0].config.priority < vr[1].config.priority and \
                        i.instance_name == vr[0].instance_name):
                            if i.state == vrrp_event.VRRP_STATE_MASTER:
                                print "bad master:"
                                print d[vr[0].instance_name].state, \
                                      d[vr[0].instance_name].config.priority
                                print d[vr[1].instance_name].state, \
                                      d[vr[1].instance_name].config.priority
                                bad += 1
#                            assert i.state != vrrp_event.VRRP_STATE_MASTER
                if bad > 0:
                    # this could be a transient state
                    print bad, "bad masters"
                    time.sleep(1)
                    continue
                if num_of_master >= len(instances):
                    assert num_of_master == len(instances)
                    break
                print num_of_master, '/', len(instances)
                time.sleep(1)
                continue

        for vrid in instances.keys():
            if vrid == _VRID:
                continue
            which = vrid & 1
            vrrp_api.vrrp_shutdown(self, instances[vrid][which].instance_name)
        vrrp_api.vrrp_shutdown(self, instances[_VRID][0].instance_name)

        if do_sleep:
            print "shutting down an instance"
            while True:
                rep = vrrp_api.vrrp_list(self)
                if len(rep.instance_list) <= len(instances):
                    break
                print "left", len(rep.instance_list)
                time.sleep(1)
            assert len(rep.instance_list) == len(instances)
            print "waiting for the rest becoming master"
            while True:
                rep = vrrp_api.vrrp_list(self)
                if all(i.state == vrrp_event.VRRP_STATE_MASTER
                    for i in rep.instance_list):
                    break
                time.sleep(1)

        vrrp_api.vrrp_shutdown(self, instances[_VRID][1].instance_name)
        for vrid in instances.keys():
            if vrid == _VRID:
                continue
            which = 1 - (vrid & 1)
            vrrp_api.vrrp_shutdown(self, instances[vrid][which].instance_name)

        print "waiting for instances shutting down"
        while True:
            rep = vrrp_api.vrrp_list(self)
            if not rep.instance_list:
                break
            print "left", len(rep.instance_list)
            time.sleep(1)
