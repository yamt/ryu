#!/usr/bin/env python
#
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

# a simple command line OF-CONFIG client
#
# a usage example:
#     % PYTHONPATH=. ./bin/of_config_cli \
#      --peers=sw1=localhost:1830:username:password
#     (Cmd) raw_get sw1

import ryu.contrib

from oslo.config import cfg

import cmd
import sys
import lxml.etree as ET

from ryu.lib import of_config
from ryu.lib.of_config import capable_switch
from ryu.lib.of_config import constants as consts
from ncclient.operations.rpc import RPCError
import ryu.lib.of_config.classes as ofc


CONF = cfg.CONF
CONF.register_cli_opts([
    cfg.ListOpt('peers', default=[], help='list of peers')
])


class Peer(capable_switch.OFCapableSwitch):
    def __init__(self, name, host, port, username, password):
        self._name = name
        super(Peer, self).__init__(
            host=host, port=port, username=username, password=password,
            unknown_host_cb=lambda host, fingeprint: True)


peers = {}


def add_peer(name, host, port, username, password):
    peers[name] = Peer(name, host, port, username, password)


def et_tostring_pp(tree):
    # pretty_print is an lxml feature, not available in ElementTree
    try:
        return ET.tostring(tree, pretty_print=True)
    except TypeError:
        return ET.tostring(tree)


def validate(tree):
    schema = ET.XMLSchema(file=of_config.OF_CONFIG_1_1_1_XSD)
    if not schema(tree):
        print schema.error_log


def _pythonify(name):
    # XXX code duplication
    return name.replace('-', '_')


class Cmd(cmd.Cmd):
    def __init__(self, *args, **kwargs):
        self._in_onecmd = False
        cmd.Cmd.__init__(self, *args, **kwargs)

    def _request(self, line, f):
        args = line.split()
        try:
            peer = args[0]
        except:
            print "argument error"
            return
        try:
            p = peers[peer]
        except KeyError:
            print "unknown peer", peer
            return
        try:
            f(p, args[1:])
        except RPCError, e:
            print "RPC Error", e
        except EOFError:
            print "disconnected"

    def _complete_peer(self, text, line, _begidx, _endidx):
        if len((line + 'x').split()) >= 3:
            return []
        return [name for name in peers if name.startswith(text)]

    def do_list_cap(self, line):
        """list_cap <peer>
        """

        def f(p, args):
            for i in p.netconf.server_capabilities:
                print i

        self._request(line, f)

    def do_raw_get(self, line):
        """raw_get <peer>
        """

        def f(p, args):
            result = p.get()
            tree = ET.fromstring(result)
            validate(tree)
            print et_tostring_pp(tree)

        self._request(line, f)

    def do_raw_get_config(self, line):
        """raw_get_config <peer> <source>
        """

        def f(p, args):
            try:
                source = args[0]
            except:
                print "argument error"
                return
            result = p.get_config(source)
            tree = ET.fromstring(result)
            validate(tree)
            print et_tostring_pp(tree)

        self._request(line, f)

    def do_list_port(self, line):
        """list_port <peer>
        """

        def f(p, args):
            result = p.get()
            o = ofc.OFCapableSwitchType.from_xml(result)
            for p in o.resources.port:
                print p.resource_id, p.name, p.number

        self._request(line, f)

    _port_settings = [
        'admin-state',
        'no-forward',
        'no-packet-in',
        'no-receive',
    ]

    def do_get_port_config(self, line):
        """get_config_port <peer> <source> <port>
        eg. get_port_config sw1 running LogicalSwitch7-Port2
        """

        def f(p, args):
            try:
                source, port = args
            except:
                print "argument error"
                return
            result = p.get_config(source)
            o = ofc.OFCapableSwitchType.from_xml(result)
            for p in o.resources.port:
                if p.resource_id != port:
                    continue
                print p.resource_id
                conf = p.configuration
                for k in self._port_settings:
                    try:
                        print k, getattr(conf, _pythonify(k))
                    except AttributeError:
                        pass

        self._request(line, f)

    def do_set_port_config(self, line):
        """set_port_config <peer> <target> <port> <key> <value>
        eg. set_port_config sw1 running LogicalSwitch7-Port2 admin-state down
        eg. set_port_config sw1 running LogicalSwitch7-Port2 no-forward false
        """

        def f(p, args):
            try:
                target, port, key, value = args
            except:
                print "argument error"
                print args
                return

            # get switch id
            result = p.get()
            o = ofc.OFCapableSwitchType.from_xml(result)
            capable_switch_id = o.id

            conf = ofc.NETCONF_Config(
                capable_switch=ofc.OFCapableSwitchType(
                    id=capable_switch_id,
                    resources=ofc.OFCapableSwitchResourceListType(
                        port=ofc.OFPortType(
                            resource_id=port,
                            configuration=ofc.OFPortConfigurationType(
                                **{_pythonify(key): value}
                            )
                        )
                    )
                )
            )
            try:
                xml = conf.to_xml()
                p.edit_config(target, xml)
            except Exception, e:
                print e

        self._request(line, f)

    def complete_raw_get(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def complete_raw_get_config(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def complete_list_cap(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def complete_list_port(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def complete_get_port_config(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def complete_set_port_config(self, text, line, begidx, endidx):
        return self._complete_peer(text, line, begidx, endidx)

    def do_EOF(self, _line):
        sys.exit(0)

    def onecmd(self, string):
        self._in_onecmd = True
        try:
            return cmd.Cmd.onecmd(self, string)
        finally:
            self._in_onecmd = False


def main():
    CONF(project='of-config-cli', version='of-config-cli')

    for p_str in CONF.peers:
        name, addr = p_str.split('=')
        host, port, username, password = addr.rsplit(':', 3)
        add_peer(name, host, port, username, password)

    Cmd().cmdloop()


if __name__ == "__main__":
    main()
