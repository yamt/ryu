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


class TypeDescr(object):
    pass


class IntDescr(TypeDescr):
    def __init__(self, size):
        self.size = size

Int1 = IntDescr(1)
Int2 = IntDescr(2)
Int3 = IntDescr(3)
Int4 = IntDescr(4)
Int8 = IntDescr(8)


class MacAddr(TypeDescr):
    size = 6


class IPv4Addr(TypeDescr):
    size = 4


class IPv6Addr(TypeDescr):
    size = 16


class D(object):
    def __init__(self, name, num, type_):
        self.name = name
        self.num = num
        self.type = type_


oxm_types = [
    D('in_port', 0, Int4),
    D('in_phy_port', 1, Int4),
    D('metadata', 2, Int8),
    D('eth_dst', 3, MacAddr),
    D('eth_src', 4, MacAddr),
    D('eth_type', 5, Int2),
    D('vlan_vid', 6, Int2),
    D('vlan_pcp', 7, Int1),
    D('ip_dscp', 8, Int1),
    D('ip_ecn', 9, Int1),
    D('ip_proto', 10, Int1),
    D('ipv4_src', 11, IPv4Addr),
    D('ipv4_dst', 12, IPv4Addr),
    D('tcp_src', 13, Int2),
    D('tcp_dst', 14, Int2),
    D('udp_src', 15, Int2),
    D('udp_dst', 16, Int2),
    D('sctp_src', 17, Int2),
    D('sctp_dst', 18, Int2),
    D('icmpv4_type', 19, Int1),
    D('icmpv4_code', 20, Int1),
    D('arp_op', 21, Int2),
    D('arp_spa', 22, IPv4Addr),
    D('arp_tpa', 23, IPv4Addr),
    D('arp_sha', 24, MacAddr),
    D('arp_tha', 25, MacAddr),
    D('ipv6_src', 26, IPv6Addr),
    D('ipv6_dst', 27, IPv6Addr),
    D('ipv6_flabel', 28, Int4),
    D('icmpv6_type', 29, Int1),
    D('icmpv6_code', 30, Int1),
    D('ipv6_nd_target', 31, IPv6Addr),
    D('ipv6_nd_sll', 32, MacAddr),
    D('ipv6_nd_tll', 33, MacAddr),
    D('mpls_label', 34, Int4),
    D('mpls_tc', 35, Int1),
    D('mpls_bos', 36, Int1),
    D('pbb_isid', 37, Int3),
    D('tunnel_id', 38, Int8),
    D('ipv6_exthdr', 39, Int2),
]


def generate_constants(modname):
    import sys
    import string

    mod = sys.modules[modname]

    def add_attr(k, v):
        setattr(mod, k, v)

    for i in oxm_types:
        uk = string.upper(i.name)
        ofpxmt = i.num
        td = i.type
        add_attr('OFPXMT_OFB_' + uk, ofpxmt)
        add_attr('OXM_OF_' + uk, mod.oxm_tlv_header(ofpxmt, td.size))
        add_attr('OXM_OF_' + uk + '_W', mod.oxm_tlv_header_w(ofpxmt, td.size))
