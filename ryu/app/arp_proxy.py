# Copyright (C) 2013,2014 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2013,2014 YAMAMOTO Takashi <yamamoto at valinux co jp>
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

# ARP responder
#
# intended to be used by OpenStack neutron agent.
#
# a user should set up flows so that we receive packet-ins for arp requests
# with appropriate metadata.
# eg.
#     ovs-ofctl -O openflow13 add-flow s0 priority=100,\
#         actions=write_metadata:999,goto_table:1
#     ovs-ofctl -O openflow13 add-flow s0 table=1,priority=1,arp,arp_op=1,\
#         actions=output:controller

from ryu.base import app_manager

from ryu.controller import ofp_event
from ryu.controller.handler import MAIN_DISPATCHER
from ryu.controller.handler import set_ev_cls

from ryu.ofproto import ofproto_v1_3

from ryu.lib import dpid as dpid_lib

from ryu.lib.packet import packet
from ryu.lib.packet import ethernet
from ryu.lib.packet import arp

import ofctl.api


class ArpProxy(app_manager.RyuApp):
    OFP_VERSIONS = [ofproto_v1_3.OFP_VERSION]

    def __init__(self, *args, **kwargs):
        super(ArpProxy, self).__init__(*args, **kwargs)
        # metadata -> ip_addr -> hw_addr
        self._table = {}
        self._table[999] = {
            '10.0.0.2': '0a:e4:1c:d1:3e:44',
        }

    @set_ev_cls(ofp_event.EventOFPPacketIn, MAIN_DISPATCHER)
    def _packet_in_handler(self, ev):
        msg = ev.msg
        datapath = msg.datapath
        metadata = msg.match.get('metadata')
        if metadata is None:
            return
        port = msg.match['in_port']
        pkt = packet.Packet(msg.data)
        self.logger.debug("packet-in dpid %s in_port %s metadata %s %s" %
                         (dpid_lib.dpid_to_str(datapath.id), port,
                          metadata, pkt,))
        fdb = self._table.get(metadata)
        if fdb is None:
            self.logger.debug("ignoring unknown metadata %s" % (metadata,))
            return
        pkt_ethernet = pkt.get_protocol(ethernet.ethernet)
        if not pkt_ethernet:
            self.logger.debug("ignoring non-ethernet packet")
            return
        pkt_arp = pkt.get_protocol(arp.arp)
        if not pkt_arp:
            self.logger.debug("ignoring non-arp packet")
            return
        if self._respond_arp(datapath, port, pkt_ethernet, pkt_arp, fdb):
            return
        # flood
        ofproto = datapath.ofproto
        parser = datapath.ofproto_parser
        actions = [parser.OFPActionOutput(port=ofproto.OFPP_FLOOD)]
        out = parser.OFPPacketOut(datapath=datapath,
                                  buffer_id=msg.buffer_id,
                                  in_port=port,
                                  actions=actions,
                                  data=msg.data)
        ofctl.api.send_msg(self, out)

    def _respond_arp(self, datapath, port, pkt_ethernet, pkt_arp, fdb):
        if pkt_arp.opcode != arp.ARP_REQUEST:
            self.logger.debug("flooding unknown arp op %s" % (pkt_arp.opcode,))
            return True
        ip_addr = pkt_arp.dst_ip
        hw_addr = fdb.get(ip_addr)
        if hw_addr is None:
            self.logger.debug("flooding arp request for %s" % (ip_addr,))
            return False
        self.logger.debug("responding arp request %s -> %s" %
                        (ip_addr, hw_addr,))
        pkt = packet.Packet()
        pkt.add_protocol(ethernet.ethernet(ethertype=pkt_ethernet.ethertype,
                                           dst=pkt_ethernet.src,
                                           src=hw_addr))
        pkt.add_protocol(arp.arp(opcode=arp.ARP_REPLY,
                                 src_mac=hw_addr,
                                 src_ip=ip_addr,
                                 dst_mac=pkt_arp.src_mac,
                                 dst_ip=pkt_arp.src_ip))
        self._send_packet(datapath, port, pkt)

    def _send_packet(self, datapath, port, pkt):
        ofproto = datapath.ofproto
        parser = datapath.ofproto_parser

        self.logger.debug("packet-out %s" % (pkt,))

        pkt.serialize()
        data = pkt.data
        actions = [parser.OFPActionOutput(port=port)]
        out = parser.OFPPacketOut(datapath=datapath,
                                  buffer_id=ofproto.OFP_NO_BUFFER,
                                  in_port=ofproto.OFPP_CONTROLLER,
                                  actions=actions,
                                  data=data)
        ofctl.api.send_msg(self, out)
