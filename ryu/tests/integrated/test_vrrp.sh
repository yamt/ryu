#! /bin/sh

# a script to set up environment for test_vrrp.py

ip netns add vrrp-dump
ip link add veth-ovs type veth peer name veth-dump
ip link set netns vrrp-dump veth-dump
ip netns exec vrrp-dump tshark -i veth-dump
ovs-vsctl add-br s0
ovs-vsctl add-port s0 veth-ovs
ovs-vsctl set bridge s0 protocols='[OpenFlow12]'
ovs-vsctl set-controller s0 tcp:127.0.0.1:6633
ip link set veth-ovs up
ip link set veth-dump up
