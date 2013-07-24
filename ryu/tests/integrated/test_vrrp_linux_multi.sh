#! /bin/sh

ip link add veth0 type veth peer name veth0-br
ip link add veth1 type veth peer name veth1-br
ip link add veth2 type veth peer name veth2-br

brctl addbr vrrpbr
brctl addif vrrpbr veth0-br
brctl addif vrrpbr veth1-br
brctl addif vrrpbr veth2-br

ip link set veth0 up
ip link set veth0-br up
ip link set veth1 up
ip link set veth1-br up
ip link set veth2 up
ip link set veth2-br up
ip link set vrrpbr up
