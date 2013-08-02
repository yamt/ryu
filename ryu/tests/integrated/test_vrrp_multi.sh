#! /bin/sh

VSCTL=${HOME}/ovs/bin/ovs-vsctl

# create two bridges.
# conncect them using patch ports.

create() {
    BR=$1
    LOCAL_PORT=patch$2
    PEER_PORT=patch$3
    CONT=$4
    ${VSCTL} add-br ${BR} -- set bridge ${BR} datapath_type=netdev
    ${VSCTL} add-port ${BR} ${LOCAL_PORT}
    ${VSCTL} set interface ${LOCAL_PORT} type=patch
    ${VSCTL} set interface ${LOCAL_PORT} options:peer=${PEER_PORT}
    ${VSCTL} set-controller ${BR} ${CONT}
}

CONT=tcp:127.0.0.1:6633

create s0 0 1 ${CONT}
create s1 1 0 ${CONT}
