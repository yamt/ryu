-module(x_flower_packet).
-export([message/3, message_extract/1, encode/1, decode/1]).

-include_lib("flower/include/flower_packet.hrl").

message(OFPVersion, Xid, Body) ->
    #ovs_msg{version=OFPVersion, xid=Xid, msg=Body}.

message_extract(Msg) ->
    #ovs_msg{version=OFPVersion, xid=Xid, msg=Body} = Msg,
    {OFPVersion, Xid, Body}.

guess_type(Msg) ->
    % eg. ofp_packet_out -> packet_out
    RecType = element(1, Msg),
    RecTypeStr = atom_to_list(RecType),
    TypeStr = lists:nthtail(4, RecTypeStr),
    list_to_atom(TypeStr).

encode(Msg) ->
    Msg2 = Msg#ovs_msg{type = guess_type(Msg#ovs_msg.msg)},
    BinMsg = flower_packet:encode(Msg2),
    {ok, BinMsg}.

decode(BinMsg) ->
    {[Msg], <<>>} = flower_packet:decode(BinMsg),
    {ok, Msg, <<>>}.
