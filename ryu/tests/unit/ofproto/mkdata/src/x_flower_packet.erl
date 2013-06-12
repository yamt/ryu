-module(x_flower_packet).
-export([message/3, message_extract/1, encode/1, decode/1]).

-include_lib("flower/include/flower_packet.hrl").

message(OFPVersion, Xid, Body) ->
    #ovs_msg{version=OFPVersion, xid=Xid, msg=Body}.

message_extract(Msg) ->
    #ovs_msg{version=OFPVersion, xid=Xid, msg=Body} = Msg,
    {OFPVersion, Xid, Body}.

guess_type(Msg) ->
    % eg. {ofp_packet_out, ...} -> packet_out
    RecType = element(1, Msg),
    case RecType of
        ofp_switch_features ->
            features_reply;
        _ ->
            RecTypeStr = atom_to_list(RecType),
            TypeStr = lists:nthtail(4, RecTypeStr),
            list_to_atom(TypeStr)
    end.

encode(Msg) ->
    Msg2 = case Msg of
        #ovs_msg{msg = features_request} ->
            Msg#ovs_msg{type = features_request, msg = <<>>};
        _ ->
            Msg#ovs_msg{type = guess_type(Msg#ovs_msg.msg)}
    end,
    %io:format("encoding ~p~n", [Msg2]),
    BinMsg = flower_packet:encode(Msg2),
    {ok, BinMsg}.

decode(BinMsg) ->
    {[Msg], <<>>} = flower_packet:decode(BinMsg),
    %io:format("decoded ~p~n", [Msg]),
    Msg2 = case Msg of
        #ovs_msg{type = features_request} ->
            Msg#ovs_msg{msg = features_request};
        _ ->
            Msg
    end,
    {ok, Msg2, <<>>}.
