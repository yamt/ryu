-module(x_of_protocol).
-export([message/3, message_extract/1, encode/1, decode/1]).

-include_lib("of_protocol/include/of_protocol.hrl").

message(OFPVersion, Xid, Body) ->
    #ofp_message{version=OFPVersion, xid=Xid, body=Body}.

message_extract(Msg) ->
    #ofp_message{version=OFPVersion, xid=Xid, body=Body} = Msg,
    {OFPVersion, Xid, Body}.

encode(Msg) ->
    of_protocol:encode(Msg).

decode(BinMsg) ->
    of_protocol:decode(BinMsg).
