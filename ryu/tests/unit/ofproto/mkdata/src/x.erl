-module(x).
-export([do/2, x/0]).

-include_lib("of_protocol/include/of_protocol.hrl").

do(skip, {OFPVersion, N}) ->
    {OFPVersion, N + 1};
do(Body, {OFPVersion, N}) ->
    Name = atom_to_list(element(1, Body)),
    io:format("processing ~B ~B ~s~n", [OFPVersion, N, Name]),
    Msg = #ofp_message{version=OFPVersion, xid=0, body=Body},
    case of_protocol:encode(Msg) of
	{ok, BinMsg} -> ok;
	{error, Error} -> io:format("~p ~p~n", [Error, Msg]), BinMsg = hoge
    end,
    {ok, F} = file:open(["../data/", integer_to_list(OFPVersion), "-",
        integer_to_list(N), "-", Name, ".packet"], [write, binary]),

    % sanity check
    % this is fragile because of order of flags.
    % ofp flags are unorderd but of_protocol keeps them in a list.
    {ok, Msg2, <<>>} = of_protocol:decode(BinMsg),
    #ofp_message{version=OFPVersion, type=_, xid=0, body=Body2} = Msg2,
    case Body == Body2 of
        false -> io:format("~p~n", [Body]), io:format("~p~n", [Body2]);
        _ -> hoge
    end,
    Body = Body2,

    ok = file:write(F, BinMsg),
    ok = file:close(F),
    {OFPVersion, N + 1}.

x() ->
    lists:map(fun(Mod) -> Mod:x() end, [x3, x4]).
