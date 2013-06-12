-module(x).
-export([do/2, x/0]).

do(skip, {OFPVersion, N}) ->
    {OFPVersion, N + 1};
do(Body, {OFPVersion, N}) ->
    Mod = case OFPVersion of
        1 -> x_flower_packet;
        _ -> x_of_protocol
    end,
    Name = case Body of
        B when is_tuple(B) ->
            atom_to_list(element(1, B));
        _ ->
            atom_to_list(Body)
    end,
    io:format("processing ~B ~B ~s~n", [OFPVersion, N, Name]),
    Msg = Mod:message(OFPVersion, 0, Body),
    case Mod:encode(Msg) of
        {ok, BinMsg} -> ok;
        {error, Error} -> io:format("~p ~p~n", [Error, Msg]), BinMsg = hoge
    end,
    {ok, F} = file:open(["../data/", integer_to_list(OFPVersion), "-",
        integer_to_list(N), "-", Name, ".packet"], [write, binary]),

    % sanity check
    % this is fragile because of order of flags.
    % ofp flags are unorderd but of_protocol keeps them in a list.
    {ok, Msg2, <<>>} = Mod:decode(BinMsg),
    {OFPVersion, 0, Body2} = Mod:message_extract(Msg2),
    case Body == Body2 of
        false -> io:format("~p~n", [Body]), io:format("~p~n", [Body2]);
        _ -> hoge
    end,
    Body = Body2,

    ok = file:write(F, BinMsg),
    ok = file:close(F),
    {OFPVersion, N + 1}.

x() ->
    lists:map(fun(Mod) -> Mod:x() end, [x1, x3, x4]).
