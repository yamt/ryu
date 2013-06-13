-module(x).
-compile(export_all).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v3.hrl").

do(Body, N) ->
    OFPVersion = 3,
    Name = atom_to_list(element(1, Body)),
    io:format("processing ~B ~B ~s~n", [OFPVersion, N, Name]),
    Msg = ofp_v3_encode:do(#ofp_message{version=OFPVersion, xid=0, body=Body}),
    {ok, F} = file:open(["../data/", integer_to_list(OFPVersion), "-",
        integer_to_list(N), "-", Name, ".packet"], [write, binary]),
    ok = file:write(F, Msg),
    ok = file:close(F),
    N + 1.

x() ->
    lists:foldl(fun do/2, 0, [
        #ofp_desc_stats_reply{flags = [], mfr_desc = <<"mfr">>,
                              hw_desc = <<"hw">>, sw_desc = <<"sw">>,
                              serial_num = <<"serial">>,
                              dp_desc = <<"dp">>},
        #ofp_packet_out{
            buffer_id = no_buffer,in_port = controller,
            actions = 
                [#ofp_action_output{seq = 14,port = all,max_len = 65535}],
            data = 
                <<242,11,164,208,63,112,242,11,164,125,248,234,8,0,69,0,
                  0,84,248,26,0,0,255,1,175,139,10,0,0,1,10,0,0,2,8,0,2,
                  8,247,96,0,0,49,214,2,0,0,0,0,0,171,141,45,49,0,0,0,0,
                  16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                  34,35,36,37,38,39,40,41,42,43,44,45,46,47,0,0,0,0,0,0,
                  0,0>>},
        #ofp_flow_mod{
            cookie = <<0,0,0,0,0,0,0,0>>,
            cookie_mask = <<0,0,0,0,0,0,0,0>>,
            table_id = 1,command = add,idle_timeout = 0,
            hard_timeout = 0,priority = 123,buffer_id = 65535,
            out_port = any,out_group = any,flags = [],
            match =
                #ofp_match{
                    fields =
                        [#ofp_field{
                             class = openflow_basic,name = eth_dst,
                             has_mask = false,
                             value = <<"\362\v\244}\370\352">>,
                             mask = undefined}]},
            instructions =
                [#ofp_instruction_write_actions{
                     seq = 3,
                     actions =
                         [#ofp_action_output{
                              seq = 14,port = 6,max_len = 65535}]}]},
        #ofp_flow_mod{
            cookie = <<0,0,0,0,0,0,0,0>>,
            cookie_mask = <<0,0,0,0,0,0,0,0>>,
            table_id = 0,command = add,idle_timeout = 0,
            hard_timeout = 0,priority = 123,buffer_id = 65535,
            out_port = any,out_group = any,flags = [],
            match =
                #ofp_match{
                    fields =
                        [#ofp_field{
                             class = openflow_basic,name = in_port,
                             has_mask = false,
                             value = <<0,0,0,6>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = eth_src,
                             has_mask = false,
                             value = <<"\362\v\244}\370\352">>,
                             mask = undefined}]},
            instructions =
                [#ofp_instruction_goto_table{seq = 5,table_id = 1}]}
    ]).
