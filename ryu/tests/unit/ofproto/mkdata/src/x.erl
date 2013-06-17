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
                [#ofp_instruction_goto_table{seq = 5,table_id = 1}]},
        #ofp_packet_in{
            buffer_id = 2,reason = action,table_id = 1,
            match =
                #ofp_match{
                    fields =
                        [#ofp_field{
                             class = openflow_basic,name = in_port,
                             has_mask = false,
                             value = <<0,0,0,6>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = eth_type,
                             has_mask = false,
                             value = <<8,6>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = eth_dst,
                             has_mask = false,value = <<"\377\377\377\377\377\377">>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = eth_src,
                             has_mask = false,value = <<"\362\v\244}\370\352">>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = arp_op,
                             has_mask = false,
                             value = <<0,1>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = arp_spa,
                             has_mask = false,
                             value = <<10,0,0,1>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = arp_tpa,
                             has_mask = false,
                             value = <<10,0,0,3>>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = arp_sha,
                             has_mask = false,value = <<"\362\v\244}\370\352">>,
                             mask = undefined},
                         #ofp_field{
                             class = openflow_basic,name = arp_tha,
                             has_mask = false,
                             value = <<0,0,0,0,0,0>>,
                             mask = undefined}]},
            data =
                <<255,255,255,255,255,255,242,11,164,125,248,234,8,6,0,
                  1,8,0,6,4,0,1,242,11,164,125,248,234,10,0,0,1,0,0,0,0,
                  0,0,10,0,0,3>>},
        #ofp_features_request{},
        #ofp_features_reply{
            datapath_mac = <<8,96,110,127,116,231>>,
            datapath_id = 0,n_buffers = 0,n_tables = 255,
            capabilities = 
                [flow_stats,table_stats,port_stats,group_stats,queue_stats],
            ports = 
                [#ofp_port{
                     port_no = 7,hw_addr = <<"\362\v\244\320?p">>,
                     name = <<80,111,114,116,7>>,
                     config = [],
                     state = [live],
                     curr = ['100mb_fd',copper,autoneg],
                     advertised = [copper,autoneg],
                     supported = ['100mb_fd',copper,autoneg],
                     peer = ['100mb_fd',copper,autoneg],
                     curr_speed = 5000,max_speed = 5000},
                 #ofp_port{
                     port_no = 6,hw_addr = <<"\362\v\244}\370\352">>,
                     name = <<80,111,114,116,6>>,
                     config = [],
                     state = [live],
                     curr = ['100mb_fd',copper,autoneg],
                     advertised = [copper,autoneg],
                     supported = ['100mb_fd',copper,autoneg],
                     peer = ['100mb_fd',copper,autoneg],
                     curr_speed = 5000,max_speed = 5000}]},
        #ofp_set_config{flags = [],miss_send_len = 128},
        #ofp_get_config_request{},
        #ofp_get_config_reply{flags = [],miss_send_len = 128},
        #ofp_hello{},
        #ofp_flow_stats_request{
            flags = [],table_id = 0,out_port = any,out_group = any,
            cookie = <<0,0,0,0,0,0,0,0>>,
            cookie_mask = <<0,0,0,0,0,0,0,0>>,
            match = #ofp_match{fields = []}},
        #ofp_flow_stats_reply{
            flags = [],
            stats =
                [#ofp_flow_stats{
                     table_id = 0,duration_sec = 358,
                     duration_nsec = 358115277000,priority = 65535,
                     idle_timeout = 0,hard_timeout = 0,
                     cookie = <<0,0,0,0,0,0,0,0>>,
                     packet_count = 0,byte_count = 0,
                     match = #ofp_match{fields = []},
                     instructions = []},
                 #ofp_flow_stats{
                     table_id = 0,duration_sec = 358,
                     duration_nsec = 358115055000,priority = 65534,
                     idle_timeout = 0,hard_timeout = 0,
                     cookie = <<0,0,0,0,0,0,0,0>>,
                     packet_count = 0,byte_count = 0,
                     match =
                         #ofp_match{
                             fields =
                                 [#ofp_field{
                                      class = openflow_basic,name = eth_type,
                                      has_mask = false,
                                      value = <<8,6>>,
                                      mask = undefined}]},
                     instructions =
                         [#ofp_instruction_apply_actions{
                              seq = 1,
                              actions =
                                  [#ofp_action_output{
                                       seq = 14,port = normal,max_len = 0}]}]},
                 #ofp_flow_stats{
                     table_id = 0,duration_sec = 316220,
                     duration_nsec = 316220511582000,priority = 123,
                     idle_timeout = 0,hard_timeout = 0,
                     cookie = <<0,0,0,0,0,0,0,0>>,
                     packet_count = 3,byte_count = 238,
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
                         [#ofp_instruction_goto_table{seq = 5,table_id = 1}]},
                 #ofp_flow_stats{
                     table_id = 0,duration_sec = 313499,
                     duration_nsec = 313499980901000,priority = 0,
                     idle_timeout = 0,hard_timeout = 0,
                     cookie = <<0,0,0,0,0,0,0,0>>,
                     packet_count = 1,byte_count = 98,
                     match = #ofp_match{fields = []},
                     instructions =
                         [#ofp_instruction_write_actions{
                              seq = 3,
                              actions =
                                  [#ofp_action_output{
                                       seq = 14,port = controller,
                                       max_len = 65535}]}]}]},
        #ofp_echo_request{
            data = <<"hoge">>
        },
        #ofp_echo_reply{
            data = <<"hoge">>
        },
        #ofp_error_msg{
            type = bad_action,
            code = unsupported_order,
            data = <<"fugafuga">>
        },
        #ofp_experimenter{
            experimenter = 98765432,
            exp_type = 123456789,
            data = <<"nazo">>
        },
        #ofp_barrier_request{},
        #ofp_barrier_reply{},
        #ofp_role_request{
            role = master,
            generation_id = 16#f000f000f000f000},
        #ofp_role_reply{
            role = slave,
            generation_id = 16#f000f000f000f000},

        #ofp_group_mod{
            command = add,type = all,group_id = 1,
            buckets = 
                [#ofp_bucket{
                     weight = 1,watch_port = 1,watch_group = 1,
                     actions = 
                         [#ofp_action_output{
                              seq = 14,port = 2,max_len = 65535}]}]},
        #ofp_port_mod{port_no = 1, hw_addr = <<0,17,0,0,17,17>>,
            config = [],mask = [], advertise = [fiber]},
        #ofp_table_mod{table_id = all,config = controller},
        #ofp_desc_stats_request{},
        #ofp_aggregate_stats_request{
            flags = [],table_id = all,out_port = any,out_group = any,
            cookie = <<0,0,0,0,0,0,0,0>>,
            cookie_mask = <<0,0,0,0,0,0,0,0>>,
            match = #ofp_match{fields = []}},
        #ofp_aggregate_stats_reply{flags = [],packet_count = 7,
                                   byte_count = 574,flow_count = 6},
        #ofp_table_stats_request{}
    ]).
