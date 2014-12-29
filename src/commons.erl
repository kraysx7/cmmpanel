-module(commons).
-author("Ilya Troshkov").
-export([get_mac_str/1, get_mac_num/1]).


get_mac_str(MacNum) when is_integer(MacNum) ->
    string:join(
	lists:map(
	    fun(B) -> lists:flatten(io_lib:format("~2.16.0B",[B])) end,
	    lists:sublist(binary_to_list(<<MacNum:64>>),3,6)
	)
	,":");
get_mac_str(Unknown) -> Unknown.





%% Длинна `17` знаков, т.к. формат MAC=00:00:00:00:00:00
get_mac_num(MacStr) when is_binary(MacStr), byte_size(MacStr)=:=17 ->
    BinListMacNums = binary:split(MacStr, <<":">>, [global]),
    [BN1, BN2, BN3, BN4, BN5, BN6] = BinListMacNums,
    ListMacNums = [
		   list_to_integer(binary_to_list(BN1), 16),
		   list_to_integer(binary_to_list(BN2), 16),
		   list_to_integer(binary_to_list(BN3), 16),
		   list_to_integer(binary_to_list(BN4), 16),
		   list_to_integer(binary_to_list(BN5), 16),
		   list_to_integer(binary_to_list(BN6), 16)
		  ],
    [N1, N2, N3, N4, N5, N6] = ListMacNums,
    <<Mac:64>> = <<0:8, 0:8, N1:8, N2:8, N3:8, N4:8, N5:8, N6:8>>,
    io:format("mac: ~p ~n", [Mac]),
    Mac;

get_mac_num(Unknown) -> Unknown.


    
