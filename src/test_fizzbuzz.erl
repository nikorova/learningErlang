-module(test_fizzbuzz).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
	{setup, 
		fun() -> fizzbuzz:start() end,
		fun(Pid) -> fizzbuzz:stop(Pid) end,
		fun generate_analyze_tests/1}.

generate_analyze_tests(Pid) ->
	[?_assertEqual(1, fizzbuzz:calc(Pid, 1)),
	?_assertEqual(fizz, fizzbuzz:calc(Pid, 3)),
	?_assertEqual(buzz, fizzbuzz:calc(Pid, 5)),
	?_assertEqual(fizzbuzz, fizzbuzz:calc(Pid, 15))].  