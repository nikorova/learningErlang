-module (test_message_route).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
	{setup,
		fun() -> message_router:start() end,
		fun() -> message_router:stop() end, 
		fun generate_tests/0}.

generate_tests() ->
	[?_assertEqual(Pid, message_route:send_message(A, M))].