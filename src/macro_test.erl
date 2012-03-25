-module(macro_test).

-compile([export_all]).

-define(SERVER, macro_test).

start() ->
	case whereis(?SERVER) of 
		undefined ->
			Pid = spawn(?SERVER, hey_server, []),
			register(?SERVER, Pid);
		_ -> 
			ok
	end.

stop() ->
	case whereis(?SERVER) of 
		undefined -> 
			ok;
		_ ->
			?SERVER ! shutdown
	end.
			

print_message(Message) ->
	?SERVER ! {print_msg, Message}.

hey_server() ->
	receive
		{print_msg, Message} ->
			io:format("~p~n", [Message]);
		shutdown ->
			io:format("pissing off~n", [])
	end.