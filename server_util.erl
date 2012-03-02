-module(server_util).

-compile([export_all]).

start(ServerName, {Module, Function, Args}) ->
	global:trans({ServerName, ServerName}, 
		fun() -> 
			case global:whereis_name(ServerName) of
					undefined ->
						Pid = spawn(Module, Function, Args),
						global:register_name(ServerName, Pid),
						io:format("~p is alive at ~p~n", [ServerName, Pid]);
					_ -> 
						ok
			end
		end).

stop(ServerName) ->
	global:trans({ServerName, ServerName}, 
		fun() -> 
			case global:whereis_name(ServerName) of
				undefined ->
					ok;
				_ -> 
					global:send(ServerName, shutdown)
			end
		end).