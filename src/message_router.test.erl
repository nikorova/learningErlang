-module(message_router).

% constant defined a la C
%-define(SERVER, message_router).

-compile(export_all).

start() ->
	global:trans({message_router, message_router}, 
		fun() -> 
			case global:whereis_name(message_router) of
				undefinded ->
					Pid = spawn(message_router, route_messages, [dict:new()]),
					global:register_name(message_router, Pid);
				_ -> 
					ok
			end
		end).

stop() ->
	global:trans({message_router, message_router}, 
		fun() -> 
			case global:whereis_name(message_router) of
				undefinded ->
					ok;
				_ -> 
					global:send(message_router, shutdown)
			end
		end).
	
send_chat_message(Addressee, MessageBody) ->
	global:send(message_router, {send_chat_msg, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
	global:send(message_router, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->	
	global:send(message_router, {unregister_nick, ClientName}).

route_messages(Clients) ->
	receive
		{send_chat_msg, ClientName, MessageBody} ->
			case dict:find(ClientName, Clients) of
				{ok, ClientPid} ->
					ClientPid ! {print_msg, MessageBody};
				error ->
					io:format("unknown client: ~p~n", [ClientName])
			end,
			route_messages(Clients);
		{register_nick, ClientName, ClientPid} -> 
		 	route_messages(dict:store(ClientName, ClientPid, Clients));
		{unregister_nick, ClientName} ->
			case dict:find(ClientName, Clients) of
				{ok, ClientPid} -> 
					ClientPid ! stop,
					route_messages(dict:erase(ClientName, Clients));
				error ->
					io:format("unknown client: ~p~n", [ClientName]),
					route_messages(Clients)
			end;
		shutdown ->
			io:format("router: shutting down~n");
		BadMessage -> 
			io:format("router: ah! received ~p~n", [BadMessage]),
			route_messages(Clients)
	end.
