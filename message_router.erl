-module(message_router).

-define(SERVER, message_router).

-compile(export_all).

start() ->
	global:trans({?SERVER, ?SERVER}, 
		fun() -> 
			case global:whereis_name(?SERVER) of
				undefinded ->
					Pid = spawn(message_router, route_messages, [dict:new()]),
					global:register_name(?SERVER, Pid);
				_ -> 
					ok
			end
		end).

stop() ->
	global:trans({?SERVER, ?SERVER}, 
		fun() -> 
			case global:whereis_name(?SERVER) of
				undefinded ->
					ok;
				_ -> 
					global:send_message(?SERVER, shutdown)
			end
		end).
	
send_chat_message(Addressee, MessageBody) ->
	global:send_message(?SERVER, {send_chat_msg, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
	global:send_message(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->	
	global:send_message(?SERVER, {unregister_nick, ClientName}).

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
			end,
			route_messages(dict:erase(ClientName, Clients));
		shutdown ->
			io:format("router: shutting down~n");
		BadMessage -> 
			io:format("router: ah! received ~p~n", [BadMessage]),
			route_messages(Clients)
	end.
