-module(message_router).

-compile([export_all]).

start() ->
	server_util:start(message_router, {message_router, route_messages, [dict:new()]}),
	message_store:start().

stop() ->
	server_util:stop(message_router),
	message_store:stop().
	
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
					message_store:save_message(ClientName, MessageBody),
					io:format("message saved for ~p~n", [ClientName])
			end,
			route_messages(Clients);
		
		{register_nick, ClientName, ClientPid} -> 
		 	Messages = message_store:find_message(ClientName),
		 	lists:foreach(fun(Msg) -> ClientPid ! {print_msg, Msg} end, Messages),
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
