-module(chat_client).

-compile([export_all]).

register_nickname(NickName) ->
	Pid = spawn(chat_client, client_loop, [NickName]),
	message_router:register_nick(NickName, Pid).

unregister_nickname(NickName) -> 
	message_router:unregister_nick(NickName).

send_message(Addressee, MessageBody) ->
	message_router:handle_chat_message(Addressee, MessageBody).

client_loop(NickName) ->
	receive 
			{print_msg, MessageBody} ->
				io:format("received:~n	~p~n", [MessageBody]),
				client_loop(NickName);
			stop ->
				ok
	end.
 