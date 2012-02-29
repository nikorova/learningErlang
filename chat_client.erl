-module(chat_client).

-compile([export_all]).

register_nickname(NickName) ->
	Pid = spawn(chat_client, handle_messages, [NickName]),
	message_router:register_nick(NickName, Pid).

unregister_nickname(NickName) -> 
	message_router:unregister_nick(NickName).

send_message(Addressee, MessageBody) ->
	message_router:send_chat_message(Addressee, MessageBody).

handle_messages(NickName) ->
	receive 
			{print_msg, MessageBody} ->
				io:format("~p received: ~p~n", [NickName, MessageBody]),
				handle_messages(NickName);
			stop ->
				ok
	end.

start_router() ->
	message_router:start().