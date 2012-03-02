-module(message_store).

-compile([export_all]).

-define(SERVER, message_store).

-include_lib("stdlib/include/qlc.hrl").

-record(chat_message,
		{addressee, 
		message_body,
		timestamp}).

save_message(Addressee, MessageBody) ->
	global:send(?SERVER, {save_msg, Addressee, MessageBody}).

find_message(Addressee) ->
	global:send(?SERVER, {find_msgs, Addressee, self()}),
	receive
		{ok, Messages} ->
			Messages
	end.

start() ->
	server_util:start(?SERVER, {message_store, run, [true]}).

stop() ->
	server_util:stop(?SERVER).

delete_messages(Messages) ->
	F = fun() ->
		lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
	mnesia:transaction(F).

get_messages(Addressee) ->
	F = fun() ->
			Query = qlc:q([M#chat_message.message_body || M <- mnesia:table(chat_message),
								M#chat_message.addressee =:= Addressee]),
			Results = qlc:e(Query),
			delete_messages(Results),
			Results 
		end,
	{atomic, Message} = mnesia:transaction(F),
	Message.

store_message(Addressee, MessageBody) ->
	F = fun() ->
		{_, TimeStamp, _} = erlang:now(),
		mnesia:write(#chat_message{addressee=Addressee, message_body=MessageBody, timestamp=TimeStamp}) end,
	mnesia:transaction(F).

init_store() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try 
		mnesia:table_info(chat_message, type)
	catch 
		exit: _ ->
			mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
					{type, bag}, 
					{disc_copies, [node()]}])
	end.

run(FirstTime) ->
	if 
		FirstTime == true ->
			init_store(),
			run(false);
		true ->
			receive
				{save_message, Addressee, MessageBody} ->
					store_message(Addressee, MessageBody),
					run(FirstTime);
				{find_msgs, Addressee, Pid} ->
					Messages = get_messages(Addressee),
					Pid ! {ok, Messages},
					run(FirstTime);
				 shutdown ->
				 	mnesia:stop(),
				 	io:format("i go to my death...~n")
			end
	end.
