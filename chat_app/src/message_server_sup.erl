-module(message_server_sup).

-behavior(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	io:format("~p (~p) starting...~n", [?MODULE, self()]),	
	MessageServer = {message_server,{message_server, start_link, []},
		permanent, 5000, worker, [message_server]},
	MessageDB = {message_db, {message_db, start_link, []}, 
		permanent, 5000, worker, [message_db]},
	{ok, {{one_for_all, 5, 30}, [MessageServer, MessageDB]}}.