-module(web_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link(Port) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	WebServer = {web_server, {web_server, start_link, [Port]},
		permanent, 5000, worker, [web_server]}, 
	Mucc = {mucc, {mucc, start_link, []},
		permanent, 5000, worker, [mucc]},
	{ok, {{one_for_all, 5, 30}, [WebServer, Mucc]}}.