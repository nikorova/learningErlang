%%%-------------------------------------------------------------------
%%% File        : message_server.erl
%%% Description : routes messages for the chat application 
%%%-------------------------------------------------------------------
-module(message_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([handle_chat_message/2, register_nick/2, unregister_nick/1, stop/0]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast({global, ?SERVER}, stop).

handle_chat_message(Addressee, MessageBody) ->
	gen_server:call({global, ?SERVER}, {handle_chat_message, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
	gen_server:call({global, ?SERVER}, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
	gen_server:call({global, ?SERVER}, {unregister_nick, ClientName}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	process_flag(trap_exit, true),
	{ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({unregister_nick, ClientName}, _From, Clients) ->
	case dict:find(ClientName, Clients) of 
		{ok, ClientPid} ->
			ClientPid ! stop,
			dict:erase(ClientName, Clients);
		error ->
			io:format("unkown client: ~p~n", [ClientName]),
			Clients			
	end,
	{reply, ok, Clients};

handle_call({register_nick, ClientName, ClientPid}, _From, Clients) ->
	Messages = message_db:find_message(ClientName), 
	lists:foreach(fun(Msg) -> ClientPid ! {print_msg, Msg} end, Messages),
	{reply, ok, dict:store(ClientName, ClientPid, Clients)};

handle_call({handle_chat_message, ClientName, MessageBody}, _From, Clients) ->
	case dict:find(ClientName, Clients) of 
		{ok, ClientPid} -> 
			ClientPid ! {print_msg, MessageBody};
		error ->
			message_db:save_message(ClientName, MessageBody),
			io:format("message saved for ~p~n", [ClientName])
	end,
	{reply, ok, Clients};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	message_db:shutdown(),
	io:format("message_server going down...~n"),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------