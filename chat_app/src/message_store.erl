%%%-------------------------------------------------------------------
%%% File        : message_store.erl
%%% Description : message database server
%%%-------------------------------------------------------------------
-module(message_store).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, stop/0, save_message/2, find_message/1]).

-record(chat_message,
		{addressee, 
		message_body,
		timestamp}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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

 save_message(Addressee, MessageBody) ->
 	gen_server:call({global, ?SERVER}, {save_message, Addressee, MessageBody}).

 find_message(Addressee) ->
 	case gen_server:call({global, ?SERVER}, {find_msgs, Addressee}) of
 		{ok, Messages} ->
 			Messages
 	end.
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
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(chat_message, type)
	catch 
		exit: _ ->
			mnesia:create_table(chat_message, 
				[{attributes, record_info(fields, chat_message)},
				{type, bag}, 
				{disc_copies, [node()]}] )
	end,
  {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({find_msgs, Addressee}, _From, State) ->
	Messages = get_messages(Addressee), 
	{reply, {ok, Messages}, State};

handle_call({save_message, Addressee, MessageBody}, _From, State) ->
	store_message(Addressee, MessageBody),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.

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
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	mnesia:stop(),
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
store_message(Addressee, MessageBody) ->
	F = fun() ->
		{_, TimeStamp, _} = erlang:now(),
		mnesia:write(#chat_message{addressee=Addressee, message_body=MessageBody, timestamp=TimeStamp}) end,
	mnesia:transaction(F).

get_messages(Addressee) ->
	F = fun() ->
		Query = qlc:q([M || M <- mnesia:table(chat_message),
			M#chat_message.addressee =:= Addressee]),
		Results = qlc:e(Query),
		delete_messages(Results),
		lists:map(fun(Msg) -> Msg#chat_message.message_body end, Results)
	end,
	{atomic, Messages} = mnesia:transaction(F),
	Messages.

delete_messages(Messages) ->
	F = fun() ->
		lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) 
	end,
	mnesia:transaction(F).