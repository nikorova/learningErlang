%%%-------------------------------------------------------------------
%%% File        : mucc.erl
%%% Description : multi user chat client
%%% 								creates proxied chat_client pids from web_server 
%%%									requests that interact with message_server 
%%%-------------------------------------------------------------------
-module(mucc).

-behaviour(gen_server).

%% API
-export([start_link/0, register_nickname/1, unregister_nickname/1, poll/1, send_message/3]).

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

register_nickname(Nickname) ->
  case gen_server:call({global, ?SERVER}, {register, Nickname}) of
    ok ->
      ok;
    {error, Error} ->
      Error
  end.

unregister_nickname(Nickname) ->
	gen_server:cast({global, ?SERVER}, {unregister, Nickname}).

poll(Nickname) ->
  case gen_server:call({global, ?SERVER}, {poll, Nickname}) of
    {ok, Messages} ->
      Messages;
    Error ->
      Error
  end.

send_message(Sender, Addressee, Message) ->
	gen_server:cast({global, ?SERVER}, {send_message, Sender, Addressee, Message}).


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
handle_call({register, Nickname}, _From, State) ->
  case dict:find(Nickname, State) of
    error ->
      Pid = spawn(fun() ->
		      process_flag(trap_exit, true),
		      proxy_client([]) end),
      erlang:monitor(process, Pid),
      message_server:register_nick(Nickname, Pid),
      {reply, ok, dict:store(Nickname, Pid, State)};
    {ok, _} ->
      {reply, {error, duplicate_nick_found}, State}
  end;

handle_call({poll, Nickname}, _From, State)->
  case dict:find(Nickname, State) of
    error ->
      {reply, {error, unknown_nick}, State};
    {ok, Pid} ->
      Pid ! {get_messages, self()},
      receive
      	{messages, Messages} ->
      	  {reply, {ok, Messages}, State}
      end
  end;

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({unregister, Nickname}, State) ->
  NewState = case dict:find(Nickname, State) of
	       error ->
		 State;
	       {ok, Pid} ->
		 Pid ! stop,
		 dict:erase(Nickname, State)
	     end,
  {noreply, NewState};

handle_cast({send_message, Sender, Addressee, Message}, State) ->
  case dict:find(Sender, State) of
    error ->
      ok;
    {ok, Pid} ->
      Pid ! {send_message, Addressee, Message}
  end,
  {noreply, State};

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
proxy_client(Messages) ->
  receive
    {printmsg, MessageBody} ->
      proxy_client([MessageBody|Messages]);
    {get_messages, Caller} ->
      Caller ! {messages, lists:reverse(Messages)},
      proxy_client([]);
    {send_message, Addressee, Message} ->
      message_server:send_chat_message(Addressee, Message),
      proxy_client(Messages);
    stop ->
      io:format("Proxy stopping...~n"),
      ok
  end.
