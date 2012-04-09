%%%-------------------------------------------------------------------
%%% File        : web_server.erl
%%% Description : web server proxies requests to mucc
%%%-------------------------------------------------------------------
-module(web_server).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, dispatch_requests/1]).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).

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
start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
	gen_server:cast(?SERVER, stop).

dispatch_requests(Req) ->
	Path = Req:get(path),
	Action = clean_path(Path),
	handle(Action, Req).

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
init([Port]) ->
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
  mochiweb_http:start([
  	{port, Port},
    {loop, fun(Req) ->
			dispatch_requests(Req) end}
	]),
  erlang:monitor(process, mochiweb_http),
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
handle_info({'DOWN', _, _, {mochiweb_http, _}, _}, State) ->
	{stop, normal, State};

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
	mochiweb_http:stop(),
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
handle("/send", Req) ->
  Params = Req:parse_qs(),
  Sender = proplists:get_value("nick", Params),
  Addressee = proplists:get_value("to", Params),
  Message = proplists:get_value("msg", Params),
  mucc:send_message(Sender, Addressee, Message),
  success(Req, ?OK);

handle("/poll", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  case mucc:poll(Nickname) of
    {error, Error} ->
      failure(Req, subst("Error: ~s~n", Error));
    Messages ->
      case length(Messages) == 0 of
	true ->
	  success(Req, <<"none">>);
	false ->
	  Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Messages),
	  success(Req, subst(lists:flatten(Template), Messages))
      end
  end;

handle("/unregister", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  mucc:unregister_nickname(Nickname),
  success(Req, ?OK);

handle("/register", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  case mucc:register_nickname(Nickname) of
    ok ->
      success(Req, ?OK);
    Error ->
      failure(Req, subst("Error: ~s", [Error]))
  end;

handle(_, Req) ->
  Req:respond(not_found).

failure(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.
