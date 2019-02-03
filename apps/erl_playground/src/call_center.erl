%%%-------------------------------------------------------------------
%%% @author Domenico Stragapede
%%% @doc
%%% Call Center Server. Handle request from generic server,
%%% receiving message as string.
%%% @end
%%%-------------------------------------------------------------------
-module(call_center).
-author("Domenico Stragapede").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([show_options/1, process_message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    server:: pid(),
    operator:: pid() | undefined
}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(OP_MODULE, operator).
-define(OP_WELCOME_MSG, "Welcome to Operator chat, please type your message.").
-define(OP_MAX_REQ_MSG, "Max requests reached, operator is gone away.").
-define(OP_TIMEOUT_MSG, "Operator is gone away.").

-define(WEATHER_LIST, ["sunny", "snow", "cloudly", "overcast", "rain", "clear"]).

-define(CC_MENU, "This is an automatic responder, here you are the available options:
             \t Press 1 to receive the weather forecast
             \t Press 2 to receive the answer to the Ultimate Question of Life, the Universe, and Everything
             \t Press 3 to call an operator
             \t Press 4 to read again this message \n").

-define(CC_OPT_WEATHER, "1").
-define(CC_OPT_DEEP_THOUGH, "2").
-define(CC_OPT_OPERATOR, "3").
-define(CC_OPT_MENU, "4").

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start_link(From) ->
    gen_server:start_link(?MODULE, [From], []).

-spec show_options(Ref :: atom() | pid()) -> string().
show_options(Ref) ->
    gen_server:call(Ref, ?CC_OPT_MENU).

-spec process_message(Ref :: atom() | pid(), Msg :: string()) -> string().
%%process_message(Ref, "1") ->
%%    gen_server:call(Ref, weather_forecast);
%%
%%process_message(Ref, "2") ->
%%    gen_server:call(Ref, deep_though_question);
%%
%%process_message(Ref, "3") ->
%%    gen_server:call(Ref, call_operator);
%%
%%process_message(Ref, "4") ->
%%    show_options(Ref);

process_message(Ref, Msg) ->
    gen_server:call(Ref, Msg).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([From]) ->
    process_flag(trap_exit, true),
    {ok, #state{server=From, operator=undefined}}.


handle_call(_Request, _From, State = #state{operator = Operator})
    when Operator =/= undefined ->
    Reply = gen_server:call(Operator, _Request),
    {reply, Reply, State};

handle_call(_Request = ?CC_OPT_MENU, _From, State) ->
    Reply = ?CC_MENU,
    {reply, Reply, State};

handle_call(_Request = ?CC_OPT_WEATHER, _From, State = #state{server=Server, operator = _Operator}) ->
    Reply = lists:nth(rand:uniform(length(?WEATHER_LIST)), ?WEATHER_LIST),
    gen_server:cast(Server, {response, ?CC_MENU}),
    {reply, Reply, State};

handle_call(_Request = ?CC_OPT_DEEP_THOUGH, _From, State = #state{server=Server, operator = _Operator}) ->
    gen_server:cast(Server, {response, ?CC_MENU}),
    {reply, "42", State};

handle_call(_Request = ?CC_OPT_OPERATOR, _From, #state{server=Server, operator = _Operator}) ->
    {ok, Pid} = ?OP_MODULE:start_link(),
    {reply, ?OP_WELCOME_MSG, #state{server=Server, operator = Pid}};

handle_call(_Request, _From, State) ->
    _ = lager:warning("unknown call [~p ~p ~p]", [_Request, _From, State]),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', _Pid, timeout}, _State = #state{server = Server, operator = _Operator}) ->
    _ = lager:notice("Operator timeout [~p ~p]", [_Pid, _State]),
    gen_server:cast(Server, {response, ?OP_TIMEOUT_MSG}),
    gen_server:cast(Server, {response, ?CC_MENU}),
    {noreply, #state{server = Server, operator = undefined}};

handle_info({'EXIT', Pid, normal}, _State = #state{server = Server, operator = _Operator}) ->
    _ = lager:notice("Operator max req reached [~p ~p]", [Pid, _State]),
    gen_server:cast(Server, {response, ?OP_MAX_REQ_MSG}),
    gen_server:cast(Server, {response, ?CC_MENU}),
    {noreply, #state{server = Server, operator = undefined}};

handle_info(_Info, State) ->
    _ = lager:notice("unknown message [~p ~p]", [_Info, State]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
