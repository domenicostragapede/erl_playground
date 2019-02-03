%%%-------------------------------------------------------------------
%%% @author Domenico Stragapede
%%% @doc
%%% Simple Echo server.
%%% This server will respond to a maximum of 3 requests with a timeout
%%% of 10 seconds.
%%% @end
%%%-------------------------------------------------------------------
-module(operator).
-author("Domenico Stragapede").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {total_request :: integer()}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).


%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{total_request = 0}, ?TIMEOUT}.


handle_call(_Request, _From, #state{total_request = TotalRequest})
    when TotalRequest < 2 ->
    State = #state{total_request = TotalRequest + 1},
    {reply, _Request, State};

handle_call(_Request, _From, _State)  ->
    %% do last request and quit
    gen_server:cast(self(), max_requests),
    {reply, _Request, _State}.


handle_cast(max_requests, _State) ->
    {stop, normal, _State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout, _State) ->
    {stop, timeout, _State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
