%%%-------------------------------------------------------------------
%%% @author Domenico Stragapede
%%% @doc
%%% Call Center Supervisor. Handle Call center calls at runtime,
%%% starting them when a client start a call.
%%% @end
%%%-------------------------------------------------------------------
-module(call_center_sup).
-author("Domenico Stragapede").

-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([start_call/2, process_message/2, end_call/1]).

%% ------------------------------------------------------------------
%% Supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CC_MODULE, call_center).
-define(CHILD_SPEC(From), {now(), {?CC_MODULE, start_link, [From]}, transient, 5000, worker, [?CC_MODULE]}).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_call(From :: atom() | pid(), ClientId :: pid()) -> string().
start_call(From, ClientId) ->
    case ets:lookup(client2pid, ClientId) of
        [{ClientId, _Pid}] ->
            {error, already_started};

        [] ->
            {ok, Pid} = supervisor:start_child(?SERVER, ?CHILD_SPEC(From)),
            ets:insert(client2pid, {ClientId, Pid}),
            ?CC_MODULE:show_options(Pid)
    end.

-spec process_message(ClientId :: pid(), Msg :: string()) -> string().
process_message(ClientId, Msg) ->
    case ets:lookup(client2pid, ClientId) of
        [{ClientId, Pid}] ->
            ?CC_MODULE:process_message(Pid, Msg);

        [] ->
            {error, invalid}
    end.

-spec end_call(ClientId :: pid()) -> ok.
end_call(ClientId) ->
    case ets:lookup(client2pid, ClientId) of
        [{ClientId, Pid}] ->
            supervisor:terminate_child(?SERVER, Pid),
            supervisor:delete_child(?SERVER, Pid),
            ets:delete(client2pid, ClientId),
            ok;

        [] ->
            {error, invalid}
    end.

%% ------------------------------------------------------------------
%% Supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ets:new(client2pid, [public, named_table]),
    MaxR = 1000, % how many times
    MaxT = 10, % in how many seconds
    {ok, { {one_for_one, MaxR, MaxT}, []} }.

