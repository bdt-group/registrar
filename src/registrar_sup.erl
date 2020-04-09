%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(registrar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/2, start_child/3]).
-export([stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(module(), list()) -> ok | {error, term()} | ignore.
start_child(Mod, Args) ->
    start_child(Mod, Mod, Args).

-spec start_child(term(), module(), list()) -> ok | {error, term()} | ignore.
start_child(Id, Mod, Args) ->
    Spec = #{id => Id,
             start => {Mod, start_link, Args},
             restart => permanent,
             shutdown => timer:seconds(5),
             type => worker,
             modules => [Mod]},
    case supervisor:start_child(?MODULE, Spec) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Err -> Err
    end.

-spec stop_child(term()) -> ok.
stop_child(Id) ->
    _ = supervisor:terminate_child(?MODULE, Id),
    _ = supervisor:delete_child(?MODULE, Id),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
