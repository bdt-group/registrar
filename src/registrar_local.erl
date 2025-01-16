%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2021, Big Data Technology. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @doc
%%%     A registrar based on ETS with read-repair
%%% @end
%%% Created :  5 Mar 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(registrar_local).

-dialyzer(unknown).
-behaviour(gen_server).

%% API
-export([start/0, start/1, start/2]).
-export([stop/0]).
-export([start_link/1]).
-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).
-export([send/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {clean_interval :: millisecs(),
                clean_time :: millisecs(),
                name :: registrar_name()}).
-type state() :: #state{}.
-type options() :: #{clean_interval => millisecs()}.
-type millisecs() :: non_neg_integer().
-type registrar_name() :: atom().

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    start(?MODULE, #{}).

-spec start(options() | registrar_name()) -> ok | {error, term()}.
start(Registrar) when is_atom(Registrar) ->
    start(Registrar, #{});
start(Opts) ->
    start(?MODULE, Opts).

-spec start(registrar_name(), options()) -> ok | {error, term()}.
start(Registrar, Opts) ->
    registrar_sup:start_child(Registrar, ?MODULE, [Opts#{name => Registrar}]).

-spec stop() -> ok.
stop() ->
    registrar_sup:stop_child(?MODULE).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    Registrar = maps:get(name, Opts, ?MODULE),
    gen_server:start_link({local, Registrar}, ?MODULE, Opts, []).

-spec register_name(term(), pid()) -> yes | no.
register_name(Name0, Pid) ->
    {Registrar, Name} = get_registrar(Name0),
    case insert_new(Registrar, Name, Pid) of
        yes -> yes;
        no ->
            %% whereis_name/1 will trigger read-repair if needed
            case whereis_name(Name0) of
                undefined ->
                    insert_new(Registrar, Name, Pid);
                _ ->
                    no
            end
    end.

-spec unregister_name(term()) -> ok.
unregister_name(Name0) ->
    {Registrar, Name} = get_registrar(Name0),
    _ = ets:delete(Registrar, Name),
    ok.

-spec whereis_name(term()) -> pid() | undefined.
whereis_name(Name0) ->
    {Registrar, Name} = get_registrar(Name0),
    case ets:lookup(Registrar, Name) of
        [{_, Pid} = Obj] ->
            %% Read-Repair
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete_object(Registrar, Obj),
                    undefined
            end;
        [] ->
            undefined
    end.

-spec send(term(), term()) -> pid().
send(Name, Msg) ->
    case whereis_name(Name) of
        undefined ->
            erlang:error({badarg, {Name, Msg}});
        Pid ->
            erlang:send(Pid, Msg),
            Pid
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(options()) -> {ok, state(), millisecs()}.
init(Opts) ->
    process_flag(trap_exit, true),
    Registrar = maps:get(name, Opts, ?MODULE),
    _ = ets:new(Registrar, [public, named_table,
                            {read_concurrency, true},
                            {write_concurrency, true}]),
    CleanInterval = maps:get(clean_interval, Opts, timer:minutes(1)),
    CleanTime = clean_time(CleanInterval),
    State = #state{
        clean_interval = CleanInterval,
        clean_time = CleanTime,
        name = Registrar
    },
    {ok, State, timeout(CleanTime)}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                  {noreply, state(), millisecs()}.
handle_call(Msg, {Pid, _}, State) ->
    ?LOG_WARNING("Unexpected call from ~p: ~p", [Pid, Msg]),
    noreply(State).

-spec handle_cast(term(), state()) -> {noreply, state(), millisecs()}.
handle_cast(Msg, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Msg]),
    noreply(State).

-spec handle_info(term(), state()) -> {noreply, state(), millisecs()}.
handle_info(timeout, #state{name = Registrar} = State) ->
    ?LOG_DEBUG("Cleaning ~s table from dead processes", [Registrar]),
    DeadObjs = ets:foldl(
                 fun({_, Pid} = Obj, Acc) ->
                         case is_process_alive(Pid) of
                             true -> Acc;
                             false -> [Obj|Acc]
                         end
                 end, [], Registrar),
    lists:foreach(
      fun(Obj) ->
              ets:delete_object(Registrar, Obj)
      end, DeadObjs),
    CleanInterval = State#state.clean_interval,
    State1 = State#state{clean_time = clean_time(CleanInterval)},
    noreply(State1);
handle_info(Msg, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Msg]),
    noreply(State).

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term() | {down, term()}, state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_registrar(term()) -> {registrar_name(), term()}.
get_registrar({?MODULE, Registrar, Name}) -> {Registrar, Name};
get_registrar(Name) -> {?MODULE, Name}.

-spec insert_new(registrar_name(), term(), pid()) -> yes | no.
insert_new(Registrar, Name, Pid) ->
    case ets:insert_new(Registrar, {Name, Pid}) of
        true -> yes;
        false -> no
    end.

-spec current_time() -> millisecs().
current_time() ->
    erlang:system_time(millisecond).

-spec clean_time(millisecs()) -> millisecs().
clean_time(Timeout) ->
    current_time() + Timeout.

-spec timeout(millisecs()) -> millisecs().
timeout(CleanTime) ->
    max(0, CleanTime - current_time()).

-spec noreply(state()) -> {noreply, state(), millisecs()}.
noreply(#state{clean_time = CleanTime} = State) ->
    {noreply, State, timeout(CleanTime)}.
