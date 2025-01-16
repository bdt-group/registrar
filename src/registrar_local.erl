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
-export([start/0, start/1]).
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
                clean_time :: millisecs()}).
-type state() :: #state{}.
-type options() :: #{clean_interval => millisecs()}.
-type millisecs() :: non_neg_integer().

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    start(#{}).

-spec start(options()) -> ok | {error, term()}.
start(Opts) ->
    registrar_sup:start_child(?MODULE, [Opts]).

-spec stop() -> ok.
stop() ->
    registrar_sup:stop_child(?MODULE).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    Name = maps:get(name, Opts, ?MODULE),
    gen_server:start_link({local, Name}, ?MODULE, Opts, []).

-spec register_name(term(), pid()) -> yes | no.
register_name(Name, Pid) ->
    case insert_new(Name, Pid) of
        yes -> yes;
        no ->
            %% whereis_name/1 will trigger read-repair if needed
            case whereis_name(Name) of
                undefined ->
                    insert_new(Name, Pid);
                _ ->
                    no
            end
    end.

-spec unregister_name(term()) -> ok.
unregister_name(Name) ->
    _ = ets:delete(?MODULE, Name),
    ok.

-spec whereis_name(term()) -> pid() | undefined.
whereis_name(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, Pid} = Obj] ->
            %% Read-Repair
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete_object(?MODULE, Obj),
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
    _ = ets:new(?MODULE, [public, named_table,
                          {read_concurrency, true},
                          {write_concurrency, true}]),
    CleanInterval = maps:get(clean_interval, Opts, timer:minutes(1)),
    CleanTime = clean_time(CleanInterval),
    State = #state{clean_interval = CleanInterval,
                   clean_time = CleanTime},
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
handle_info(timeout, State) ->
    ?LOG_DEBUG("Cleaning ~s table from dead processes", [?MODULE]),
    DeadObjs = ets:foldl(
                 fun({_, Pid} = Obj, Acc) ->
                         case is_process_alive(Pid) of
                             true -> Acc;
                             false -> [Obj|Acc]
                         end
                 end, [], ?MODULE),
    lists:foreach(
      fun(Obj) ->
              ets:delete_object(?MODULE, Obj)
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
-spec insert_new(term(), pid()) -> yes | no.
insert_new(Name, Pid) ->
    case ets:insert_new(?MODULE, {Name, Pid}) of
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
