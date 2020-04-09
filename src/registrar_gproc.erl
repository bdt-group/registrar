%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(registrar_gproc).

%% API
-export([start/0]).
-export([stop/0]).
-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).
-export([send/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(gproc) of
        {ok, _} -> ok;
        Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    case application:stop(gproc) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        Err -> Err
    end.

-spec register_name(term(), pid()) -> yes | no.
register_name(Name, Pid) when Pid == self() ->
    try gproc:add_local_name(Name) of
        _ -> yes
    catch error:badarg ->
            no
    end.

-spec unregister_name(term()) -> ok.
unregister_name(Name) ->
    try gproc:unreg({n, l, Name}) of
        _ -> ok
    catch error:badarg ->
            ok
    end.

-spec whereis_name(term()) -> pid() | undefined.
whereis_name(Name) ->
    try gproc:lookup_pid({n, l, Name})
    catch error:badarg ->
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
%%% Internal functions
%%%===================================================================
