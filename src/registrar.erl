%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(registrar).

-behaviour(application).

%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1, config_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
            {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    registrar_sup:start_link().

-spec stop(term()) -> any().
stop(_State) ->
    ok.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
