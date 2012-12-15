%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2012 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(erlangdc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() ->
    start_db(),
    start_deps(erlangdc, permanent).

start_deps(App, Type) ->
    case application:start(App, Type) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            start_deps(Dep, Type),
            start_deps(App, Type)
    end.

start(_StartType, _StartArgs) ->    
    erlangdc_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_db() ->
    application:start(epgsql),
    application:set_env(epgsql_connpool, pools, 
                       [ {db, 
                         [
                         {host,      "127.0.0.1"}, 
                         {username,  "tristan"}, 
                         {password,  "temp4now"}, 
                         {size,      10}, 
                         {opts, [
                                {timeout,  5000}, 
                                {database, "erlangdc"}
                                ]}
                         ]
                         }
                       ]
                       ),
    application:start(epgsql_connpool).
