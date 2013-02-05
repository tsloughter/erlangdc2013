%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2012 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(erlangdc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Dispatch = cowboy_router:compile([
                                     %% {HostMatch, list({PathMatch, Handler, Opts})}
                                     {'_', [{"/user", erlangdc_handler, []}]}
                                     ]),

    ListenPort = list_to_integer(os:getenv("PORT")),

    ChildSpecs = [ranch:child_spec(erlangdc_cowboy, 100,
                                   ranch_tcp, [{port, ListenPort}],
                                   cowboy_protocol, [{env, [{dispatch, Dispatch}]}])],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
