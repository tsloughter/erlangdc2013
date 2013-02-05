%% @author Richard Jones <rj@metabrew.com>
%% @copyright 2011 Richard Jones
%% @doc The EStatsD main supervisor module.
-module (estatsd_sup).

% This is an OPT supervisor.
-behaviour (supervisor).

%% Client API.
-export ([
  start_link/0
]).

%% OTP supervisor callbacks.
-export ([
  init/1
]).


% ====================== \/ CLIENT API =========================================

%% @doc Starts EStatsD's main supervisor.
%%      Registers a process named `estatsd_sup`.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% ====================== /\ CLIENT API =========================================


% ====================== \/ SUPERVISOR CALLBACKS ===============================

%% @doc Builds the supervisor and child-spec.
init([]) ->
  ChildSpec = [
    % Server handling metrics.
    worker_spec_(estatsd_server),
    % UDP listener parsing packets and sending them to estatsd_server.
    worker_spec_(estatsd_udp)
  ],

  RestartStrategy = one_for_one,
  % If more than MaxRestarts number of restarts occur in the last
  % RestartTimeframe seconds, then the supervisor terminates all the child
  % processes and then itself.
  MaxRestarts = 10000,
  RestartTimeframe = 10,

  % Final spec.
  {ok, {{RestartStrategy, MaxRestarts, RestartTimeframe}, ChildSpec}}.

% ====================== /\ SUPERVISOR CALLBACKS ===============================


% ====================== \/ HELPER FUNCTIONS ===================================

%% @doc Builds a worker spec for the supervisor.
worker_spec_(Module) ->
  Id = Module,
  StartFunc = {Module, start_link, []},
  RestartType = permanent,
  ShutdownTimeout = 5000,
  ChildType = worker,
  Modules = [Module],
  {Id, StartFunc, RestartType, ShutdownTimeout, ChildType, Modules}.

% ====================== /\ HELPER FUNCTIONS ===================================
