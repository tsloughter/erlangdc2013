%% @author Richard Jones <rj@metabrew.com>
%% @copyright 2011 Richard Jones
%% @doc The EStatsD application module.
-module (estatsd_app).

%% This is an OPT application.
-behaviour (application).

%% OTP application callbacks.
-export ([
  start/2,
  stop/1
]).


% ====================== \/ APPLICATION CALLBACKS ==============================

%% @doc Starts the main supervisor.
start(_StartType, _StartArgs) -> estatsd_sup:start_link().


%% @doc Returns ok.
stop(_State) -> ok.

% ====================== /\ APPLICATION CALLBACKS ==============================
