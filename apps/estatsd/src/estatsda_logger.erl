%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2012 Johannes Huning
%% @doc Logging adapter, simply logs handled metrics.
-module (estatsda_logger).

%% This is an estatsd adapter.
-behaviour (estatsda_adapter).

% Adapter callbacks.
-export ([
  init/1,
  handle_metrics/2,
  sync_handle_metrics/2
]).


% ====================== \/ ESTATSD_ADAPTER CALLBACKS ==========================

%% @doc Does nothing.
init([]) -> {ok, nostate}.


%% @doc Immediately returns and logs the metrics in another process.
handle_metrics(Metrics, State) ->
  spawn(fun() -> sync_handle_metrics(Metrics, State) end),
  {ok, State}.


%% @doc Logs the metrics.
sync_handle_metrics(Metrics, State) ->
  error_logger:info_msg("[~s] Metrics:~n~p~n", [?MODULE, Metrics]),
  {ok, State, noreply}.

% ====================== /\ ESTATSD_ADAPTER CALLBACKS ==========================
