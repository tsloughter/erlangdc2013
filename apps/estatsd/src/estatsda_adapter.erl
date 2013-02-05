%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2012 Johannes Huning
%% @doc Event handler implementing the generic parts of an adapter.
%%      In this case of course metrics to be published are the events.
-module (estatsda_adapter).

%% This is an OTP gen_event handler.
% -behaviour (gen_event).

%% See estatsd.hrl for a complete list of introduced types.
-include ("estatsd.hrl").

%% Behaviour definition for estatsda_adapter.
-export ([
  behaviour_info/1
]).

%% OPT gen_event callbacks.
-export ([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% @doc The handler's state: The backing adapter's module and state.
-record (state, {
  adapter :: atom(),
  adapter_state :: term()
}).


% ====================== \/ BEHAVIOUR DEFINITION ===============================

%% Whenever a new adapter is added to the metrics event manager,
% this function is called to initialize the adapter.
% -callback init(Args::term()) -> {ok, State::term()}.


% Whenever the metrics event manager receives an event sent using
% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
% the adapter to handle the metrics.
% -callback handle_metrics(prepared_metrics(), State::term()) ->
%   {ok, NewState::term()}.


%% Whenever the metrics event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the adapter to handle
%% the metrics.
% -callback sync_handle_metrics(prepared_metrics(), State::term()) ->
%   {ok, NewState::term()}.


behaviour_info(callbacks) -> [
  {init,1},
  {handle_metrics, 2},
  {sync_handle_metrics, 2}
];

behaviour_info(_) -> undefined.

% ====================== /\ BEHAVIOUR DEFINITION ===============================


% ====================== \/ GEN_EVENT CALLBACKS ================================

%% @doc Initializes the backing adapter.
init({Adapter, InitArgs}) ->
  {ok, AdapterState} = Adapter:init(InitArgs),
  State = #state{adapter = Adapter, adapter_state = AdapterState},
  {ok, State}.


%% @doc Forwards metrics to the backing adapter.
%%      It's in the clients duty to handle the request asynchronously.
handle_event({metrics, Metrics}, State) ->
  Adapter = State#state.adapter,
  AState = State#state.adapter_state,
  {ok, NewAState} = Adapter:handle_metrics(Metrics, AState),
  {ok, State#state{adapter_state = NewAState}};


%% @doc Logs and drops unexpected events.
handle_event(Event, State) ->
  error_logger:warning_msg("[~s] Ignored event: '~p'", [?MODULE, Event]),
  {ok, State}.


%% @doc Forwards metrics to the backing adapter.
handle_call({metrics, Metrics}, State) ->
  Adapter = State#state.adapter,
  AState = State#state.adapter_state,
  {ok, NewAState, Reply} = Adapter:synchandle_metrics(Metrics, AState),
  {ok, Reply, State#state{adapter_state = NewAState}};


%% @doc Logs and drops unexpected calls.
handle_call(Request, State) ->
  error_logger:warning_msg("[~s] Ignored call: '~p'", [?MODULE, Request]),
  {ok, undefined, State}.


%% @doc Logs and drops unexpected infos.
handle_info(Info, State) ->
  error_logger:warning_msg("[~s] Ignored info: '~p'", [?MODULE, Info]),
  {ok, State}.


%% @doc Returns the old state.
terminate(_Arg, State) -> State.


%% @doc Returns the old state.
code_change(_OldVsn, State, _Extra) -> State.

% ====================== /\ GEN_EVENT CALLBACKS ================================
