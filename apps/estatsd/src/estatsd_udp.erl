%% @author Richard Jones <rj@metabrew.com>
%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2011 Richard Jones
%% @doc TODO
-module (estatsd_udp).

% Include global type definitions.
-include ("estatsd.hrl").

%% This is an OTP gen_server.
-behaviour (gen_server).

%% Client API.
-export ([start_link/0]).

%% OTP gen_server callbacks.
-export ([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% @doc The server's state:
-record (state, {
  socket :: inet:socket(),
  batch = [] :: [binary()],
  max_batch_size :: non_neg_integer(),
  max_batch_age :: non_neg_integer()
}).

% ====================== \/ CLIENT API =========================================

%% @doc Starts the estatsd UDP server.
%%      Registers a process named `estatsd_udp`.
-spec start_link() -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

% ====================== /\ CLIENT API =========================================


% ====================== \/ GEN_SERVER CALLBACKS ===============================

%% @doc Initializes the server's state.
%%      Opens up an UDP socket on the configured port.
init([]) ->
  % Read the batch settings
  MaxBatchSize = estatsd:env_or_default(max_batch_size, 50),
  MaxBatchAge = estatsd:env_or_default(max_batch_age, 1000),

  Port = estatsd:env_or_default(port, 3344),
  ServerOpts = estatsd:env_or_default(server_opts, []) ++
    [binary, {active, once}],
  % Open the socket
  {ok, Socket} = gen_udp:open(Port, ServerOpts),

  {ok, #state{
    socket = Socket,
    batch = [],
    max_batch_size = MaxBatchSize,
    max_batch_age = MaxBatchAge
  }}.


%% @doc Logs and drops unexpected calls.
handle_call(Request, From, State) ->
  error_logger:warning_msg(
    "[~s] Ignored call '~p' from '~p'", [?MODULE, Request, From]),
  {noreply, ok, State}.


%% @doc Logs and drops unexpected casts.
handle_cast(Message, State) ->
  error_logger:warning_msg("[~s] Ignored cast '~p'", [?MODULE, Message]),
  {noreply, State}.


%% @doc Closes the current batch, submitting it to be parsed.
%%      Makes the received binary the first element in the new batch.
handle_info({udp, Socket, _Host, _Port, Bin},
    #state{batch = Batch, max_batch_size = MaxBatchSize} = State)
    when length(Batch) == MaxBatchSize ->

  handle_batch_(Batch),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{batch = [Bin]}};


%% @doc Appends the received binary to the current batch.
handle_info({udp, Socket, _Host, _Port, Bin},
    #state{batch = Batch, max_batch_age = MaxAge} = State) ->

  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{batch = [Bin | Batch]}, MaxAge};


%% @doc Timeouts the current batch, submitting it to be parsed.
handle_info(timeout, #state{batch = Batch} = State) ->
  handle_batch_(Batch),
  % Continue with a new batch.
  {noreply, State#state{batch=[]}};


%% @doc Logs and drops unexpected info messages.
handle_info(Info, State) ->
  error_logger:info_msg("[~s] Ignored info '~p'~n", [?MODULE, Info]),
  {noreply, State}.


%% @doc Does nothing.
terminate(_Reason, _State) -> ok.


%% @doc Returns the old state.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ====================== /\ GEN_SERVER CALLBACKS ===============================


% ====================== \/ HELPER FUNCTIONS ===================================

%% @doc Handles the batch's messages in a new process.
-spec handle_batch_([binary()]) -> no_return().
handle_batch_(Batch) ->
  %% Make sure we process messages in the order received
  spawn(fun() ->
    [handle_message_(Message) || Message <- lists:reverse(Batch)]
  end).


%% @doc Splits the given message, then calls handle_line_ for every line found.
-spec handle_message_(Message::binary()) -> no_return().
handle_message_(Message) -> try
  [handle_line_(Line) || Line <- binary:split(Message, <<"\n">>, [global])]
catch
  error:Reason -> error_logger:error_report(
    {error, "handle_message_ failed",
      Message, Reason, erlang:get_stacktrace()})
end.


%% @doc Parses the given line and sends the included metrics to the server.
-spec handle_line_(Line::binary()) -> no_return().
handle_line_(Line) ->
  Tokens = binary:split(Line, [<<":">>, <<"|">>], [global]),
  case length(Tokens) of
    4 -> [Key, Value, Type, <<"@", SampleRate/binary>>] = Tokens;
    % In case no sample rate was set, default to 1.
    3 -> [Key, Value, Type, SampleRate] = Tokens ++ [<<"1.0">>]
  end,
  send_estatsd_metric_(Type, Key, Value, SampleRate).


%% @doc Sends the timing information to the server.
send_estatsd_metric_(<<"ms">>, Key, Value, _SampleRate) ->
  estatsd:timing(Key, convert_value_(Value));

%% @doc Sends the counter information to the server.
send_estatsd_metric_(<<"c">>, Key, Value, SampleRate) ->
  estatsd:increment(Key, convert_value_(Value),
    list_to_float(binary_to_list(SampleRate)));

%% @doc Ignores any non-matching metrics.
send_estatsd_metric_(_, _, _, _) -> ignored.


%% @doc Formats the given binary into an integer.
-define (to_int(Value), list_to_integer(binary_to_list(Value))).

%% @doc Converts the given value to its integer representation.
-spec convert_value_(Value :: binary() | integer()) -> integer().
convert_value_(Value) when is_binary(Value) -> ?to_int(Value);
convert_value_(Value) when is_integer(Value) -> Value.

% ====================== /\ HELPER FUNCTIONS ===================================
