%% @author Richard Jones <rj@metabrew.com>
%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2011 Richard Jones
%% @doc Global type definitions.

%% A key is either an atom, a binary or a list/string.
-type key() :: atom() | binary() | list().

%% A counter consists of a naming key and a tuple of it's total value and
%% the number of increments.
-type counter() ::
  {key(), {Value::non_neg_integer(), NoIncrements::non_neg_integer()}}.
-type counters() :: [counter()].

%% A duration is an integer >= 0. Unit is ms.
-type duration() :: non_neg_integer().

%% A timer consists of a naming key and contains a list of measured durations.
-type timer() :: {key(), [duration()]}.
-type timers() :: [timer()].

%% Raw metrics are tuples of counters and timers.
-type metrics() :: {counters(), timers()}.


%% @doc A prepared counter is an already converted binary key and
%%      the computed value per second.
-type prepared_counter() ::
  {KeyAsBinary::binary(), ValuePerSec::float(),
    NoIncrements::non_neg_integer()}.
-type prepared_counters() :: [prepared_counter()].

%% @doc A prepared timer is an already converted string key, the same list of
%%      durations to support further computations and the calculated maximum,
%%      minimum and the _sorted_ overall number of durations.
-type prepared_timer() :: {KeyAsString::string(), [duration()],
  Count::non_neg_integer(), Min::non_neg_integer(), Max::non_neg_integer()}.
-type prepared_timers() :: [prepared_timer()].

%% @doc Prepated metrics are tuples of prepared counters and timers.
-type prepared_metrics() :: {prepared_counters(), prepared_timers()}.
