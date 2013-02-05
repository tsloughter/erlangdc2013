estatsd
=============
Heavily modified fork of the [Opscode estatsd](https://github.com/opscode/estatsd), able to publish metrics to [Librato](https://metrics.librato.com).


Adding estatsd as a dependency
-----------------------------------------
Even though **estatsd** comes as a node you can add it as a dependency:

1. Clone or submodule **estatsd** into your `priv/` directory or whatever have you
2. Include it in `sub_dirs` in your `rebar.config`, e.g.:

     ```erlang
     {sub_dirs, [
       "apps/my_app",
       "priv/estatsd/apps/estatsd",
       "rel"
     ]}.
     ```

3. Add an `include_cond` to your reltool.config:

     ```erlang
     % ...
     {app, my_app, [{incl_cond, include}]},
     {app, estatsd, [{incl_cond, include}]}
     % ...
     ```

4. Make sure you have a `estatsd` section, like the one in `rel/files/app.config`, in your environment.

Should you know a better way, let me know!


Installation & Usage
--------------------
I recommend you run **estatsd** as a standalone node to which you talk from your various clients.
Otherwise, include **estatsd** into your project, then make sure you start the application, e.g.:

```erlang
application:start(estatsd).
```

See `rel/files/app.config` for a list of configuration options and their descriptions.


### Counters
```erlang
% Increment `num_foos` by one:
estatsd:increment(num_foos).

% Decrement `num_bars` by 3:
estatsd:decrement(<<"num_bars">>, 3).

% Increment `tcp.bytes_in` by 512:
estatsd:increment("tcp.bytes_in", 512).
```


### Timers
```erlang
% Report that `sometask` took 1534ms:
estatsd:timing(sometask, 1534).
```


### Sampling
```erlang
% Let estatsd know that only 10% are being reported.
% 125 * (1/0.1) = value actually incremented by.
estatsd:increment(num_foos, 125, 0.1).
```


Metrics Groups & Librato
========================
~~Librato does not (yet) support combining several different metrics into one chart.~~

This has been implemented! :)

To achieve this effect the [Librato Adapter](https://github.com/johannesh/estatsd/blob/master/apps/estatsd/src/adapters/estatsda_librato.erl) introduces the notion of _groups_:

```erlang
estatsd:increment("myapp.calls-failure").
estatsd:increment("myapp.calls-success").
estatsd:increment("myapp.calls-unknown").
```

As a result you get one metric named `myapp.calls` including three different graphs, namely `failure`, `success` and `unknown`.
