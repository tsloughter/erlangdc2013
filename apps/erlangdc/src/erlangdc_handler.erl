-module(erlangdc_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         process_post/2,
         delete_resource/2,
         get_json/2,
         terminate/3]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

process_post(Req, State) ->
    %%
    {true, Req, State}.

delete_resource(Req, State) ->
    %%
    {true, Req, State}.

get_json(Req, State) ->
        try
        estatsd:increment("erlangdc_handler.requests"),
        {ok, {<<"basic">>, {Key, Value}}, Req2} = cowboy_req:parse_header(<<"authorization">>, Req),
        User = erlangdc_user:get_user(Key, Value),
        io:format("User ~p~n", [User]),
        {User, Req2, State}
    catch
        T:E ->
            lager:info("at=handle type=~p exception=~p ~p", [T, E, erlang:get_stacktrace()]),
            {401, Req, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.


%%
%% Internal functions
%%
