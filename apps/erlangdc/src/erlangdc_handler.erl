-module(erlangdc_handler).

-export([init/3, 
         handle/2, 
         terminate/3]).

-record(state, {}).

init({tcp, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    try
        estatsd:increment("erlangdc_handler.requests"),
        {ok, {<<"basic">>, {Key, _Value}}, Req2} = cowboy_req:parse_header(<<"authorization">>, Req),
        User = erlangdc_user:get_user(Key),
        {ok, Req3} = cowboy_req:reply(200, [], User, Req2),
        {ok, Req3, State}
    catch
        T:E ->
            lager:info("at=handle type=~p exception=~p ~p", [T, E, erlang:get_stacktrace()]),
            {ok, Req4} = cowboy_req:reply(401, Req),
            {ok, Req4, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%
%% Internal functions
%%
