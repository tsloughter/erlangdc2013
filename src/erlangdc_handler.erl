-module(erlangdc_handler).

-export([init/3, 
         handle/2, 
         terminate/2]).

-record(state, {}).

init({tcp, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    try
        {<<"Basic ", Key/binary>> , Req2} = cowboy_req:header(<<"authorization">>, Req),
        {DecodedKey, _} = decoded_credentials(Key),
        User = erlangdc_user:get_user(DecodedKey),
        {ok, Req3} = cowboy_req:reply(200, [], User, Req2),
        {ok, Req3, State}
    catch
        T:E ->
            lager:info("at=handle type=~p exception=~p ~p~n", [T, E, erlang:get_stacktrace()]),
            {ok, Req4} = cowboy_req:reply(401, Req),
            {ok, Req4, State}
    end.

terminate(_Req, _State) ->
    ok.

%%
%% Internal functions
%%

decoded_credentials(EncodedCredentials) ->    
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {undefined, undefined}
    end.

