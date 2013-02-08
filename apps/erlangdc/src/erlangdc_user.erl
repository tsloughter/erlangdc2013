%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2012 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(erlangdc_user).

%% API
-export([add_user/1
        ,get_user/2]).

get_user(UserName, ApiKey) ->
    {dirty,{ok, Columns, [User]}} =
        epgsql_connpool:dirty(db, fun(_Db) ->
                                          Sql = "select id, email, created_at, updated_at, name, admin, active from users where name = $1 and apikey = $2",
                                          epgsql_query:equery(Sql, [UserName, ApiKey])
                                  end),
    to_json(Columns, User).

add_user(User) ->
    epgsql_connpool:dirty(db, fun(_Db) ->
                                      Sql = "insert into users (id, apikey, name, email, password_hash) values ('$1', '$2', '$3', '$4', '$5');",
                                      epgsql_query:equery(Sql, [User])
                              end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_json(Columns, User) ->
    Result = lists:zipwith(fun({column, Key = <<"created_at">>, _, _, _, _}, Value) ->
                                   {Key, iso8601:format(Value)};
                              ({column, Key = <<"updated_at">>, _, _, _, _}, Value) ->
                                   {Key, iso8601:format(Value)};                              
                              ({column, Key, _, _, _, _}, Value) ->
                                   {Key, Value}
                           end, Columns, tuple_to_list(User)),
    jsx:encode(Result).
