-module(util).

% -export([crud_controller/3, default_crud/2, connect_main_db/0, http_respond_json/1]).
-compile(export_all).

-include_lib("inets/include/httpd.hrl").

-define(is_method(M, S), M#mod.method == S).

connect_main_db() ->
    odbc:connect("Driver=SQLite3;Database=../main.db", []).

values_to_str(Len) ->
    values_to_str(Len, "").

values_to_str(Len, Acc) when Len == 1 ->
    Acc ++ "?";
values_to_str(Len, Acc) ->
    values_to_str(Len - 1, Acc ++ "?, ").

update_cols_to_str(Cols) ->
    update_cols_to_str(Cols, "").

update_cols_to_str([Col | Cols], Acc) when Cols == [] ->
    Acc ++ Col ++ " = ?";
update_cols_to_str([Col | Cols], Acc) ->
    update_cols_to_str(Cols, Acc ++ Col ++ " = ?, ").

insert_cols_to_str(Cols) ->
    insert_cols_to_str(Cols, "").

insert_cols_to_str([Col | Cols], Acc) when Cols == [] ->
    Acc ++ Col;
insert_cols_to_str([Col | Cols], Acc) ->
    insert_cols_to_str(Cols, Acc ++ Col ++ ",").

maybe_list_to_atom(List) ->
    try
        list_to_atom(List)
    catch
        _:_ ->
            List
    end.

map_from_listkeys_tuplevals(Keys, Values) ->
    map_from_listkeys_tuplevals(Keys, Values, 1, #{}).

map_from_listkeys_tuplevals([Key | Keys], Values, Index, Map) when Keys == [] ->
    maps:put(list_to_atom(Key), maybe_list_to_atom(element(Index, Values)), Map);
map_from_listkeys_tuplevals([Key | Keys], Values, Index, Map) ->
    map_from_listkeys_tuplevals(Keys,
                                Values,
                                Index + 1,
                                maps:put(list_to_atom(Key),
                                         maybe_list_to_atom(element(Index, Values)),
                                         Map)).

% map_from_listkeys_tuplevals(Keys, Values) ->
%     map_from_listkeys_tuplevals(Keys, Values, #{}).

% map_from_listkeys_tuplevals(Keys, Values, #{}) -> [Map = maps:put(list_to_atom(Key), , Map) || <- Keys]

result_to_maparr({selected, _, Values}) when Values == [] ->
    [];
result_to_maparr({selected, Cols, Values}) ->
    result_to_maparr({selected, Cols, Values}, []).

result_to_maparr({selected, Cols, [Val | Values]}, Arr) when Values == [] ->
    Arr ++ [map_from_listkeys_tuplevals(Cols, Val)];
result_to_maparr({selected, Cols, [Val | Values]}, Arr) ->
    result_to_maparr({selected, Cols, Values},
                     Arr ++ [map_from_listkeys_tuplevals(Cols, Val)]).

json_encode(V) ->
    binary_to_list(iolist_to_binary(json:encode(V))).

execute_param_query(Query, P) ->
    case connect_main_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            case {odbc:param_query(Cr, Query, P), odbc:disconnect(Cr)} of
                {{error, Rea}, _} ->
                    {error, Rea};
                {_, {error, Rea}} ->
                    {error, Rea};
                {R, ok} ->
                    R
            end
    end.

table_describe(T) ->
    case connect_main_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            case {odbc:describe_table(Cr, T), odbc:disconnect(Cr)} of
                {{error, Rea}, _} ->
                    {error, Rea};
                {_, {error, Rea}} ->
                    {error, Rea};
                {{ok, D}, ok} ->
                    D
            end
    end.

sql_retype(T) when T == 'SQL_LONGVARCHAR' ->
    {sql_varchar, 128};
sql_retype(T) ->
    T.

sql_maybe_binary_to_list(V) ->
    try
        binary_to_list(V)
    catch
        _:_ ->
            V
    end.

sql_values_from_desc_binmap(Desc, Binmap) ->
    sql_values_from_desc_binmap(maps:keys(Desc), Desc, Binmap, keys_from_desc, #{}).

sql_values_from_desc_binmap(Desc, Binmap, keys_from_desc) ->
    sql_values_from_desc_binmap(maps:keys(Desc), Desc, Binmap, keys_from_desc, #{});
sql_values_from_desc_binmap(Desc, Binmap, keys_from_binmap) ->
    sql_values_from_desc_binmap(maps:keys(Binmap), Desc, Binmap, keys_from_binmap, #{}).

sql_values_from_desc_binmap([Key | Keys], Desc, Binmap, keys_from_desc, Map)
    when Keys == [] ->
    case maps:find(list_to_binary(Key), Binmap) of
        error ->
            {error, no_key};
        {ok, Value} ->
            maps:put(Key, {sql_retype(maps:get(Key, Desc)), [sql_maybe_binary_to_list(Value)]}, Map)
    end;
sql_values_from_desc_binmap([Key | Keys], Desc, Binmap, keys_from_desc, Map) ->
    case maps:find(list_to_binary(Key), Binmap) of
        error ->
            {error, no_key};
        {ok, Value} ->
            sql_values_from_desc_binmap(Keys,
                                        Desc,
                                        Binmap,
                                        keys_from_desc,
                                        maps:put(Key,
                                                 {sql_retype(maps:get(Key, Desc)),
                                                  [sql_maybe_binary_to_list(Value)]},
                                                 Map))
    end;
sql_values_from_desc_binmap([Key | Keys], Desc, Binmap, keys_from_binmap, Map)
    when Keys == [] ->
    case maps:find(binary_to_list(Key), Desc) of
        error ->
            {error, no_key};
        {ok, _} ->
            maps:put(binary_to_list(Key),
                     {sql_retype(maps:get(binary_to_list(Key), Desc)),
                      [sql_maybe_binary_to_list(maps:get(Key, Binmap))]},
                     Map)
    end;
sql_values_from_desc_binmap([Key | Keys], Desc, Binmap, keys_from_binmap, Map) ->
    case maps:find(binary_to_list(Key), Desc) of
        error ->
            {error, no_key};
        {ok, _} ->
            sql_values_from_desc_binmap(Keys,
                                        Desc,
                                        Binmap,
                                        keys_from_binmap,
                                        maps:put(binary_to_list(Key),
                                                 {sql_retype(maps:get(binary_to_list(Key), Desc)),
                                                  [sql_maybe_binary_to_list(maps:get(Key,
                                                                                     Binmap))]},
                                                 Map))
    end.

crud_controller(M, Ent, {_, _, _, _, Delete}) when ?is_method(M, "DELETE") ->
    case M#mod.data of
        [Ent, Id] ->
            Delete(M#mod.parsed_header, Id);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_, _, Post, _, _}) when ?is_method(M, "POST") ->
    case M#mod.data of
        [Ent] ->
            Post(M#mod.parsed_header, M#mod.entity_body);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_, _, _, Patch, _}) when ?is_method(M, "PATCH") ->
    case M#mod.data of
        [Ent, Id] ->
            Patch(M#mod.parsed_header, Id, M#mod.entity_body);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, _, {_, _, _, _, _}) when ?is_method(M, "PUT") ->
    http_respond_json(501, "Method PUT not implemented");
crud_controller(M, Ent, {Get, Getall, _, _, _}) ->
    case M#mod.data of
        [Ent] ->
            Get(M#mod.parsed_header);
        [Ent, Id] ->
            Getall(M#mod.parsed_header, Id);
        _ ->
            {proceed, M#mod.data}
    end.

table_get(T) ->
    execute_param_query("SELECT * FROM " ++ T, []).

table_get(T, Id) ->
    execute_param_query("SELECT * FROM " ++ T ++ " WHERE id = ? LIMIT 1",
                        [{{sql_varchar, 20}, [Id]}]).

table_insert(T, Cols, Values) ->
    execute_param_query("INSERT INTO "
                        ++ T
                        ++ " ("
                        ++ insert_cols_to_str(Cols)
                        ++ ") VALUES("
                        ++ values_to_str(length(Values))
                        ++ ")",
                        Values).

table_update(T, Id, Cols, Values) ->
    execute_param_query("UPDATE "
                        ++ T
                        ++ " SET "
                        ++ update_cols_to_str(Cols)
                        ++ " WHERE id = ?",
                        Values ++ [{{sql_varchar, 20}, [Id]}]).

table_delete(T, Id) ->
    execute_param_query("DELETE FROM " ++ T ++ " WHERE id = $1", [{sql_integer, [Id]}]).

http_respond_json(Code, Message, Data) ->
    {break,
     [{response,
       {Code,
        json_encode(#{result => Code,
                      message => Message,
                      data => Data})}}]}.

http_respond_json(Code, Message) ->
    {break, [{response, {Code, json_encode(#{result => Code, message => Message})}}]}.

http_respond_json(Code) ->
    {break, [{response, {Code, nobody}}]}.

default_crud(M, T) ->
    util:crud_controller(M,
                         T,
                         {fun(_) ->
                             case table_get(T) of
                                 {error, _} -> http_respond_json(500, 'Error getting data from DB');
                                 Res -> http_respond_json(200, 'Retrieved', result_to_maparr(Res))
                             end
                          end,
                          fun(_, Id) ->
                             case table_get(T, Id) of
                                 {error, _} -> http_respond_json(500, 'Error getting data from DB');
                                 Res -> http_respond_json(200, 'Retrieved', result_to_maparr(Res))
                             end
                          end,
                          fun(Head, Body) ->
                             case maps:get("content_type", maps:from_list(Head), "application/json")
                             of
                                 "application/json" ->
                                     case table_describe(T) of
                                         {error, _} ->
                                             http_respond_json(500,
                                                               'Error getting table description');
                                         Desc ->
                                             case sql_values_from_desc_binmap(maps:remove("id",
                                                                                          maps:from_list(Desc)),
                                                                              json:decode(
                                                                                  unicode:characters_to_binary(Body)))
                                             of
                                                 {error, _} ->
                                                     http_respond_json(422, 'Invalid entity');
                                                 Map ->
                                                     table_insert(T,
                                                                  maps:keys(Map),
                                                                  maps:values(Map)),
                                                     http_respond_json(200, 'Created')
                                             end
                                     end;
                                 _ ->
                                     http_respond_json(415,
                                                       'Content type should be application/json')
                             end
                          end,
                          fun(Head, Id, Body) ->
                             case maps:get("content_type", maps:from_list(Head), "application/json")
                             of
                                 "application/json" ->
                                     case table_describe(T) of
                                         {error, _} ->
                                             http_respond_json(500,
                                                               'Error getting table description');
                                         Desc ->
                                             case sql_values_from_desc_binmap(maps:remove("id",
                                                                                          maps:from_list(Desc)),
                                                                              json:decode(
                                                                                  unicode:characters_to_binary(Body)),
                                                                              keys_from_binmap)
                                             of
                                                 {error, _} ->
                                                     http_respond_json(422, 'Invalid entity');
                                                 Map ->
                                                     table_update(T,
                                                                  Id,
                                                                  maps:keys(Map),
                                                                  maps:values(Map)),
                                                     http_respond_json(200, 'Created')
                                             end
                                     end;
                                 _ ->
                                     http_respond_json(415,
                                                       'Content type should be application/json')
                             end
                          end,
                          fun(_, Id) ->
                             case table_delete(T, Id) of
                                 {error, _} ->
                                     http_respond_json(500, 'Error deleting record from DB');
                                 _ -> http_respond_json(200, 'Deleted')
                             end
                          end}).
