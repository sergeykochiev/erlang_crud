-module(util).

-export([crud_controller/3, connect_db/0, default_crud/2]).

-include_lib("inets/include/httpd.hrl").

-define(is_method(M, S), M#mod.method == S).

connect_db() ->
    odbc:connect("Driver=SQLite3;Database=./main.db", []).

% cols_to_string([ Col | Cols ], Acc) when Cols == [] -> Acc++" "++Col;
% cols_to_string([ Col | Cols ], Acc) -> cols_to_string(Cols, Acc++" "++Col).

values_to_string(Len) ->
    values_to_string(Len, "").

values_to_string(Len, Acc) when Len == 0 ->
    Acc ++ " " ++ "?";
values_to_string(Len, Acc) ->
    values_to_string(Len - 1, Acc ++ " " ++ "?").

update_cols_to_string([Col | Cols]) ->
    update_cols_to_string([Col | Cols], "").

update_cols_to_string([Col | Cols], Acc) when Cols == [] ->
    Acc ++ Col ++ " = ?";
update_cols_to_string([Col | Cols], Acc) ->
    update_cols_to_string(Cols, Acc ++ Col ++ " = ?, ").

map_from_list([Key | Keys], V) ->
    map_from_list([Key | Keys], V, 1, #{}).

map_from_list([Key | Keys], V, C, Map) when Keys == [] ->
    maps:put(list_to_atom(Key), element(C, V), Map);
map_from_list([Key | Keys], V, C, Map) ->
    map_from_list(Keys, V, C + 1, maps:put(list_to_atom(Key), element(C, V), Map)).

result_to_maparr({selected, Cols, [Val | Values]}) ->
    result_to_maparr({selected, Cols, [Val | Values]}, []).

result_to_maparr({selected, Cols, [Val | Values]}, Arr) when Values == [] ->
    Arr ++ [map_from_list(Cols, Val)];
result_to_maparr({selected, Cols, [Val | Values]}, Arr) ->
    result_to_maparr({selected, Cols, Values}, Arr ++ [map_from_list(Cols, Val)]).

json_encode(V) ->
    binary_to_list(iolist_to_binary(json:encode(V))).

execute_param_query(Query, P) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            io:put_chars(Query),
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
    case connect_db() of
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

sql_values_from_desc_and_data(Desc, Data) ->
    sql_values_from_desc_and_data(maps:keys(Desc), Desc, Data, none, []).

sql_values_from_desc_and_data(Desc, Data, skip_missing) ->
    sql_values_from_desc_and_data(maps:keys(Desc), Desc, Data, skip_missing, #{}).

sql_values_from_desc_and_data([Key | Keys], Desc, Data, Opt, Map) when Keys == [] ->
    case maps:find(list_to_binary(Key), Data) of
        error ->
            case Opt of
                skip_missing ->
                    Map;
                _ ->
                    {error, no_key}
            end;
        {ok, Value} ->
            maps:put(Key, {maps:get(Key, Desc), [binary_to_list(Value)]}, Map)
    end;
sql_values_from_desc_and_data([Key | Keys], Desc, Data, Opt, Map) ->
    case maps:find(list_to_binary(Key), Data) of
        error ->
            case Opt of
                skip_missing ->
                    sql_values_from_desc_and_data(Keys, Desc, Data, Opt, Map);
                _ ->
                    {error, no_key}
            end;
        {ok, Value} ->
            sql_values_from_desc_and_data(Keys,
                                          Desc,
                                          Data,
                                          Opt,
                                          maps:put(Key,
                                                   {maps:get(Key, Desc), [binary_to_list(Value)]},
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

table_insert(T, Values) ->
    execute_param_query("INSERT INTO "
                        ++ T
                        ++ " values ("
                        ++ values_to_string(length(Values))
                        ++ ")",
                        Values).

table_update(T, Id, Cols, Values) ->
    execute_param_query("UPDATE"
                        ++ T
                        ++ " SET "
                        ++ update_cols_to_string(Cols)
                        ++ " WHERE id = ?",
                        Values ++ [{{sql_varchar, 20}, [Id]}]).

table_delete(T, Id) ->
    execute_param_query("DELETE FROM " ++ T ++ " WHERE id = $1", [{sql_integer, [Id]}]).

default_crud(M, T) ->
    util:crud_controller(M,
                         T,
                         {fun(_) ->
                             {break,
                              [{response,
                                {200,
                                 json_encode(#{result => "200 RETRIEVED",
                                               data => result_to_maparr(table_get(T))})}}]}
                          end,
                          fun(_, Id) ->
                             {break,
                              [{response,
                                {200,
                                 json_encode(#{result => "200 RETRIEVED",
                                               data => result_to_maparr(table_get(T, Id))})}}]}
                          end,
                          fun(Head, Body) ->
                             case maps:get("content_type", maps:from_list(Head), "application/json")
                             of
                                 "application/json" ->
                                     case table_describe(T) of
                                         {error, _} -> {break, [{response, {200, ""}}]};
                                         Desc ->
                                             case sql_values_from_desc_and_data(maps:remove("id",
                                                                                            maps:from_list(Desc)),
                                                                                json:decode(
                                                                                    unicode:characters_to_binary(Body)))
                                             of
                                                 {error, _} ->
                                                     {break,
                                                      [{response,
                                                        {422, "422 UNPROCESSIBLE ENTITY"}}]};
                                                 Values ->
                                                     table_insert(T, maps:values(Values)),
                                                     {break, [{response, {200, ""}}]}
                                             end
                                     end;
                                 _ -> {break, [{response, {415, "415 UNSUPPORTED MEDIA TYPE"}}]}
                             end
                          end,
                          fun(Head, Id, Body) ->
                             case maps:get("content_type", maps:from_list(Head), "application/json")
                             of
                                 "application/json" ->
                                     case table_describe(T) of
                                         {error, _} -> {break, [{response, {200, ""}}]};
                                         Desc ->
                                             case sql_values_from_desc_and_data(maps:remove("id",
                                                                                            maps:from_list(Desc)),
                                                                                json:decode(
                                                                                    unicode:characters_to_binary(Body)),
                                                                                skip_missing)
                                             of
                                                 #{} ->
                                                     {break,
                                                      [{response,
                                                        {422, "422 UNPROCESSIBLE ENTITY"}}]};
                                                 Map ->
                                                     table_update(T,
                                                                  Id,
                                                                  maps:keys(Map),
                                                                  maps:values(Map)),
                                                     {break, [{response, {200, ""}}]}
                                             end
                                     end;
                                 _ -> {break, [{response, {415, "415 UNSUPPORTED MEDIA TYPE"}}]}
                             end
                          end,
                          fun(_, Id) ->
                             case table_delete(T, Id) of
                                 {error, _} ->
                                     {break, [{response, {500, "500 INTERNAL SERVER ERROR"}}]};
                                 _ -> {break, [{response, {200, "200 `DELETED"}}]}
                             end
                          end}).
