-module(util).

-export([crud_controller/3, connect_db/0, table_get/1, table_get/2, table_insert/2,
         table_update/3, table_delete/2]).

-include_lib("inets/include/httpd.hrl").

-define(is_method(M, S), M#mod.method == S).

connect_db() ->
    odbc:connect("Driver=SQLite3;Database=./main.db", []).

% cols_to_string([ Col | Cols ], Acc) when Cols == [] -> Acc++" "++Col;
% cols_to_string([ Col | Cols ], Acc) -> cols_to_string(Cols, Acc++" "++Col).

values_to_string(Len, Acc) when Len == 0 ->
    Acc ++ " " ++ "?";
values_to_string(Len, Acc) ->
    values_to_string(Len - 1, Acc ++ " " ++ "?").

update_cols_to_string([Col | Cols], Acc) when Cols == [] ->
    Acc ++ Col ++ " = ?";
update_cols_to_string([Col | Cols], Acc) ->
    update_cols_to_string(Cols, Acc ++ Col ++ " = ?, ").

crud_controller(M, Ent, {_, _, _, _, Delete}) when ?is_method(M, "DELETE") ->
    case M#mod.data of
        [Ent, _Id] ->
            Delete(_Id);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_, _, Post, _, _}) when ?is_method(M, "POST") ->
    case M#mod.data of
        [Ent] ->
            Post(M#mod.entity_body);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_, _, _, Patch, _}) when ?is_method(M, "PATCH") ->
    case M#mod.data of
        [Ent, _Id] ->
            Patch(M#mod.entity_body);
        _ ->
            {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {Get, Getall, _, _, _}) ->
    case M#mod.data of
        [Ent] ->
            Get();
        [Ent, _Id] ->
            Getall(_Id);
        _ ->
            {proceed, M#mod.data}
    end.

table_get(T) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            Query = "SELECT * FROM " ++ T,
            dbc:sql_query(Cr, Query),
            odbc:disconnect()
    end.

table_get(T, Id) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            Query = "SELECT * FROM " ++ T ++ " WHERE id = $1 LIMIT 1",
            R = odbc:param_query(Cr, Query, [{sql_integer, [Id]}]),
            odbc:disconnect(),
            R
    end.

table_insert(T, Values) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            Query =
                "INSERT INTO " ++ T ++ " values (" ++ values_to_string(length(Values), "") ++ ")",
            R = odbc:param_query(Cr, Query, Values),
            odbc:disconnect(),
            R
    end.

table_update(T, Cols, Values) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            Query = "UPDATE" ++ T ++ " SET " ++ update_cols_to_string(Cols, ""),
            R = odbc:param_query(Cr, Query, Values),
            odbc:disconnect(),
            R
    end.

table_delete(T, Id) ->
    case connect_db() of
        {error, E} ->
            {error, E};
        {ok, Cr} ->
            Query = "DELETE FROM " ++ T ++ " WHERE id = $1",
            R = odbc:param_query(Cr, Query, [{sql_integer, [Id]}]),
            odbc:disconnect(),
            R
    end.
