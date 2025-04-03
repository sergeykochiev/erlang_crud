-module(main).

-export([start/0, do/1, initdb/0, stop/0]).

-include_lib("inets/include/httpd.hrl").

start() ->
    odbc:start(),
    inets:start(),
    inets:start(httpd,
                [{port, 3003},
                 {server_name, "dupa"},
                 {server_root, "/tmp"},
                 {document_root, "/tmp"},
                 {bind_address, "localhost"},
                 {modules, [main, person, notfound]}]).

stop() ->
    odbc:stop(),
    inets:stop().

initdb() ->
    {ok, ConRef} = util:connect_db(),
    {odbc:sql_query(ConRef,
                    "CREATE TABLE person (id INTEGER PRIMARY KEY AUTOINCREMENT, "
                    "name TEXT NOT NULL, surname TEXT NOT NULL, age INTEGER NOT "
                    "NULL, car_id INTEGER NOT NULL, FOREIGN KEY (car_id) REFERENCES "
                    "person (id)); CREATE TABLE car (id INTEGER PRIMARY KEY AUTOINCREMENT"
                    ", brand TEXT NOT NULL, model TEXT NOT NULL, year INTEGER NOT "
                    "NULL, mileage INTEGER NOT NULL, engine_id INTEGER NOT NULL, "
                    "FOREIGN KEY (engine_id) REFERENCES person (id)); CREATE TABLE "
                    "engine (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, codename "
                    "TEXT DEFAULT NULL, litre_capacity REAL NOT NULL, piston_count "
                    "INTEGER NOT NULL, shape TEXT NOT NULL, horsepower INTEGER NOT "
                    "NULL)"),
     odbc:disconnect(ConRef)}.

do(M) when M#mod.request_uri == "/" ->
    {break, [{response, {404, ""}}]};
do(M)
    when M#mod.method == "GET"
         orelse M#mod.method == "DELETE"
         orelse M#mod.method == "PATCH"
         orelse M#mod.method == "POST" ->
    [_ | S] = string:split(M#mod.request_uri, "/", all),
    {proceed, S};
do(_) ->
    {break, [{response, {501, "Method not implemented\n"}}]}.
