-module(main).

-export([start/0, restart/0, initdb/0, stop/0]).

start() ->
    c:c(root),
    c:c(util),
    c:c(person),
    c:c(car),
    c:c(engine),
    odbc:start(),
    inets:start(),
    inets:start(httpd,
                [{port, 3003},
                 {server_name, "dupa1"},
                 {server_root, "/tmp"},
                 {document_root, "/tmp"},
                 {bind_address, "localhost"},
                 {modules, [root, person, notfound]}]).

restart() ->
    stop(),
    start().

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
