-module(main).

-export([start/0, restart/0, initdb/0, stop/0]).

start() ->
    c:c(root),
    c:c(util),
    c:c(person),
    c:c(car),
    c:c(engine),
    c:c(notfound),
    odbc:start(),
    inets:start(),
    inets:start(httpd,
                [{port, 3003},
                 {server_name, "erlang_crud"},
                 {server_root, "/tmp"},
                 {document_root, "/tmp"},
                 {bind_address, "localhost"},
                 {modules, [root, person, car, engine, notfound]}]).

restart() ->
    stop(),
    start().

stop() ->
    odbc:stop(),
    inets:stop().

initdb() ->
    {ok, ConRef} = util:connect_main_db(),
    {ok, Bin} = file:read_file("../schema/schema.sql"),
    {odbc:sql_query(ConRef, binary_to_list(Bin)), odbc:disconnect(ConRef)}.
