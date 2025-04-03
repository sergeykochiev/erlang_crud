-module(root).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

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
