-module(root).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

do(M) when M#mod.request_uri == "/" ->
    {break, [{response, {404, ""}}]};
do(M) ->
    [_ | S] = string:split(M#mod.request_uri, "/", all),
    {proceed, S}.
