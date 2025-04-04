-module(root).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

do(M) when M#mod.request_uri == "/" ->
    case file:read_file("../schema/schema.json") of
        {error, _} ->
            {break, [{response, {500, "500 INTERNAL SERVER ERROR"}}]};
        {ok, Bin} ->
            {break, [{response, {200, binary_to_list(Bin)}}]}
    end;
do(M) ->
    [_ | S] = string:split(M#mod.request_uri, "/", all),
    {proceed, S}.
