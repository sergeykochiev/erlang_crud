-module(notfound).

-export([do/1]).

do(_) ->
    util:http_respond_json(404).
