-module(car).

-export([do/1]).

do(M) ->
    util:default_crud(M, "car").
