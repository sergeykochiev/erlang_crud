-module(person).
-export([do/1]).

do(M) ->
    util:crud_controller(M, "person", {
        fun() -> {break, [{response, {200, "GET person"}}]} end,
        fun(Id) -> {break, [{response, {200, "GET person with id " ++ Id}}]} end,
        fun(_B) -> {break, [{response, {200, "POST person"}}]} end,
        fun(Id) -> {break, [{response, {200, "PATCH person with id " ++ Id}}]} end,
        fun(Id) -> {break, [{response, {200, "DELETE person with id " ++ Id}}]} end
    }).
