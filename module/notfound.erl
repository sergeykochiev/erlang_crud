-module(notfound).
-export([do/1]).

do(_) -> {break, [{response, {404, "NOT FOUND\n"}}]}.
