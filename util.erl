-module(util).
-export([crud_controller/3]).
-define(is_method(M, S), (M#mod.method == S)).
-include_lib("inets/include/httpd.hrl").

crud_controller(M, Ent, {_,_,_,_,Delete}) when ?is_method(M, "DELETE") ->
    case M#mod.data of
        [Ent, _Id] -> Delete(_Id);
        _ -> {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_,_,Post,_,_}) when ?is_method(M, "POST") ->
    case M#mod.data of
        [Ent] -> Post(M#mod.entity_body);
        _ -> {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {_,_,_,Patch,_}) when ?is_method(M, "PATCH") ->
    case M#mod.data of
        [Ent, _Id] -> Patch(M#mod.entity_body);
        _ -> {proceed, M#mod.data}
    end;
crud_controller(M, Ent, {Get,Getall,_,_,_}) ->
    case M#mod.data of
        [Ent] -> Get();
        [Ent, _Id] -> Getall(_Id);
        _ -> {proceed, M#mod.data}
    end.
