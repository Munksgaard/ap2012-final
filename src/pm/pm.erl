%%%---------------------------------------------------------------------
%%% @author Philip Munksgaard <pmunksgaard@gmail.com>
%%% Created : Oct 2012 by Philip Munksgaard <pmunksgaard@gmail.com>
%%%---------------------------------------------------------------------
-module(pm).

-export([newVanilla/0, newPrincess/1, get/1, put/2, compromised/1, test/1]).

%%%% Interface

newVanilla() ->
    spawn(fun vanilla_loop/0).

newPrincess(P) ->
    spawn(fun () -> princess_loop(P) end).

get(V) ->
    Reply = rpc(V, get),
    case Reply of
        notset -> pm:get(V);
        {ok, T} -> T;
        Unknown -> io:format("Unknown message ~p~n", [Unknown])
    end.

put(V, T) ->
    V ! {put, T}.

compromised(V) ->
    {ok, Reply} = rpc(V, compromised),
    Reply.

test(IVar) ->
    X = self(),
    spawn(fun () -> getter(X, IVar) end),
    pm:put(IVar, 3),
    receive
        {got, T} -> T;
        Unknown -> io:format("Unknown ~p~n",[Unknown])
    end.

getter(From, IVar) ->
    From ! {got, pm:get(IVar)}.

%% Synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

%%% Vanilla flavour

vanilla_loop() ->
    receive
        {put, T} ->
            vanilla_loop(T, false);
        {From, get} ->
            reply(From, notset),
            vanilla_loop();
        {From, compromised} ->
            reply_ok(From, false),
            vanilla_loop();
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            vanilla_loop()
    end.

vanilla_loop(T, Compromised) ->
    receive
        {From, get} ->
            reply_ok(From, T),
            vanilla_loop(T, Compromised);
        {put, _} -> vanilla_loop(T, true);
        {From, compromised} ->
            reply_ok(From, Compromised),
            vanilla_loop(T, Compromised);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            vanilla_loop(T, Compromised)
    end.

%%% Princess Flavour

princess_loop(P) ->
    receive
        {From, get} ->
            reply(From, notset),
            princess_loop(P);
        {put, T} -> 
            case catch (P(T)) of
                true -> princess_loop(set, T);
                _ -> princess_loop(P)
            end;
        {From, compromised} ->
            reply_ok(From, false),
            princess_loop(P)
    end.

princess_loop(set, T) ->
    receive
        {From, get} ->
            reply_ok(From, T),
            princess_loop(set, T);
        {put, _} -> princess_loop(set, T);
        {From, compromised} ->
            reply_ok(From, false),
            princess_loop(set, T)
    end.
