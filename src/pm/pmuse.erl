%%%---------------------------------------------------------------------
%%% @author Philip Munksgaard <pmunksgaard@gmail.com>
%%% Created : Oct 2012 by Philip Munksgaard <pmunksgaard@gmail.com>
%%%---------------------------------------------------------------------
-module(pmuse).

-export([pmmap/2, treeforall/2]).
-import(pm).

pmmap(F, L) ->
    pmmap(F, L, []).

pmmap(_, [], IVars) ->
    pmmap_receive(IVars, []);
pmmap(F, [X | XS], IVars) ->
    IVar = pm:newVanilla(),
    spawn(fun () -> app(F, X, IVar) end),
    pmmap(F, XS, [IVar | IVars]).

pmmap_receive([], Acc) -> Acc;
pmmap_receive([IVar | IVars], Acc) ->
    pmmap_receive(IVars, [pm:get(IVar) | Acc]).

app(F, X, IVar) ->
    pm:put(IVar, F(X)).

treeforall(T, P) ->
    Self = self(),
    ShortCircuit = pm:newPrincess(fun (X) -> P(X) == false end),
    spawn(fun () -> short(Self, ShortCircuit) end),
    spawn(fun () -> treeforall_aux(Self, T, P, ShortCircuit) end),
    receive
        {short, _} -> false;
        bad -> false;
        ok -> true;
        Unknown -> io:format("Unknown message: ~p~n", [Unknown])
    end.

short(From, ShortCircuit) ->
    From ! {short, pm:get(ShortCircuit)}.

treeforall_aux(From, T, P, ShortCircuit) ->
    IVar = pm:newVanilla(),
    spawn(fun () -> treewalk(T, P, ShortCircuit, IVar) end),
    Result = pm:get(IVar),
    From ! Result.

treewalk(leaf, _, _, IVar) -> pm:put(IVar, ok);
treewalk({node, E, L, R}, P, ShortCircuit, IVar) ->
    case P(E) of
        true ->
            LeftVar = pm:newVanilla(),
            RightVar = pm:newVanilla(),
            spawn(fun () -> treewalk(L, P, ShortCircuit, LeftVar) end),
            spawn(fun () -> treewalk(R, P, ShortCircuit, RightVar) end),
            Left = pm:get(LeftVar),
            Right = pm:get(RightVar),
            if (Left == ok) and (Right == ok) ->
                    pm:put(IVar, ok);
               true -> pm:put(IVar, bad)
            end;
       false ->
            pm:put(ShortCircuit, E),
            pm:put(IVar, bad)
    end.
