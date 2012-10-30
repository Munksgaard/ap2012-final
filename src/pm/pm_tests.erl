-module(pm_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%% Functions for testing that pm:get gets the result eventually,
%%% even if the IVar is not set when first called

getPut(IVar, Val) ->
    X = self(),
    spawn(fun () -> getter(X, IVar) end),
    pm:put(IVar, Val),
    receive
        {got, T} -> T;
        Unknown -> io:format("Unknown ~p~n",[Unknown])
    end.

getter(From, IVar) ->
    From ! {got, pm:get(IVar)}.

%%% Vanilla tests

pm_newVanilla_1_test() -> pm:newVanilla().

pm_putGetVanilla_test() ->
    IVar = pm:newVanilla(),
    pm:put(IVar, test_value),
    ?assertEqual(pm:get(IVar), test_value),
    ?assertEqual(pm:compromised(IVar), false).

pm_getPutVanilla_test() ->
    IVar = pm:newVanilla(),
    ?assertEqual(getPut(IVar, test_value), test_value),
    ?assertEqual(pm:compromised(IVar), false).

pm_vanillaCompromised_1_test() ->
    IVar = pm:newVanilla(),
    ?assertEqual(pm:compromised(IVar), false).

pm_vanillaCompromised_2_test() ->
    IVar = pm:newVanilla(),
    pm:put(IVar, some_value),
    ?assertEqual(pm:compromised(IVar), false).

pm_vanillaCompromised_3_test() ->
    IVar = pm:newVanilla(),
    pm:put(IVar, some_value),
    pm:put(IVar, another_value),
    ?assertEqual(pm:compromised(IVar), true),
    ?assertEqual(pm:get(IVar), some_value).

%%% Princess tests

pm_newPrincess_1_test() -> pm:newPrincess(fun (_) -> true end).
pm_newPrincess_2_test() -> pm:newPrincess(fun (_) -> false end).

pm_putGetPrincess_1_test() ->
    IVar = pm:newPrincess(fun (X) -> X rem 2 == 0 end),
    pm:put(IVar, 4),
    ?assertEqual(pm:get(IVar), 4),
    ?assertEqual(pm:compromised(IVar), false).

pm_putGetPrincess_2_test() ->
    IVar = pm:newPrincess(fun (X) -> X rem 2 == 0 end),
    pm:put(IVar, 3),
    pm:put(IVar, 4),
    ?assertEqual(pm:get(IVar), 4),
    ?assertEqual(pm:compromised(IVar), false).

pm_putGetPrincess_3_test() ->
    IVar = pm:newPrincess(fun (X) -> X rem 2 == 0 end),
    pm:put(IVar, 4),
    pm:put(IVar, 2),
    ?assertEqual(pm:get(IVar), 4),
    ?assertEqual(pm:compromised(IVar), false).

pm_getPutPrincess_test() ->
    IVar = pm:newPrincess(fun (X) -> X rem 2 == 0 end),
    ?assertEqual(getPut(IVar, 4), 4),
    ?assertEqual(pm:compromised(IVar), false).

pm_princessRobust_1_test() ->
    IVar = pm:newPrincess(fun (X) -> 2 / X == 1 end),
    pm:put(IVar, 0),
    pm:put(IVar, 2),
    ?assertEqual(pm:get(IVar), 2).
