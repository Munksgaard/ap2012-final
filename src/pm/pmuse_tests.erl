-module(pmuse_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

pmuse_pmmap_test() ->
    ?assertEqual(pmuse:pmmap(fun (X) -> X + 1 end, []), []),
    ?assertEqual(pmuse:pmmap(fun (X) -> X + 1 end, [1,2,3,4]), [2,3,4,5]),
    ?assertEqual(pmuse:pmmap(fun (X) -> X + 2 end, "hej"), "jgl").

pmuse_treeforall_test() ->
    ?assert(pmuse:treeforall({node, 2, {node, 4, leaf, leaf}, {node, 2, {node, 16, leaf, leaf}, {node, 8, leaf, leaf}}}, fun (X) -> X rem 2 == 0 end)),
    ?assert(pmuse:treeforall(leaf, fun (X) -> X rem 2 == 0 end)),
    ?assert(pmuse:treeforall({node, 2, leaf, leaf}, fun (X) -> X rem 2 == 0 end)),
    ?assertNot(pmuse:treeforall({node, 3, leaf, leaf}, fun (X) -> X rem 2 == 0 end)),
    ?assertNot(pmuse:treeforall({node, 2, {node, 4, leaf, leaf}, {node, 2, {node, 3, leaf, leaf}, leaf}}, fun (X) -> X rem 2 == 0 end)),
    ?assertNot(pmuse:treeforall({node, 2, {node, 4, leaf, leaf}, {node, 2, {node, 3, leaf, leaf}, {node, 8, leaf, leaf}}}, fun (X) -> X rem 2 == 0 end)).
