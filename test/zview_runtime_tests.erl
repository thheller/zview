-module(zview_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

for_loop_helper(Var) ->
  BlockFun = fun(VarStack) ->
      zview_runtime:resolve(Var, VarStack)
  end,
  zview_runtime:call_for({in, [x], [a,b,c]}, BlockFun, {var_stack, [], [], root}).

for_loop_test() ->
  ?assertEqual([a,b,c], for_loop_helper(x)).

for_loop_counter_helper(Var) ->
  for_loop_helper([<<"_for">>, Var]).

for_loop_counter_test() ->
  ?assertEqual([3,3,3], for_loop_counter_helper(<<"length">>)),
  ?assertEqual([true,false,false], for_loop_counter_helper(<<"first">>)),
  ?assertEqual([false,false,true], for_loop_counter_helper(<<"last">>)),
  ?assertEqual([1,2,3], for_loop_counter_helper(<<"index">>)),
  ?assertEqual([0,1,2], for_loop_counter_helper(<<"index0">>)),
  ?assertEqual([2,1,0], for_loop_counter_helper(<<"rindex">>)),
  ?assertEqual([3,2,1], for_loop_counter_helper(<<"rindex0">>)).


equals_test() ->
  ?assertEqual(true, zview_runtime:equals("a", "a")),
  ?assertEqual(true, zview_runtime:equals("a", <<"a">>)),
  ?assertEqual(true, zview_runtime:equals(<<"a">>, <<"a">>)),
  ?assertEqual(true, zview_runtime:equals(<<"a">>, "a")),
  ?assertEqual(true, zview_runtime:equals([<<"a">>, "bc"], <<"abc">>)),
  ?assertEqual(true, zview_runtime:equals(1, 1)),
  ?assertEqual(true, zview_runtime:equals(1, "1")),
  ?assertEqual(true, zview_runtime:equals("1", 1)),
  ?assertEqual(true, zview_runtime:equals(<<"1">>, 1)),
  ?assertEqual(true, zview_runtime:equals(1, <<"1">>)),
  ?assertEqual(true, zview_runtime:equals(<<"test">>, test)),
  ?assertEqual(true, zview_runtime:equals(test, <<"test">>)),
  ?assertEqual(false, zview_runtime:equals(1, 2)),
  ?assertEqual(false, zview_runtime:equals("2", 1)),
  ?assertEqual(false, zview_runtime:equals(1, "2")),
  % TODO: decide how i want to deal with badarg
  % ?assertEqual(false, zview_runtime:equals(1, "b")),
  ?assertEqual(false, zview_runtime:equals("a", "b")),
  ?assertEqual(false, zview_runtime:equals(<<"abc">>, ["x", <<"y">>, "z"])),
  ok.
