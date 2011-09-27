-module(zview_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

for_loop_test() ->
  BlockFun = fun(VarStack) ->
      zview_runtime:resolve(x, VarStack)
  end,
  Result = zview_runtime:call_for({in, [x], [a,b,c]}, BlockFun, {var_stack, [], [], root}),
  ?assertEqual([a,b,c], Result).

for_loop_counter_test() ->
  ?assertEqual([3,3,3], for_loop_counter_helper(<<"length">>)),
  ?assertEqual([true,false,false], for_loop_counter_helper(<<"first">>)),
  ?assertEqual([false,false,true], for_loop_counter_helper(<<"last">>)),
  ?assertEqual([1,2,3], for_loop_counter_helper(<<"index">>)),
  ?assertEqual([0,1,2], for_loop_counter_helper(<<"index0">>)),
  ?assertEqual([2,1,0], for_loop_counter_helper(<<"rindex">>)),
  ?assertEqual([3,2,1], for_loop_counter_helper(<<"rindex0">>)).


for_loop_counter_helper(Var) ->
  BlockFun = fun(VarStack) ->
      zview_runtime:resolve([<<"_for">>, Var], VarStack)
  end,

  zview_runtime:call_for({in, [x], [a,b,c]}, BlockFun, {var_stack, [], [], root}).

