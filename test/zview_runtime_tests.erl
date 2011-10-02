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
  for_loop_helper([<<"$">>, <<"loop">>, Var]).

for_loop_counter_test() ->
  ?assertEqual([3,3,3], for_loop_counter_helper(<<"length">>)),
  ?assertEqual([true,false,false], for_loop_counter_helper(<<"first">>)),
  ?assertEqual([false,false,true], for_loop_counter_helper(<<"last">>)),
  ?assertEqual([1,2,3], for_loop_counter_helper(<<"index">>)),
  ?assertEqual([0,1,2], for_loop_counter_helper(<<"index0">>)),
  ?assertEqual([2,1,0], for_loop_counter_helper(<<"rindex">>)),
  ?assertEqual([3,2,1], for_loop_counter_helper(<<"rindex0">>)).

boolean_op_test() ->
  ?assertEqual(true, zview_runtime:boolean_op('true', {[{value, "blubb"}]}, true)).

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
  % TODO: decide how i want to deal with badarg (trying to convert list_to_integer)
  % ?assertEqual(false, zview_runtime:equals(1, "b")),
  ?assertEqual(false, zview_runtime:equals("a", "b")),
  ?assertEqual(false, zview_runtime:equals(<<"abc">>, ["x", <<"y">>, "z"])),
  ok.

var_stack_test() ->
  Options = [
    {custom_tags, [{test, zview_test_tags}]}
  ],

  Vars = [ ],

  % what exactly do i want to test here?
  {var_stack, {root, Context}, Vars, root} = VarStack = zview_runtime:init_var_stack(Options, Vars),

  {ok, zview_test_tags} = zview_runtime:find_tag_alias(test, VarStack),
  {ok, Context} = zview_runtime:get_context(root, VarStack),

  not_found = zview_runtime:get_context(dummy, VarStack),

  DummyStack = zview_runtime:push_var_stack([], dummy, blubb, VarStack),
  {ok, blubb} = zview_runtime:get_context(dummy, DummyStack),

  ok.
