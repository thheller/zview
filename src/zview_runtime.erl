-module(zview_runtime).

-define(LOG, fun(Term) -> io:format("~p:~p - ~p.~n", [?MODULE, ?LINE, Term]) end).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

% Key is expected to be a kvc:kvc_key()
% 'some.path'
% 'simple_atom'
% [<<"binary">>, <<"path">>]
resolve(Key, VarStack) ->
  Value = resolve_it(Key, VarStack),
  Value.

make_var_stack({var_stack, _Context, Vars, _Parent} = Input, undefined) when is_list(Vars) ->
  {ok, Input};

make_var_stack(Input, Context) when is_list(Input) ->
  {ok, {var_stack, Context, Input, root}}.

% TODO: implement filters
apply_filter(_Filter, Value, _Args, _VarStack) ->
  Value.

% TODO: implement tag calling
% TagName = {Mod, Fun}
call_tag(_TagName, _TagArgs, _VarStack) ->
  [].

% for loop for an empty list, does nothing obviously
call_for({in, _, []}, _, _) ->
  [];

call_for({in, VarNames, Var}, Block, VarStack) ->
  iterate_block(VarNames, Var, Block, VarStack, []).

% TODO: actually do something instead of just calling the body
call_block_tag(_TagName, _TagArgs, Block, VarStack) ->
  Block(VarStack).

'eq'(Left, Right) when Left == Right -> true;
'eq'(_Left, _Right) ->
  false.

'and'(true, true) -> true;
'and'(false, true) -> false;
'and'(true, false) -> false;
'and'(Left, Right) ->
  'and'(is_true(Left), is_true(Right)).

is_true(true) -> true;
is_true(false) -> false;
is_true(0) -> false;
is_true([]) -> false;
is_true(undefined) -> false;
is_true(_) -> true.

to_string([]) -> [];
to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(List) when is_list(List) -> List.

make_stack_vars([VarName], Item) -> [{VarName, Item}];
make_stack_vars(_VarNames, _Item) -> throw({cannot_assign_multiple_yet}).

% these should not be exported, maybe dont use export_all 

iterate_block(_VarNames, [], _Block, _VarStack, Results) ->
  lists:reverse(Results);

iterate_block(VarNames, [Item | Rest], Block, VarStack, Results) ->
  BlockStack = {var_stack, none, make_stack_vars(VarNames, Item), VarStack},
  Result = Block(BlockStack),
  iterate_block(VarNames, Rest, Block, VarStack, [Result | Results]).

resolve_it(_Key, root) -> [];

resolve_it([<<"_parent">> | Key], {var_stack, _Context, _Vars, Parent}) ->
  resolve_it(Key, Parent);

resolve_it(Key, {var_stack, _Context, Vars, Parent}) ->
  case kvc:path(Key, Vars) of
    [] ->
      resolve(Key, Parent);
    Value ->
      Value
  end.


-ifdef(TEST).

for_loop_test() ->
  BlockFun = fun(VarStack) ->
      resolve(x, VarStack)
  end,

  Result = call_for({in, [x], [a,b,c]}, BlockFun, {var_stack, [], []}),

  ?assertEqual([a,b,c], Result).

-endif.
