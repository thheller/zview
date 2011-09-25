-module(zview_runtime).


-define(LOG, fun(Term) -> io:format("~p:~p - ~p.~n", [?MODULE, ?LINE, Term]) end).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

resolve(Key, VarStack) ->
  Value = resolve_it(Key, VarStack),
  ?debugVal({resolve, Key, Value, VarStack}),
  Value.

resolve_it(Key, []) -> [];

resolve_it([<<"_parent">> | Key], [{stack, _} | VarStack]) ->
  resolve_it(Key, VarStack);

resolve_it(Key, [{stack, Vars} | VarStack]) ->
  case kvc:path(Key, Vars) of
    [] ->
      resolve(Key, VarStack);
    Value ->
      Value
  end.

make_var_stack([{stack, [List]} | _] = Input) when is_list(List) ->
  {ok, Input};

make_var_stack(Input) when is_list(Input) ->
  {ok, [{stack, Input}]}.

call_tag(TagName, TagArgs, VarStack) ->
  ?debugVal({tag, TagName, TagArgs, VarStack}),
  ["TagResult: ", atom_to_list(TagName)].

call_block_tag({for, _, []}, _, _) ->
  [];

call_block_tag({for, VarNames, Var}, Block, VarStack) ->
  ?debugVal({for, VarNames, Var}),
  iterate_block(VarNames, Var, Block, VarStack, []);

call_block_tag(Tag, Block, VarStack) ->
  Block(VarStack).

iterate_block(VarNames, [], Block, VarStack, Results) ->
  lists:reverse(Results);

iterate_block(VarNames, [Item | Rest], Block, VarStack, Results) ->
  BlockStack = [ {stack, make_stack_vars(VarNames, Item)} | VarStack ],
  Result = Block(BlockStack),
  iterate_block(VarNames, Rest, Block, VarStack, [Result | Results]).

apply_filter(Filter, Value, Args, VarStack) ->
  ?debugVal({filer, Filter, Args}),
  Value.

'eq'(Left, Right) ->
  true.

'and'(Left, Right) ->
  true.

is_true(Test) ->
  true.

to_string([]) -> [];
to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(List) when is_list(List) -> List.


make_stack_vars([VarName], Item) -> [{VarName, Item}];
make_stack_vars(VarNames, Item) -> throw({cannot_assign_multiple_yet}).


-ifdef(TEST).

for_loop_test() ->
  BlockFun = fun(VarStack) ->
      resolve(x, VarStack)
  end,

  Result = call_block_tag({for, [x], [a,b,c]}, BlockFun, [{stack, []}]),

  ?assertEqual([a,b,c], Result).

-endif.
