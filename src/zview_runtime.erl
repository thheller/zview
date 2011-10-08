-module(zview_runtime).

-define(LOG, fun(Term) -> io:format("~p:~p - ~p.~n", [?MODULE, ?LINE, Term]) end).

% public functions
-export([
    init_var_stack/2,
    resolve/2
  ]).

% functions intended to be called by templates mostly
-export([
    get_context/2,
    find_tag_alias/2,
    validate_var_stack/1,
    push_var_stack/2,
    push_var_stack/4,
    call_tag/4,
    call_block_tag/5,
    apply_filter/5,
    call_for/3,
    boolean_op/3,
    to_string/1,
    is_true/1,
    equals/2
  ]).

-include_lib("eunit/include/eunit.hrl").

-record(root_context, {custom_tags = []}).
-record(for_counter, {length, current}).

% Key is expected to be a kvc:kvc_key()
% 'some.path'
% 'simple_atom'
% [<<"binary">>, <<"path">>]
resolve(Key, VarStack) ->
  Value = resolve_it(Key, VarStack),
  Value.

init_var_stack(Context, List) when is_list(List) ->
  CustomTags = proplists:get_value(custom_tags, Context, []),
  {var_stack, {root, #root_context{custom_tags = CustomTags}}, List, root}.

validate_var_stack({var_stack, Context, Vars, root} = Input) when is_list(Vars) andalso is_record(Context, root_context) ->
  {ok, Input};
validate_var_stack({var_stack, _Context, Vars, _Parent} = Input) when is_list(Vars) ->
  {ok, Input};

validate_var_stack(_) ->
  invalid_var_stack.

push_var_stack(Vars, {var_stack, _, _, _} = Parent) ->
  {var_stack, undefined, Vars, Parent}.

push_var_stack(Vars, Id, Context, {var_stack, _, _, _} = Parent) ->
  {var_stack, {Id, Context}, Vars, Parent}.

apply_filter(_CallingTemplate, {FilterAlias, FilterName}, Value, Args, VarStack) ->
  case find_tag_alias(FilterAlias, VarStack) of
    {ok, TagModule} ->
      case TagModule:zview_filter(FilterName, Value, Args, VarStack) of
        {value, NewValue} ->
          NewValue;

        invalid_filter ->
          % TODO: error report this
          io_lib:format("invalid filter call: {~p,~p} in ~p", [FilterAlias, FilterName, TagModule])
      end;

    not_found ->
      % TODO: warn somehow, but dont fail
      Value

  end.


call_tag(CallingTemplate, {TagAlias, TagName}, TagArgs, VarStack) ->
  case find_tag_alias(TagAlias, VarStack) of
    {ok, TagModule} ->
      case TagModule:zview_tag(TagName, TagArgs, VarStack) of
        {output, Body} ->
          Body;

        invalid_tag ->
          % TODO: error report this
          io_lib:format("invalid tag call: {~p,~p} in ~p", [TagAlias, TagName, TagModule])
      end;

    not_found ->
      % TODO: error report this
      io_lib:format("invalid tag alias: {~p,~p}", [TagAlias, TagName])
  end.


call_block_tag(CallingTemplate, {TagAlias, TagName}, TagArgs, Block, VarStack) ->
  case find_tag_alias(TagAlias, VarStack) of
    {ok, TagModule} ->
      case TagModule:zview_block(TagName, TagArgs, Block, VarStack) of
        {output, Body} ->
          Body;

        invalid_tag ->
          % TODO: error report this
          io_lib:format("invalid block tag call: {~p,~p} in ~p", [TagAlias, TagName, TagModule])
      end;

    not_found ->
      % TODO: error report this
      io_lib:format("invalid tag alias: {~p,~p}", [TagAlias, TagName])
  end.

% for loop for an empty list, does nothing obviously
call_for({in, _, []}, _, _) -> [];
call_for({in, VarNames, Var}, Block, VarStack) ->
  ForState = make_for_counter(Var),
  iterate_block(ForState, VarNames, Var, Block, VarStack, []).

boolean_op('eq', Left, Right) ->
  try
    equals(Left, Right)
  catch
    badarg ->
      false
  end;

boolean_op('and', Left, Right) ->
  'and'(Left, Right);

% TODO: implement all boolean ops, 
boolean_op(Op, Left, Right) ->
  ?debugVal(Op),
  ?debugVal(Left),
  ?debugVal(Right),
  true.

is_true(true) -> true;
is_true(false) -> false;
is_true(0) -> false;
is_true([]) -> false;
is_true(undefined) -> false;
is_true(_) -> true.

to_string([]) -> [];
to_string(Bin) when is_binary(Bin) -> Bin;
to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
to_string(List) when is_list(List) ->
  erlang:iolist_to_binary(List).

% TODO: this will most likely fail for unicode comparisions
equals(Left, Right) when Left =:= Right ->
  true;
equals(Left, Right) when is_atom(Left) ->
  equals(atom_to_list(Left), Right);
equals(Left, Right) when is_atom(Right) ->
  equals(Left, atom_to_list(Right));
equals(Left, Right) when is_binary(Left) andalso is_list(Right) ->
  equals(Left, iolist_to_binary(Right));
equals(Left, Right) when is_binary(Right) andalso is_list(Left) ->
  equals(iolist_to_binary(Left), Right);
equals(Left, Right) when is_integer(Left) and not is_integer(Right)->
  equals(Left, convert_to_int(Right));
equals(Left, Right) when is_integer(Right) and not is_integer(Left) ->
  equals(convert_to_int(Left), Right);
equals(_Left, _Right) ->
  false.

convert_to_int(Bin) when is_binary(Bin) -> convert_to_int(binary_to_list(Bin));
convert_to_int(List) when is_list(List) -> list_to_integer(List).

%%% --- INTERNAL METHODS ---

'and'(true, true) -> true;
'and'(false, _) -> false;
'and'(_, false) -> false;
'and'(Left, Right) ->
  'and'(is_true(Left), is_true(Right)).

find_tag_alias(default, _VarStack) -> {ok, zview_default_tags};
find_tag_alias(Alias, VarStack) ->
  {ok, Root} = get_context(root, VarStack),
  case proplists:get_value(Alias, Root#root_context.custom_tags, undefined) of
    undefined ->
      not_found;

    Found ->
      {ok, Found}
  end.

get_context(Id, root) -> not_found;
get_context(Id, {var_stack, {Id, Context}, _Vars, Parent}) -> {ok, Context};
get_context(Id, {var_stack, _, _, Parent}) -> get_context(Id, Parent).

make_stack_vars([VarName], Item) -> [{VarName, Item}];
make_stack_vars([N1, N2], {V1, V2}) -> [{N1, V1}, {N2, V2}];
make_stack_vars(_VarNames, _Item) -> throw(cannot_assign_multiple_yet).

make_for_counter(Val) when is_list(Val) ->
  #for_counter{length = length(Val), current = 0}.

iterate_block(_ForState, _VarNames, [], _Block, _VarStack, Results) ->
  lists:reverse(Results);

iterate_block(#for_counter{current = Current } = ForState, VarNames, [Item | Rest], Block, VarStack, Results) ->
  BlockStack = {var_stack, ForState, make_stack_vars(VarNames, Item), VarStack},
  Result = Block(BlockStack),
  NewForState = ForState#for_counter{current = Current + 1},
  iterate_block(NewForState, VarNames, Rest, Block, VarStack, [Result | Results]).

resolve_it(_Key, root) -> [];

resolve_it([<<"$">> | Key], VarStack) ->
  resolve_context_variable(Key, VarStack);

resolve_it(Key, {var_stack, _Context, Vars, Parent}) ->
  case kvc:path(Key, Vars) of
    [] ->
      resolve(Key, Parent);
    Value ->
      Value
  end.

resolve_context_variable([<<"vars">>], VarStack) ->
  collect_var_stack_vars(VarStack, []);
  
resolve_context_variable([<<"context">>, <<"vars">>], {var_stack, _Context, Vars, _Parent}) ->
  Vars;

resolve_context_variable([<<"loop">>, VarName], VarStack)->
  ForState = find_for_context(VarStack),
  resolve_for_counter(VarName, ForState);

resolve_context_variable([<<"parent">> | Key], {var_stack, _Context, _Vars, Parent}) ->
  resolve(Key, Parent);

resolve_context_variable(Key, VarStack) ->
  [].

collect_var_stack_vars(root, Result) ->
  lists:flatten(lists:reverse(Result));

collect_var_stack_vars({var_stack, _Context, Vars, Parent}, Result) ->
  collect_var_stack_vars(Parent, [Vars | Result]).

find_for_context(root) -> throw(not_inside_for_loop);
find_for_context({var_stack, Context, _Vars, _Parent}) when is_record(Context, for_counter) ->
  Context;

find_for_context({var_stack, _, _, Parent}) ->
  find_for_context(Parent).

resolve_for_counter(<<"length">>, #for_counter{length = Len}) ->
  Len;
resolve_for_counter(<<"index">>, #for_counter{current = Current}) ->
  Current + 1;
resolve_for_counter(<<"index0">>, #for_counter{current = Current}) ->
  Current;
resolve_for_counter(<<"rindex">>, #for_counter{current = Current, length = Len}) ->
  Len - (Current + 1);
resolve_for_counter(<<"rindex0">>, #for_counter{current = Current, length = Len}) ->
  Len - Current;
resolve_for_counter(<<"first">>, #for_counter{current = Current}) ->
  Current == 0;
resolve_for_counter(<<"last">>, #for_counter{current = Current, length = Length}) ->
  Current == Length - 1;

resolve_for_counter(Unknown, ForState) ->
  throw({unknown_for_variable, Unknown, ForState}).

