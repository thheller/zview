-module(new_compiler).

-export([to_source/2, compile/2]).

-include_lib("eunit/include/eunit.hrl").

-define(C, erl_syntax).
-define(MFA, fun(Mod, Fun, Args) ->
    ?C:application(
      ?C:atom(Mod),
      ?C:atom(Fun),
      Args
  ) end).

-define(PP, fun(Arg) ->
      erl_prettypr:format(Arg, [{paper, 240}])
  end).

-record(tpl_mod, {fun_idx = 0, funs = []}).


to_source(ParseTree, TargetModule) ->
  {ast, _, Sources} = to_ast(ParseTree, TargetModule),

  Body = string:join([ ?PP(Source) || Source <- Sources ], "\n\n"),
  {source, TargetModule, Body}.

to_beam({ast, TargetModule, Sources}) ->
  Forms = [ ?C:revert(X) || X <- Sources ],
  {ok, TargetModule, Binary} = compile:forms(Forms, [debug_info, verbose, report_errors]),
  {ok, Binary}.


to_module({ast, TargetModule, Sources} = Ast) ->
  {ok, ModuleBinary} = to_beam(Ast),

  code:purge(TargetModule),
  {module, _} = code:load_binary(TargetModule, atom_to_list(TargetModule) ++ ".erl", ModuleBinary),

  {ok, TargetModule}.

compile(Bin, TargetModule) ->
  {ok, ParseTree} = erlydtl_compiler:parse(Bin),
  Ast = to_ast(ParseTree, TargetModule),
  to_module(Ast).

tpl_function(FunName, FunBody) ->
  ?C:function(
    ?C:atom(FunName),
    [
      ?C:clause([ ?C:variable("VarStack") ], none, [
          ?MFA(erlang, iolist_to_binary, [FunBody])
        ])
    ]
  ).

to_ast(ParseTree, TargetModule) ->
  {ok, RenderInternalAst, State} = transform_tree(ParseTree, [], #tpl_mod{}),
  Funs = [{render, RenderInternalAst} | State#tpl_mod.funs ],
  FunsAst = lists:reverse([ tpl_function(FunName, FunBody) || {FunName, FunBody} <- Funs ]),

  ModuleAst = ?C:attribute(?C:atom(module), [?C:atom(TargetModule)]),
  ExportAst = ?C:attribute(?C:atom(export), [
      ?C:list([
          ?C:arity_qualifier(?C:atom(render), ?C:integer(1))
        ])
    ]),

  Sources = lists:flatten([
    ModuleAst,
    ExportAst,
    FunsAst
  ]),

  {ast, TargetModule, Sources}.


transform_tree([], Result, State) ->
  {ok, ?C:list(lists:reverse(Result)), State};

transform_tree([Node | Rest], Result, State) ->
  {ok, Code, NewState} = transform_node(Node, State),
  transform_tree(Rest, [Code | Result], NewState).

define_block_function(Id, Body, #tpl_mod{fun_idx = FunIdx} = State) ->
  FunName = list_to_atom(lists:flatten(["block_", integer_to_list(FunIdx), "_", Id])),

  {ok, FunAst, StateMod} = transform_tree(Body, [], State#tpl_mod{fun_idx = FunIdx + 1}),
  Funs = StateMod#tpl_mod.funs,
  NewState = StateMod#tpl_mod{funs = [{FunName, FunAst} | Funs]},
  {FunName, FunAst, NewState}.


transform_node({string, _, Value}, State) -> {ok, ?C:abstract(list_to_binary(Value)), State};

transform_node({block, Id, BlockBody}, State) ->
  BlockId = make_var_name(Id),
  {FunName, FunFast, NewState} = define_block_function("block", BlockBody, State),

  Code = ?MFA(
    new_runtime,
    call_block_tag,
    [
      ?C:tuple([
          ?C:atom(block),
          ?C:atom(BlockId)
        ]),
      ?C:implicit_fun(?C:arity_qualifier(?C:atom(FunName), ?C:integer(1))),
      ?C:variable("VarStack")
    ]
  ),

  {ok, Code, NewState};

transform_node({for, {in, ForVariables, ForList}, ForBody}, #tpl_mod{fun_idx = FunIdx} = State) ->
  VarNames = [ make_var_name(X) || X <- ForVariables ],
  {ok, ListAst, StateList} = transform_node(ForList, State),

  {FunName, FunAst, NewState} = define_block_function("for", ForBody, StateList),

  % apply: new_runtime:call_block_tag({for, [x], 'some.list'}, fun tpl_for_0/2, VarStack)
  Code = ?MFA(
    new_runtime,
    call_block_tag, 
    [
      ?C:tuple(
        [
          ?C:atom(for),
          ?C:list([ ?C:atom(Var) || Var <- VarNames]),
          ListAst
        ]
      ),
      ?C:implicit_fun(?C:arity_qualifier(?C:atom(FunName), ?C:integer(1))),
      ?C:variable("VarStack")
    ]
  ),

  {ok, Code, NewState};

transform_node({apply_filter, ToVariable, {filter, FilterName, FilterArgs}}, State) ->
  ?debugVal(FilterArgs),
  {ok, ApplyTo, State} = transform_node(ToVariable, State),

  FiltersAst = [ transform_node(FilterArg, State) || FilterArg <- FilterArgs ],

  Code = ?MFA(
    new_runtime,
    apply_filter,
    [
      ?C:atom(make_var_name(FilterName)),
      ApplyTo,
      ?C:list([ ArgAst || {ok, ArgAst, _} <- FiltersAst ]),
      ?C:variable("VarStack")
    ]
  ),

  {ok, Code, State};

transform_node({variable, VarName}, State) ->
  Code = ?MFA(
    new_runtime,
    resolve,
    [
      transform_to_binary(make_var_name(VarName)),
      ?C:variable("VarStack")
    ]
  ),
  {ok, Code, State};

transform_node({ifelse, Expr, True, False}, State) ->
  {ok, TrueAst, State2} = transform_tree(True, [], State),
  {ok, FalseAst, State3} = transform_tree(False, [], State2),

  {ok, ExprAst, State4} = transform_node(Expr, State3),

  Code = ?C:case_expr(
    ?MFA(new_runtime, is_true, [ExprAst]),
    [
      ?C:clause([?C:atom(true)], none, [TrueAst]),
      ?C:clause([?C:underscore()], none, [FalseAst])
    ]
  ),

  {ok, Code, State4};

transform_node({expr, Op, Lhs, Rhs}, State) ->
  {ok, LhsAst, State2} = transform_node(Lhs, State),
  {ok, RhsAst, State3} = transform_node(Rhs, State2),

  Code = ?MFA(
    new_runtime,
    Op,
    [
      LhsAst,
      RhsAst
    ]
  ),

  {ok, Code, State3};

transform_node({tag, Id, Args}, State) ->
  ArgsAst = [ transform_arg(Arg) || Arg <- Args ],

  Code = ?MFA(
    new_runtime,
    call_tag,
    [
      ?C:atom(make_var_name(Id)),
      ?C:list(
        ArgsAst
      ),
      ?C:variable("VarStack")
    ]
  ),
  {ok, Code, State};

transform_node({output, Expr}, State) ->
  {ok, ExprAst, State2} = transform_node(Expr, State),
  Code = ?MFA(
    new_runtime,
    to_string,
    [
      ExprAst
    ]
  ),

  {ok, Code, State2};

transform_node({trans, Var}, State) ->
  {ok, VarAst, State} = transform_node(Var, State),
  Code = ?MFA(
    new_runtime,
    translate,
    [
      VarAst
    ]
  ),

  {ok, Code, State};

transform_node({string_literal, _, String}, State) ->
  {ok, ?C:abstract(unescape_string_literal(String)), State};

transform_node(Unparsed, State) ->
  throw({unparsed, Unparsed}).

transform_to_binary(Atom) ->
  Bin = list_to_binary(atom_to_list(Atom)),
  ?C:abstract(Bin).

transform_arg({{identifier, _, _} = Id, Var}) ->
  {ok, ValAst, _} = transform_node(Var, no_state),
  ?C:tuple([
      ?C:atom(make_var_name(Id)),
      ValAst
    ]).

make_var_name({identifier, _, Atom}) -> Atom.

unescape_string_literal(String) ->
  unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
  list_to_binary(lists:reverse(Acc));
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
  unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
  unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
  unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
  unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
  unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
  unescape_string_literal(Rest, [C | Acc], noslash).

-ifdef(TEST).

compile_test_test_none() ->
  DummyTpl = <<
    "{% if y == \"test\" and wtf %}true{{ true }}{% else %}false{% endif %}"
    "hello world"
    "{{ x | default: 'a', 'b', 'c' }}"
    "{% for x in some.list %}{{ x | test | test: some.other.var, 'hello' }}{% endfor %}"
    "{% some_tag hello=arg2 more='test' %}"
   >>, 

  {ok, Module} = compile(DummyTpl, compile_test_dtl),
  VarStack = [{stack, [
        {y, "test"},
        {true, "yeah"},
        {some, [
              {list, [a,b,c]}
            ]}
      ]}],

  Result = Module:render(VarStack),
  ?debugVal(Result),
  ok = not_ok.

-endif.
