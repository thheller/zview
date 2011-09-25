-module(zview_compiler).

-export([to_source/2, compile/2]).

-include_lib("eunit/include/eunit.hrl").

-define(C, erl_syntax).
-define(MFA,
  fun
    (none, AppFun, AppArgs) ->
      ?C:application(
        none,
        ?C:atom(AppFun),
        AppArgs);

    (Mod, AppFun, AppArgs) ->
      ?C:application(
        ?C:atom(Mod),
        ?C:atom(AppFun),
        AppArgs)
  end
).

-define(PP, fun(Arg) ->
      erl_prettypr:format(Arg, [{paper, 240}])
  end).

-record(tpl_mod, {blocks = [], exports = []}).

to_source({from_source, Bin}, TargetModule) ->
  {ok, Scan} = zview_scanner:scan(Bin),
  {ok, Parse} = zview_parser:parse(Scan),
  to_source(Parse, TargetModule);

to_source(ParseTree, TargetModule) ->
  {ast, _, Sources} = to_ast(ParseTree, TargetModule),

  Body = string:join([ ?PP(Source) || Source <- Sources ], "\n\n"),
  {source, TargetModule, Body}.

to_beam({ast, TargetModule, Sources}) ->
  Forms = [ ?C:revert(X) || X <- Sources ],
  {ok, TargetModule, Binary} = compile:forms(Forms, [debug_info, verbose, report_errors]),
  {ok, Binary}.


to_module({ast, TargetModule, _Sources} = Ast) ->
  {ok, ModuleBinary} = to_beam(Ast),

  code:purge(TargetModule),
  {module, _} = code:load_binary(TargetModule, atom_to_list(TargetModule) ++ ".erl", ModuleBinary),

  {ok, TargetModule}.

compile(Bin, TargetModule) ->
  {ok, Scan} = zview_scanner:scan(Bin),
  {ok, ParseTree} = zview_parser:parse(Scan),
  Ast = to_ast(ParseTree, TargetModule),
  to_module(Ast).

tpl_function(FunName, FunBody) ->
  ?C:function(
    ?C:atom(FunName),
    [
      ?C:clause([ ?C:variable("VarStack") ], none, [
          FunBody
        ])
    ]
  ).

to_ast(ParseTree, TargetModule) ->
  {ok, RenderInternalAst, State} = transform_tree(ParseTree, [], #tpl_mod{}),

  Funs = [{render_internal, RenderInternalAst} | State#tpl_mod.blocks ],
  FunsAst = lists:reverse([ tpl_function(FunName, FunBody) || {FunName, FunBody} <- Funs ]),

  ModuleAst = ?C:attribute(?C:atom(module), [?C:atom(TargetModule)]),
  ExportAst = ?C:attribute(?C:atom(export), [
      ?C:list([
          ?C:arity_qualifier(?C:atom(render), ?C:integer(1)),
          ?C:arity_qualifier(?C:atom(render), ?C:integer(2))
        ])
    ]),


  ExportFuns = State#tpl_mod.exports,

  % map exported vars to either Fun(VarStack), or [{Key, Fun(VarStack)}]
  % Fun(VarStack) is for {% export with=multiple args="yo" %}
  % {Key, Fun} is for {% export key do %} body {% end %}
  ExportVars = lists:map(
    fun
      ({Key, Callback}) ->
        ?C:list([?C:tuple([ ?C:atom(Key), ?MFA(none, Callback, [ ?C:variable("VarStack") ]) ])]);
      (Callback) ->
        ?MFA(none, Callback, [ ?C:variable("VarStack") ])
    end,
    ExportFuns
  ),

  RenderFunAst = ?C:function(
    ?C:atom(render),
    [
      ?C:clause([ ?C:variable("Input") ], none, [
          ?MFA(none, render, [ ?C:variable("Input"), ?C:atom(undefined) ])
        ])
    ]
  ),

  Render2FunAst = ?C:function(
    ?C:atom(render),
    [
      ?C:clause([ ?C:variable("Input"), ?C:variable("Context") ], none, [
          ?C:match_expr(
            ?C:tuple([
                ?C:atom(ok),
                ?C:variable("VarStack")
              ]),
            ?MFA(
              zview_runtime,
              make_var_stack,
              [
                ?C:variable("Input"),
                ?C:variable("Context")
              ]
            )
          ),
          ?C:match_expr(
            ?C:variable("Exports"),
            ?MFA(
              lists,
              append,
              [
                ?C:list(ExportVars)
              ]
            )
          ),
          ?C:match_expr(
            ?C:variable("Content"),
            ?MFA(none, render_internal, [ ?C:variable("VarStack") ])
          ),
          ?C:tuple([
              ?C:atom(ok),
              ?C:variable("Content"),
              ?C:variable("Exports")
            ])
        ])
    ]
  ),

  Sources = lists:flatten([
    ModuleAst,
    ExportAst,
    FunsAst,
    RenderFunAst,
    Render2FunAst
  ]),

  {ast, TargetModule, Sources}.


transform_tree([], Result, State) ->
  {ok, ?C:list(lists:reverse(Result)), State};

transform_tree([Node | Rest], Result, State) ->
  case transform_node(Node, State) of
    {ok, Code, NewState} ->
      transform_tree(Rest, [Code | Result], NewState);

    {skip, NewState} ->
      transform_tree(Rest, Result, NewState)

  end.


define_block_function(Id, Body, State) ->
  {ok, FunAst, StateMod} = transform_tree(Body, [], State),
  define_block_function_with_ast(Id, FunAst, StateMod).

define_block_function_with_ast(Id, FunAst, #tpl_mod{blocks = Blocks} = State) ->
  FunName = make_fun_name(Id),
  {FunName, State#tpl_mod{blocks = [{FunName, FunAst} | Blocks]}}.

transform_node({string, _, Value}, State) -> {ok, ?C:abstract(list_to_binary(Value)), State};

transform_node({block, {block_tag, Id, Args}, BlockBody}, State) ->
  BlockId = make_var_name(Id),
  BlockArgsAst = make_args_ast(Args),
  {FunName, NewState} = define_block_function(Id, BlockBody, State),

  Code = ?MFA(
    zview_runtime,
    call_block_tag,
    [
      ?C:tuple([
          ?C:atom(block),
          ?C:atom(BlockId),
          BlockArgsAst
        ]),
      ?C:implicit_fun(?C:arity_qualifier(?C:atom(FunName), ?C:integer(1))),
      ?C:variable("VarStack")
    ]
  ),

  {ok, Code, NewState};

transform_node({for, {Id, {in, ForVariables, ForList}}, ForBody}, State) ->
  VarNames = [ make_var_name(X) || X <- ForVariables ],
  {ok, ListAst, StateList} = transform_node(ForList, State),

  {FunName, NewState} = define_block_function(Id, ForBody, StateList),

  % zview_runtime:call_block_tag({for, [x], 'some.list'}, fun tpl_for_0/2, VarStack)
  Code = ?MFA(
    zview_runtime,
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
  {ok, ApplyTo, State} = transform_node(ToVariable, State),

  FiltersAst = [ transform_node(FilterArg, State) || FilterArg <- FilterArgs ],

  Code = ?MFA(
    zview_runtime,
    apply_filter,
    [
      ?C:atom(make_var_name(FilterName)),
      ApplyTo,
      ?C:list([ ArgAst || {ok, ArgAst, _} <- FiltersAst ]),
      ?C:variable("VarStack")
    ]
  ),

  {ok, Code, State};

transform_node({block, {export_block, Id, [{auto, ExportId}]}, BlockBody}, State) ->
  {FunName, #tpl_mod{exports = Exports} = NewState} = define_block_function(Id, BlockBody, State),
  {skip, NewState#tpl_mod{exports = [{make_var_name(ExportId), FunName} | Exports]}};

transform_node({export_tag, Id, ExportArgs}, State) ->
  Code = make_args_ast(ExportArgs),
  {FunName, #tpl_mod{exports = Exports} = NewState} = define_block_function_with_ast(Id, Code, State),
  {skip, NewState#tpl_mod{exports = [FunName | Exports]}};

transform_node({attribute, _} = Token, State) ->
  make_resolve_call(variable_path(Token, []), State);

transform_node({variable, _} = Token, State) ->
  make_resolve_call(variable_path(Token, []), State);

transform_node({ifelse, Expr, True, False}, State) ->
  {ok, TrueAst, State2} = transform_tree(True, [], State),
  {ok, FalseAst, State3} = transform_tree(False, [], State2),

  {ok, ExprAst, State4} = transform_node(Expr, State3),

  Code = ?C:case_expr(
    ?MFA(zview_runtime, is_true, [ExprAst]),
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
    zview_runtime,
    Op,
    [
      LhsAst,
      RhsAst
    ]
  ),

  {ok, Code, State3};

transform_node({tag, Id, Args}, State) ->

  Code = ?MFA(
    zview_runtime,
    call_tag,
    [
      ?C:atom(make_var_name(Id)),
      make_args_ast(Args),
      ?C:variable("VarStack")
    ]
  ),
  {ok, Code, State};

transform_node({output, Expr}, State) ->
  {ok, ExprAst, State2} = transform_node(Expr, State),
  Code = ?MFA(
    zview_runtime,
    to_string,
    [
      ExprAst
    ]
  ),

  {ok, Code, State2};

transform_node({trans, Var}, State) ->
  {ok, VarAst, State} = transform_node(Var, State),
  Code = ?MFA(
    zview_runtime,
    translate,
    [
      VarAst
    ]
  ),

  {ok, Code, State};

transform_node({string_literal, _, String}, State) ->
  {ok, ?C:abstract(unescape_string_literal(String)), State};

transform_node(Unparsed, _State) ->
  throw({unparsed, Unparsed}).

transform_arg({{identifier, _, _} = Id, Var}) ->
  {ok, ValAst, _} = transform_node(Var, no_state),
  ?C:tuple([
      ?C:atom(make_var_name(Id)),
      ValAst
    ]).

make_var_name({identifier, _, Atom}) -> Atom.

make_args_ast(Args) ->
  ArgsAst = [ transform_arg(Arg) || Arg <- Args ],
  ?C:list(ArgsAst).


make_resolve_call(Path, State) ->
  Code = ?MFA(
    zview_runtime,
    resolve,
    [
      ?C:abstract(Path),
      ?C:variable("VarStack")
    ]
  ),
  {ok, Code, State}.

make_fun_name({identifier, {Row, Col}, Id}) ->
  list_to_atom(lists:flatten([
        atom_to_list(Id),
        "_at_row_",
        integer_to_list(Row),
        "_col_",
        integer_to_list(Col)
    ]));

make_fun_name({for_keyword, {Row, Col}, _}) ->
  list_to_atom(lists:flatten([
        "for_loop_at_row_",
        integer_to_list(Row),
        "_col_",
        integer_to_list(Col)
      ]));

make_fun_name({export_keyword, {Row, Col}, _}) ->
  list_to_atom(lists:flatten([
        "export_at_row_",
        integer_to_list(Row),
        "_col_",
        integer_to_list(Col)
      ])).

variable_path({attribute, {{identifier, _, Attr}, Of}}, Path) ->
  AttrBin = list_to_binary(atom_to_list(Attr)),
  variable_path(Of, [AttrBin | Path]);

variable_path({variable, {identifier, _, VarName}}, Path) ->
  [list_to_binary(atom_to_list(VarName)) | Path].


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

compile_test_test() ->
  DummyTpl = 
    "{% if y == \"test\" and x %}true{{ true }}{% else %}false{% endif %}"
    "hello world"
    "{{ x | default: 'a', 'b', 'c' }}"
    "{% for x in some.list %}{{ x }} {{ _parent.x }} {% endfor %}"
    "{% some_tag hello=arg2 more='test' %}"
    "{% some_tag hello=arg2 do %}"
    "some_tag_content"
    "{% end %}"
    "{% export hello='world' dummy=y %}"
    "{% export title do %}hello title{% end %}"
  , 

  % {source, _, Source} = to_source({from_source, DummyTpl}, compile_test),
  % ?debugMsg(Source),

  {ok, Module} = compile(DummyTpl, compile_test_dtl),
  VarStack = [
        {y, "test"},
        {x, "from args"},
        {true, "yeah"},
        {some, [
              {list, [a,b,c]}
            ]}
      ],

  {ok, Result, Exports} = Module:render(VarStack),
  ?debugMsg(Result),
  ?debugVal(Exports),
  ok.

-endif.
