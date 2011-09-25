-module(sample).


render(InputVars) ->
  VarStack = zview_runtime:make_var_stack(InputVars),

  Exports = []
    ++ exports_0(VarStack)
    ++ [{content, render_export_0(VarStack)}]
    ++ [{title, "wassup"}],

  Body = render_internal(VarStack),

  {ok, Body, Exports}.

exports_0(VarStack) ->
  [{hello, <<"world">>}, {sample, zview_runtime:resolve([<<"test">>], VarStack)}].

exports_1(VarStack) ->
  [{content, Code}].
