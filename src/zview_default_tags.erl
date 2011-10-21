-module(zview_default_tags).

-export([zview_filter/4, zview_tag/3, zview_block/4]).

zview_filter(inspect, Value, _Args, VarStack) ->
  {value, io_lib:format("~p", [Value])};

zview_filter(default, Value, [], VarStack) ->
  {value, Value};

zview_filter(default, Value, [First | Args], VarStack) ->
  case zview_runtime:is_true(Value) of
    true -> {value, Value};
    false -> zview_filter(default, First, Args, VarStack)
  end;

zview_filter(_Filter, _Value, _Args, _VarStack) ->
  invalid_filter.


zview_tag(render, Args, VarStack) ->
  case proplists:get_value(template, Args, undefined) of
    undefined ->
      {output, ["cannot render without template arg"]};

    TemplateName ->
      RenderStack = zview:init_var_stack(Args),

      {Repo, Config} = zview_runtime:get_template_context(VarStack),

      {ok, Content, _} = Repo:render(Config, binary_to_list(TemplateName), RenderStack),
      {output, Content}

  end;


zview_tag(_TagName, _TagArgs, _VarStack) ->
  invalid_tag.

zview_block(_TagName, _TagArgs, _Block, _VarStack) ->
  invalid_tag.
