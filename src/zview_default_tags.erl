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

zview_tag(_TagName, _TagArgs, _VarStack) ->
  invalid_tag.

zview_block(_TagName, _TagArgs, _Block, _VarStack) ->
  invalid_tag.
