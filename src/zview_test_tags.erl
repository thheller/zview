-module(zview_test_tags).

-export([zview_filter/4, zview_tag/3, zview_block/4]).

zview_filter(_Name, _Value, _Args, _VarStack) ->
  invalid_filter.

zview_tag(_TagName, _TagArgs, _VarStack) ->
  invalid_tag.

zview_block(_TagName, _TagArgs, _Block, _VarStack) ->
  invalid_block.
