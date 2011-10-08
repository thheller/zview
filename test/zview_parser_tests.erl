-module(zview_parser_tests).

-include_lib("eunit/include/eunit.hrl").

example_export_test() ->
  Doc1 = 
    "{% export layout='test' foo=bar %}"
    "{% export content do %}"
      "{% inside_tag foo=bar do %} inside_{{ foo }} tag {% end %}"
      "inside custom_tag"
    "{% end %}",
  {ok, Result} = zview_scanner:scan(Doc1),
  % ?debugVal(Result),
  % should probably test this somewhat, but ok is good enough for now
  {ok, ParseTree} = zview_parser:parse(Result),
  %?debugVal(ParseTree),
  ok.


example1_test() ->
  Doc1 = 
    "{% custom_tag foo=bar do %}"
      "{% inside_tag foo=bar do %} inside_{{ foo }} tag {% end %}"
      "inside custom_tag"
    "{% end %}",
  {ok, Result} = zview_scanner:scan(Doc1),
  % should probably test this somewhat, but ok is good enough for now
  {ok, ParseTree} = zview_parser:parse(Result),
  % ?debugVal(ParseTree),
  ok.


example2_test() ->
  Doc = "{% if x %}true x{% elsif y %}true y{% else %}false{% end %}",

  {ok, Scan} = zview_scanner:scan(Doc),
  {ok, ParseTree} = zview_parser:parse(Scan),
  ?debugVal(ParseTree),
  ok.

example3_test() ->
  Doc = "{% html@form action=['/products/', product.id] method='POST' %}",

  {ok, Scan} = zview_scanner:scan(Doc),
  {ok, ParseTree} = zview_parser:parse(Scan),
  % ?debugVal(ParseTree),
  ok.

% TODO: write actual tests instead of just syntax checks
