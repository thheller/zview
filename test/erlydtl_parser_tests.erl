-module(erlydtl_parser_tests).

-include_lib("eunit/include/eunit.hrl").

example_export_test() ->
  Doc1 = 
    "{% export layout='test' foo=bar %}"
    "{% export content do %}"
      "{% inside_tag foo=bar do %} inside_{{ foo }} tag {% end %}"
      "inside custom_tag"
    "{% end %}",
  {ok, Result} = erlydtl_scanner:scan(Doc1),
  ?debugVal(Result),
  % should probably test this somewhat, but ok is good enough for now
  {ok, ParseTree} = erlydtl_parser:parse(Result),
  ?debugVal(ParseTree),
  ok.


example1_test() ->
  Doc1 = 
    "{% custom_tag foo=bar do %}"
      "{% inside_tag foo=bar do %} inside_{{ foo }} tag {% end %}"
      "inside custom_tag"
    "{% end %}",
  {ok, Result} = erlydtl_scanner:scan(Doc1),
  % should probably test this somewhat, but ok is good enough for now
  {ok, ParseTree} = erlydtl_parser:parse(Result),
  % ?debugVal(ParseTree),
  ok.


example2_test() ->
  Doc = "{% if x %} {% some_tag arg=x %} {% endif %}",

  {ok, Scan} = erlydtl_scanner:scan(Doc),
  {ok, ParseTree} = erlydtl_parser:parse(Scan),
  % ?debugVal(ParseTree),
  ok.
