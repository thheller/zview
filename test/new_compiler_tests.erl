-module(new_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

example1_test() ->
  Doc1 = "<html>"
      "<head>"
        "{{ title | default: 'My Page Title' }}"
      "</head>"
      "<body>"
        "{% for x in content.by_group.content %}"
          "<div id=\"{{ x.id }}\" class=\"content\">{{ x.content }}</div>"
        "{% endfor %}"
        "{% custom_tag foo=bar %}"
        "inside custom_tag"
        "{% endcustom_tag %}"
      "</body>"
    "</html>",

  {ok, Result} = erlydtl_scanner:scan(Doc1),
  {ok, ParseTree} = erlydtl_parser:parse(Result),
  {source, _, Source} = new_compiler:to_source(ParseTree, example_test1),
  ?debugMsg(Source).
