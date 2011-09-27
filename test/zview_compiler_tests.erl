-module(zview_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

super_meaningless_template_parse_and_compile_test() ->
  Doc1 =
    "{% export wtf='hi' test=1 demo=x %}"
    "{% export dummy do %}"
    "hello exported dummy"
    "{% end %}"
    "{{ title | default: 'My Page Title' }}"
    "{% for x in content.by_group.content %}"
      "<div id=\"{{ x.id }}\" class=\"content\">{{ x.content }}</div>"
    "{% endfor %}"
    "{% builtin_tag foo=bar do %}"
      "inside builtin_tag"
    "{% end %}"
    "{% ext@custom_tag foo=bar string=\"arg\" do %}"
      "inside custom_tag"
      "{{ x | ext@custom_filter: 'test', a, 'X' | more_filters }}"
    "{% end %}"
    "{% if x == \"from_args\" %}"
    "inside if true"
    "{% endif %}"
    "{% if x >= 1 and y <= \"test\" %}"
    "inside if true"
    "{% else %}"
    "inside else"
    "{% endif %}",

  {source, _, Source} = zview_compiler:to_source({from_source, Doc1}, compile_test),
  ?debugMsg(Source),

  {ok, Module} = zview_compiler:compile(Doc1, compile_test_dtl),
, Vars = [
        {y, "test"},
        {x, "from args"},
        {true, "yeah"},
        {some, [
              {list, [a,b,c]}
            ]}
      ],

  VarStack = zview_runtime:new_var_stack(Vars),

  {ok, Result, Exports} = Module:render(VarStack),
  ?debugMsg(Result),

  ?assertEqual(4, length(Exports)),
  ?assertEqual("from args", proplists:get_value(demo, Exports)),
  ?assertEqual(1, proplists:get_value(test, Exports)),
  ?assertEqual(<<"hi">>, proplists:get_value(wtf, Exports)),
  ?assertEqual(<<"hello exported dummy">>, iolist_to_binary(proplists:get_value(dummy, Exports))),
  ok.


compile_test() ->
  DummyTpl = 
    "{% if y == \"test\" and x %}true{{ true }}{% else %}false{% endif %}"
    "hello world üäö"
    "{{ x | default: 'a', 'b', 'c' | ext@some_filter }}"
    "{% for x in some.list %}{{ x }} {{ _parent.x }} {% endfor %}"
    "{% ext@some_tag hello=arg2 more='test' %}"
    "{% some_tag hello=arg2 do %}"
    "some_tag_content"
    "{% end %}"
    "{% export hello='world' dummy=y %}"
    "{% export title do %}hello title{% end %}",
  ok.
