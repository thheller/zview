zview - Introduction
==============

This is zview, a Template Engine for Erlang (at least it will be sometime in the future).

Its in early alpha and not recommended for use (half of the Stuff doesnt work yet).

It started out as a fork of erlydtl since I wanted to address some of the issue I had
with it. Turned out that changing those was way more work than expected and would have 
broken most DTL Templates anyway, so I started from scratch, only reusing the
erlydtl\_scanner and erlydtl\_parser, although those were modified too.

I'll highlight the major differences later, first lets have a look:

    <html>
      <head>
        <title>{{ title | default: 'Default Page Title' }}</title>
      </head>

      <body>

        The usual for loop
        <ul>
          {% for x in items %}
            <li>{{ x }}</li>
          {% endfor %}
        </ul>

        The usual if/else

        {% if foo == 'bar' %}
          bar
        {% else %}
          {{ foo }}
        {% endif %}

        Simple Tags
        {% render "other_template" %}

        Simple Block Tag, vars declared in Args are only available inside the do/end Block 
        {% with var1="test" var2=some.nested.json.data.struct do %}
          {{ var2 }}
        {% end %}

        Not defined, outputs nothing (instead of undefined like erlydtl)
        {{ var1 }}

        Custom Tags are defined by prefixing it with alias@
        {% ext@simple_tag arg1=title %}

        Custom Tags can have Blocks too
        {% ext@custom_tag also="has args" do %}
          inside custom tag
        {% end %}

      </body>
    </html>

Should look familiar to anyone having used erlydtl, Django Templates or Liquid before.

Documentation
-----

TODO!!!

Differences in the Templates itself
---

Main Difference is that I wanted to be able to have the 
Template export Key/Value Pairs to the caller.

    {% export meta="data" some=var %}

    {% export block do %}
      <h1>Super Duper Title</h1>
    {% end %}

    {ok, Content, Exports} = zview:render("my_template.tpl", [{var, "test"}]).

Exports will be a proplist containing [{meta, "data"}, {some, "test"}, {block, "h1..."}].

I wanted this as a stricter alternative to View Inheritance. Templates are completely standalone
and cannot have dependencies to the outside (other than the runtime, tags). You could still
achieve the same as View Inheritance with a simple loop and some exports.
 
    {% export layout="my_page_layout.html" %}
    {% export content do %}
      Page Content
    {% end %}

Custom Block Tags:

    {% ext@some_tag kw="args" var=some.list do %}
      tag content
    {% end %}

Custom Tags:

    {% ext@some_tag with="args" %}

Filters may have multiple Args, and of course can be custom too:

    {{ some_var | mod@some_filter: 'arg1', arg2, 'arg3' }}

Differences in the Runtime
---

Way too many to even start comparing them.

"Special" Variables
----

There are some Special Variables that you can access in Templates giving access to
the Template Context itself. They are prefixed by "$"

$for
---

Same as forloop in DTL, Liquid and only available inside for loops, will raise an Error
if called outside for loop.

    {{ $for.index }} Also has the common forloop attributes (index, index0, rindex, rindex0, first, last, length)

$vars
---

{Key, Value} Pairs of all Vars given to the Template render fun. Mainly intended for debugging.
    
    {% for key, value in $vars %} 
      Key: {{ key }} Value: {{ value }}
    {% endfor %}

    
Probably more I cant think of right now.


Thanks
===

- [erlydtl](https://github.com/evanmiller/erlydtl)
  Without it I probably wouldnt even have started, never written a parser/compiler before so
  it was a very valuable Reference. Also obviously Django Templates and Liquid for Inspiration.

- [kvc](https://github.com/etrepum/kvc)
  Used to do most Variable Lookups. Its awesome.


License
---

MIT
