= TODO

Write README.

= Motivation

I started using erlydtl but felt limited quite soon, so I started building this based
on the erlydtl scanner/parser.

It still uses the same syntax mostly, but doesnt aim to be compatible to DTL or any
other similar Template Engine (eg. liquid).

Also it should be way easier to extend than erlydtl is.

= Differences:

Main Difference is that I wanted to be able to have the 
Template export Key/Value Pairs to the caller.

    {% export meta="data" some=var %}

    {% export block do %}
      <h1>Super Duper Title</h1>
    {% end %}

    {ok, Content, Exports} = template:render([{var, "test"}]).

Exports will be a proplist containing [{meta, "data"}, {some, "test"}, {block, ...}].

I wanted this as a stricter alternative to View Inheritance. Templates are completely standalone
and cannot have dependencies to the outside (other than the runtime, tags). Although its trivial
to implement inheritance with a couple lines of plain erlang.

    {% export layout="my_page_layout.html" %}
    {% export content do %}
      Page Content
    {% end %}

== Custom Block Tags:

    {% some_tag kw="args" var=some.list do %}
      tag content
    {% end %}

== Custom Tags:

    {% some_tag with="args" %}

== Filters may have multiple Args:

    {{ some_var | some_filter: 'arg1', arg2, 'arg3' }}

Probably more I cant think of right now.
