{%
  export
  format="html"
  title="Simple Template"
%}
{# could also be all one line #}

<html>
  <head>
    {{ title | default: 'Default Title' }}
  </head>
  <body>
    {% for x in list %}
      <span>{{ x }}</span>
    {% endfor %}

    {{ list }}
  </body>
</html>
