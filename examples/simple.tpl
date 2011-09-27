{% export format="html" title="Simple Template" %}

<html>
  <head>
    {{ title | default: 'Default Title' }}
  </head>
  <body>
    {% for x in list %}
    {% if _for.first %}
      first
    {% endif %}
      <span id="{{ _for.index }}">{{ x }}</span>
    {% if _for.last %}
      last
    {% endif %}
    {% endfor %}

    {{ list }}
  </body>
</html>
