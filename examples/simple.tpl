{% export format="html" title="Simple Template" %}

<html>
  <head>
    {{ title | default: 'Default Title' }}
  </head>
  <body>
    {% for x in list %}
      {% if $for.first %}
      first
      {% endif %}
      <span id="{{ $for.index }}">{{ x }}</span>
      {% if $for.last %}
      last
      {% endif %}
    {% endfor %}

    <table>
      <tbody>
        {% for k, v in $context.vars %}
        <tr>
          <td>{{ k }}</td>
          <td>{{ v }}</td>
        </tr>
        {% endfor %}
      </tbody>
    </table>
  </body>
</html>
