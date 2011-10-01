{% export format="html" title="Simple Template" %}

<html>
  <head>
    {{ title | default: 'Default Title' }}
  </head>
  <body>
    {% for x in list %}
      {% if $loop.first %}
      first
      {% endif %}
      <span id="{{ $loop.index }}">{{ x }}</span>
      {% if $loop.last %}
      last
      {% endif %}
    {% endfor %}

    {{ ["2011", "09", "30"] | inspect }}

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
