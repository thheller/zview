{% export format="html" title="Simple Template" %}

<html>
  <head>
    {{ title | default: 'Default Title' }}
  </head>
  <body>
    {% for x in list %}
      {% if $loop.first %}
      first
      {% elsif $loop.last %}
      last
      {% end %}
      <span id="{{ $loop.index }}">{{ x }}</span>
    {% end %}

    {{ ["2011", "09", "30"] | inspect }}

    {% render template="includes_tags" list=list %}

    <table>
      <tbody>
        {% for k, v in $context.vars %}
        <tr>
          <td>{{ k }}</td>
          <td>{{ v }}</td>
        </tr>
        {% end %}
      </tbody>
    </table>
  </body>
</html>
