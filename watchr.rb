watch('^src/(.*)\.(e|y)rl') do |m|
  mod = m[1]

  if system('rebar compile skip_deps=true')
    system("rebar eunit suite=#{mod} skip_deps=true")
  end
end

watch('^test/(.*)\.erl') do |m|
  mod = m[1]

  if system('rebar compile skip_deps=true')
    system("rebar eunit suite=#{mod} skip_deps=true")
  end
end

watch('^templates/(.*).dtl') do |m|
  system('rebar compile skip_deps=true')
end

watch('^examples/(.*).(tpl|input)') do |m|
  cmd = %{ ./scripts/run_example examples #{ m[1] } }
  puts "Running: #{ cmd }"
  system(cmd)
end
