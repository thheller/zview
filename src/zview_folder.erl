-module(zview_folder).

-export([init/1, render/3]).

-record(folder, {root, table, ext = ".tpl"}).
-record(tpl, {name, module, filename, source, last_modified}).

init(Options) ->
  case proplists:get_value(root, Options) of
    undefined ->
      {error, "no template root given"};

    Root ->
      case filelib:is_dir(Root) of
        true ->
          Tab = ets:new(zview_folder, [set, public, {keypos, #tpl.name}]),
          {ok, #folder{root = Root, table = Tab}};
        false ->
          {error, {"not a directory", Root}}

      end
  end.

render(#folder{table = Table} = Config, Template, Vars) ->
  case ets:lookup(Table, Template) of
    [] ->
      compile_and_render_template(Config, Template, Vars);

    [Compiled] ->
      check_modified_and_render_template(Config, Compiled, Vars)
 end.

get_template_filename(#folder{root = TemplateRoot, ext = Ext}, Template) ->
  filename:join([TemplateRoot, Template ++ Ext]).

compile_and_render_template(Config, Template, Vars) ->
  Filename = get_template_filename(Config, Template),

  case filelib:is_regular(Filename) of
    true ->
      {ok, Source} = file:read_file(Filename),

      {A, B, C} = erlang:now(),

      TargetModule = list_to_atom(lists:flatten(io_lib:format("zview_~p_~p_~p_~p", [Config#folder.table, A, B, C]))),

      ok = zview_compiler:compile(Source, TargetModule),

      CompiledTemplate = #tpl{
        module = TargetModule, 
        name = Template,
        source = Source,
        filename = Filename,
        last_modified = filelib:last_modified(Filename)
      },

      true = ets:insert(Config#folder.table, CompiledTemplate),

      render_template(Config, CompiledTemplate, Vars);

    false ->
      {template_not_found, Filename}

  end.

check_modified_and_render_template(Config, #tpl{name = Name, last_modified = Ts, filename = Filename} = Template, Vars) ->
  LastMod = filelib:last_modified(Filename),

  case LastMod > Ts of
    true ->
      io:format("Purging: ~p~n", [Template]),
      ok = purge_template(Config, Template),
      compile_and_render_template(Config, Name, Vars);

    false ->
      render_template(Config, Template, Vars)

  end.
    
purge_template(Config, Template) ->
  ok.

render_template(Config, #tpl{module = Module} = Template, Vars) ->
  VarStack = zview_runtime:new_var_stack(Vars),
  Result = Module:render(VarStack),
  Result.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% this is not exaclty the right way, need to figure this out

setup_zview() ->
  application:start(zview),

  {file, Path} = code:is_loaded(zview_folder),
  Root = filename:dirname(filename:dirname(Path)),
  TemplateRoot = filename:absname(filename:join([Root, "test", "templates"])),
  {ok, TemplateRoot}.


templates_test() ->
  {ok, TemplateRoot} = setup_zview(),

  {ok, Repo} = init([
      {root, TemplateRoot}
    ]),

  {ok, Content, Exports} = render(Repo, "simple", []),

  ?debugVal(Content),
  ?debugVal(Exports),

  application:stop(zview),

  ok.

-endif.

