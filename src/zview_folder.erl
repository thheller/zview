-module(zview_folder).

-export([init/3, render/3, get_custom_tag/2]).

-record(folder, {id, root, table, ext = ".tpl", options = []}).
-record(tpl, {name, module, filename, source, last_modified}).

init(Id, Root, Options) ->
  case filelib:is_dir(Root) of
    true ->
      Tab = ets:new(zview_folder, [set, public, {keypos, #tpl.name}]),
      {ok, #folder{
          id = Id,
          root = Root,
          table = Tab,
          options = Options
        }
      };

    false ->
      {error, {"not a directory", Root}}
  end.

get_custom_tag(Config, Alias) ->
  CustomTags = proplists:get_value(custom_tags, Config#folder.options, []),

  case proplists:get_value(Alias, CustomTags, undefined) of
    undefined ->
      not_found;

    Found ->
      {ok, Found}
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

      case zview_compiler:compile(Source, TargetModule, {zview_folder, Config}) of
        ok ->
          CompiledTemplate = #tpl{
            module = TargetModule, 
            name = Template,
            source = Source,
            filename = Filename,
            last_modified = filelib:last_modified(Filename)
          },

          true = ets:insert(Config#folder.table, CompiledTemplate),

          render_template(Config, CompiledTemplate, Vars);

        {error, Reason} ->
          {error, Reason}
      end;

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

render_template(Config, #tpl{module = Module} = Template, VarStack) ->
  Result = Module:render(VarStack),
  Result.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% this is not exaclty the right way, need to figure this out

setup_zview() ->
  {ok, EunitRoot} = file:get_cwd(),

  Root = filename:dirname(EunitRoot),

  TemplateRoot = filename:absname(filename:join([Root, "test", "templates"])),
  {ok, TemplateRoot}.


templates_test() ->
  {ok, TemplateRoot} = setup_zview(),

  {ok, Repo} = init(test, TemplateRoot, [ {custom_tags, [{test, zview_test_tags}]} ]),

  Vars = [
    {title, "Test Title (should not be default)"},
    {list, ["a", "b", "c"]}
  ],

  VarStack = zview:init_var_stack(Vars), 

  {ok, Content, Exports} = render(Repo, "simple", VarStack),

  ?debugMsg(Content),
  ?debugVal(Exports),

  ok.

-endif.

