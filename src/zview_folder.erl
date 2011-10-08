-module(zview_folder).

-export([init/1, render/3, init_var_stack/1]).

-record(folder, {root, table, ext = ".tpl", options = []}).
-record(tpl, {name, module, filename, source, last_modified}).

init(Options) ->
  case proplists:get_value(root, Options) of
    undefined ->
      {error, "no template root given"};

    Root ->
      case filelib:is_dir(Root) of
        true ->
          Tab = ets:new(zview_folder, [set, public, {keypos, #tpl.name}]),
          {ok, #folder{
              root = Root,
              table = Tab,
              options = proplists:get_value(options, Options, [])
            }
          };

        false ->
          {error, {"not a directory", Root}}

      end
  end.

init_var_stack(#folder{options = Options}) ->
  zview_runtime:init_var_stack(Options, []).

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

      case zview_compiler:compile(Source, TargetModule) of
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

  {file, Path} = code:is_loaded(?MODULE),
  Root = filename:dirname(filename:dirname(Path)),
  TemplateRoot = filename:absname(filename:join([Root, "test", "templates"])),
  {ok, TemplateRoot}.


templates_test() ->
  {ok, TemplateRoot} = setup_zview(),

  {ok, Repo} = init([
      {root, TemplateRoot},
      {options, [ {custom_tags, [{test, zview_test_tags}]} ]}
    ]),

  InitStack = init_var_stack(Repo),

  ?debugVal(InitStack),

  Vars = [
    {title, "Test Title (should not be default)"},
    {list, ["a", "b", "c"]}
  ],

  VarStack = zview_runtime:push_var_stack(Vars, InitStack), 

  {ok, Content, Exports} = render(Repo, "simple", VarStack),

  ?debugMsg(Content),
  ?debugVal(Exports),

  ok.

-endif.

