-module(zview).

-include("zview.hrl").

-export([start/0, register/2, render/3]).

start() ->
  application:start(zview).

register(App, Options) ->
  gen_server:call(zview_server, {register, App, Options}).

render(App, Template, Vars) ->
  case ets:lookup(?ZVIEW_TAB, {App, Template}) of
    [] ->
      compile_and_render_template(App, Template, Vars);

    [Compiled] ->
      render_template(Compiled, Vars)
 end.

get_app_config(App) ->
  case ets:lookup(?ZVIEW_CFG, App) of
    [] ->
      not_registered;

    [Config] ->
      {ok, Config}

 end.

get_template_filename(#zview_cfg{template_root = TemplateRoot}, Template) ->
  filename:join([TemplateRoot, Template]).

compile_and_render_template(App, Template, Vars) ->
  {ok, Config} = get_app_config(App),

  Filename = get_template_filename(Config, Template),

  case filelib:is_regular(Filename) of
    true ->
      {ok, Source} = file:read_file(Filename),

      {A, B, C} = erlang:now(),

      TargetModule = list_to_atom(lists:flatten(io_lib:format("~p_~p_~p_~p", [App, A, B, C]))),

      ok = zview_compiler:compile(Source, TargetModule),

      Id = {App, Template},

      CompiledTemplate = #zview_tpl{
        id = Id,
        module = TargetModule, 
        name = Template,
        app = App,
        source = Source,
        filename = Filename,
        last_modified = filelib:last_modified(Filename)
      },

      true = ets:insert(?ZVIEW_TAB, CompiledTemplate),
      ok = gen_server:cast(zview_server, {compiled, Id}),

      render_template(CompiledTemplate, Vars);

    false ->
      template_not_found
  end.

render_template(#zview_tpl{module = Module, id = Id} = Config, Vars) ->
  VarStack = zview_runtime:new_var_stack(Vars),
  Result = Module:render(VarStack),
  ok = gen_server:cast(zview_server, {rendered, Id}),
  Result.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% this is not exaclty the right way, need to figure this out

setup_zview() ->
  application:start(zview),

  {file, Path} = code:is_loaded(zview),
  Root = filename:dirname(filename:dirname(Path)),
  TemplateRoot = filename:absname(filename:join([Root, "test", "templates"])),
  ok = zview:register(test, [{template_root, TemplateRoot}]),
  {ok, TemplateRoot}.


templates_test_() ->
  {ok, TemplateRoot} = setup_zview(),

  Files = filelib:fold_files(
    TemplateRoot,
    ".*\\.tpl",
    true,
    fun(Filename, List) ->
        [string:substr(Filename, length(TemplateRoot) + 2) | List]
    end,
    []
  ),

  [ fun() -> test_compile_and_render_template(TemplateRoot, File) end || File <- Files ].


test_compile_and_render_template(TemplateRoot, File) ->
  InputFile = filename:join([TemplateRoot, filename:basename(File, ".tpl") ++ ".input"]),

  Vars = case filelib:is_regular(InputFile) of
    true ->
      {ok, List} = file:consult(InputFile),
      List;

    false ->
      []
  end,

  ?debugFmt("~nTesting: ~p~nArgs: ~p~n", [File, Vars]),

  {ok, Output, Exports} = zview:render(test, File, Vars),
  ?debugVal(Exports),
  ?debugMsg(Output),
  ok.

-endif.
