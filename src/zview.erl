-module(zview).

-include("zview.hrl").

-export([
    start/0,
    stop/0,
    init_var_stack/1,
    render/3
  ]).

-record(repo, {type, config}).

start() ->
  application:start(zview).

stop() ->
  application:stop(zview).

render(RepoId, Name, {List}) when is_list(List) ->
  render(RepoId, Name, init_var_stack(List));

render(RepoId, Name, List) when is_list(List) ->
  render(RepoId, Name, init_var_stack(List));

render(RepoId, Name, {var_stack, _, _, _} = VarStack) ->
  case ets:lookup(?ZVIEW_CFG, RepoId) of
    [{zview_repo, _, Config}] ->
      zview_folder:render(Config, safe_template_name(Name), VarStack);

    [] ->
      repo_not_found
  end.

init_var_stack(Vars) ->
  {var_stack, root, Vars, root}.

safe_template_name(Bin) when is_binary(Bin) ->
  safe_template_name(binary_to_list(Bin));

safe_template_name(Atom) when is_atom(Atom) ->
  safe_template_name(atom_to_list(Atom));

% strip out all leading path stuff, so we cant get out of the repo by accident
safe_template_name(Name) ->
  re:replace(Name, "^[~/.]+", "", [{return, list}]).
