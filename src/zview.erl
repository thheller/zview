-module(zview).

-include("zview.hrl").

-export([start/0, open_repository/2, render/3]).

-record(repo, {type, config}).

start() ->
  application:start(zview).

open_repository(folder, Options) ->
  {ok, Repo} = zview_folder:init(Options),
  {ok, #repo{type = zview_folder, config = Repo}};

open_repository(Type, Options) ->
  unknown_type.

render(Repo, Name, Vars) when is_binary(Name) ->
  render(Repo, binary_to_list(Name), Vars);

render(Repo, Name, Vars) when is_atom(Name) ->
  render(Repo, atom_to_list(Name), Vars);

render(#repo{type = RepoType, config = Config}, Name, Vars) ->
  RepoType:render(Config, Name, Vars).

