-module(zview_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, register_repo/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("zview.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-record(zview_repo, { id, module, state }).

register_repo(Id, Module, State) when is_atom(Id) and is_atom(Module) ->
  gen_server:call(?SERVER, {register_repo, Id, Module, State}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_) ->
  ets:new(?ZVIEW_CFG, [named_table, protected, set, {keypos, #zview_repo.id}]),

  {ok, Options} = application:get_env(zview, options),

  case application:get_env(zview, repos) of
    {ok, Repos} when is_list(Repos) ->
      ok = setup_repos(Repos, Options),
      {ok, none};

    {ok, Repos} ->
      {error, {expected_list, Repos}};

    undefined ->
      {error, no_repos}
  end.

handle_call({register_repo, Id, Module, State}, _From, State) ->
  ok = store_repo(Id, Module, State),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


store_repo(Id, Module, Repo) ->
  true = ets:insert(?ZVIEW_CFG, #zview_repo{id = Id, module = Module, state = Repo}),
  ok.

setup_repos([], _) ->
  ok;

% should each repo have its own sup?
setup_repos([{Id, {folder, Root}} | More], Options) ->
  {ok, Repo} = zview_folder:init(Id, Root, Options),
  ok = store_repo(Id, zview_folder, Repo),
  setup_repos(More, Options).
