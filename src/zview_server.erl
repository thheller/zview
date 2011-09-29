-module(zview_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("zview.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_) ->
  ets:new(?ZVIEW_TAB, [named_table, public, set, {keypos, #zview_tpl.id}]),
  ets:new(?ZVIEW_CFG, [named_table, protected, set, {keypos, #zview_cfg.app}]),
  {ok, none}.

handle_call({register, App, Options}, _From, State) ->
  case proplists:get_value(template_root, Options) of
    undefined ->
      {reply, missing_template_root, State};

    TemplateRoot ->
      case filelib:is_dir(TemplateRoot) of
        true ->
          ets:insert(?ZVIEW_CFG, #zview_cfg{app = App, template_root = TemplateRoot}),
          {reply, ok, State};

        false ->
          {reply, {not_a_directory, TemplateRoot}, State}
      end
  end;

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
