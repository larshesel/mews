
-module(mews_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Processes = [web_server(), statistics_module()],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.


web_server() ->
    Cfg = [{ip,      {127,0,0,1}},
	   {port,    8008}],
    ModName = mews_webserver,
    Mfa = {mews_webserver, start, [Cfg]},
    {ModName, Mfa, permanent, 5000, worker, dynamic}.


statistics_module() ->
    Cfg = [],
    ModName = mews_statistics,
    Mfa = {mews_statistics, start_link, [Cfg]},
    {ModName, Mfa, permanent, 5000, worker, dynamic}.

%%    {ok, { {one_for_one, 5, 10}, []} }.

