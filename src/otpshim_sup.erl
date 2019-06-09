%%------------------------------------------------------------------------------
%% @doc otpshim_sup
%%
%% otpshim top-level supervisor
%%
%% @copyright Jeremey L Barrett <jlb@rot26.com>
%%------------------------------------------------------------------------------
-module(otpshim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%%==============================================================================
%% API functions
%%==============================================================================

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
