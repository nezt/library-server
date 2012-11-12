%% ===================================================================
%% Library SUP
%% Copyright (c) 2012 All Rights Reserved.
%% Nestor Ocampo <anezt_oh@hotmail.com>
%% ===================================================================
-module(supervisor_library).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Flags = {one_for_one, 1000, 3600},
    {ok, { Flags, [child(library_server)]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child(Module) ->
    {Module, {Module,start_link,[]},permanent,2000,worker,[Module]}.

