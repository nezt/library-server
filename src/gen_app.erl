%% ===================================================================
%% Library APP
%% Copyright (c) 2012 All Rights Reserved.
%% Nestor Ocampo <anezt_oh@hotmail.com>
%% ===================================================================
-module(gen_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    library_server_sup:start_link().

stop(_State) ->
    ok.
