%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(test_utils).

-export([
         mock/1,
         mock/2,
         unmock/1,
         validate_modules/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% helper functions for configuring mocking.

%%@doc setup mocking for a list of modules.  This would normally be
%% called in the setup/0 method. You can optionally pass in a list of
%% meck options. See meck docs for details at
%% http://doc.erlagner.org/meck/meck.html#new-2
mock(Modules) ->
  mock(Modules, []).
mock(Modules, Opts) ->
    [ meck:new(M, Opts) || M <- Modules ].

%%@doc Unload a list of mocked modules
unmock(Modules) ->
    [ meck:unload(M) || M <-Modules ].

%% @doc Validate the state of the mock modules and raise
%% an eunit error if the modules have not been used according to
%% expectations
validate_modules(Modules) ->
    [?assert(meck:validate(M)) || M <- Modules].

