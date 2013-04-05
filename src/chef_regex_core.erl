%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Serdar Sutay <serdar@opscode.com>
%%
%% Regex used in erchef
%%
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_regex_core).

-export([
         regex_for/2
        ]).

-include_lib("chef_regex.hrl").

%% Regular Expression Macros
%% (Just to help DRY things up and make the usage patterns a little more clear)

%% We commonly need to fully anchor a regex
-define(ANCHOR_REGEX(Regex), "^" ++ Regex ++ "$").

%% Cookbooks, Recipes, and Roles have common naming conventions.
%% There is a concrete reference for role names at
%% http://wiki.opscode.com/display/chef/Roles#Roles-name.  Judging
%% from the cookbook names and recipes in the opscode/cookbooks
%% repository, this regular expression applies to them as well.
-define(NAME_REGEX, "[.[:alnum:]_-]+").

regex_for(object_name, Message) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX), Message).

-spec generate_regex_msg_tuple(regex_pattern(), re_msg()) -> {re_regex(), re_msg()}.
generate_regex_msg_tuple(Pattern, Message) ->
  Regex = generate_regex(Pattern),
  {Regex, Message}.

-spec generate_regex(regex_pattern()) -> re_regex().
generate_regex(Pattern) ->
  {ok, Regex} = re:compile(Pattern),
  Regex.
