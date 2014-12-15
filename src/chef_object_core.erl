%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Serdar Sutay <serdar@opscode.com>
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

%% @doc General utility module for common functions that operate on
%% "Chef Objects", such as nodes, roles, etc.
-module(chef_object_core).

-include("chef_types.hrl").
-include_lib("ej/include/ej.hrl").

-export([strictly_valid/3
        ]).

%% These type specs are taken from ej. They are not in an exportable form
%% They are reproduced here to make dialyzer work for strictly_valid()
%% Perhaps, that means strictly_valid() should be moved into ej
-type ej_string_match() :: {'string_match', {re:mp(), _}}.
-type ej_fun_match() :: {fun_match, {fun((json_term()) -> ok | error),
                                        ej_json_type_name(), _}}.
-type ej_array_map() :: {array_map, ej_json_val_spec()}.

-type ej_object_map() :: {object_map, {{keys, ej_json_val_spec()},
                                       {values, ej_json_val_spec()}}}.

-type ej_json_spec() :: {[ej_json_spec_rule()]} | ej_object_map().
-type ej_json_spec_rule() :: {ej_json_key_spec(), ej_json_val_spec()}.
-type ej_json_key_spec() :: binary() | {opt, binary()}.
-type ej_json_val_spec() :: binary()             |
                            ej_json_type_name()  |
                            ej_string_match()    |
                            ej_fun_match()       |
                            ej_array_map()       |
                            ej_object_map()      |
                            {[ej_json_val_spec()]}.

%% Call this instead of ej:valid() if you want to validate specs
%% and check for invalid top-level keys. This will not check for
%% invalid keys beyond the top-level.
-spec strictly_valid(Constraints :: ej_json_spec(), ValidKeys :: [binary()],  Ejson :: json_object()) -> ok | #ej_invalid{}.
strictly_valid(Constraints, ValidKeys, Ejson) ->
    case allowed_keys(ValidKeys, Ejson) of
        ok ->
            ej:valid(Constraints, Ejson)
        % allowed_keys will throw, not return
    end.

allowed_keys(_ValidKeys, []) ->
    ok;
allowed_keys(ValidKeys, {List}) when is_list(List) ->
    allowed_keys(ValidKeys, List);
allowed_keys(ValidKeys, [{Item, _}|Rest]) ->
    case lists:member(Item, ValidKeys) of
        true -> allowed_keys(ValidKeys, Rest);
        _ ->
            throw({invalid_key, Item})
    end.
