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

-module(chef_db_compression_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA, <<"This is a test of the emergency broadcast system. "
                "This is only a test.">>).

compression_mysql_test_() ->
    {setup,
     fun() ->
             application:set_env(sqerl, db_type, mysql)
     end,
     fun(_) -> ok end,
      [ {"compressed for: " ++ atom_to_list(Type),
         fun() ->
                 assert_compressed(chef_db_compression:compress(Type, ?DATA))
         end} || Type <- [chef_role, chef_data_bag_item, chef_node]
      ]}.

compression_pgsql_test_() ->
    {setup,
     fun() ->
             application:set_env(sqerl, db_type, pgsql)
     end,
     fun(_) -> ok end,
      [ {"sometimes compressed", generator,
         fun() ->
                 Tests = [{chef_node, fun assert_not_compressed/1},
                          {chef_role, fun assert_compressed/1},
                          {chef_data_bag_item, fun assert_compressed/1}],
                 [ fun() ->
                           Assert(chef_db_compression:compress(Type, ?DATA))
                   end || {Type, Assert} <- Tests ]
         end}
      ]}.

assert_compressed(CData) ->
    ?assert(size(CData) =/= size(?DATA)),
    ?assertNot(?DATA =:= CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).

assert_not_compressed(CData) ->
    ?assertEqual(?DATA, CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).

