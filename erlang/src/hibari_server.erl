%%----------------------------------------------------------------------
%% Copyright (c) 2015 Hibari developers.  All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%----------------------------------------------------------------------

-module(hibari_server).

-compile(export_all).

-include("hibari_thrift.hrl").

-define(KEEP,    ?hibari_KeepOrReplace_Keep).
-define(REPLACE, ?hibari_KeepOrReplace_Replace).

start_link(Port) ->
    thrift_server:start_link(Port, hibari_thrift, ?MODULE).

handle_function(Function, Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok ->
            ok;
        Else ->
            {reply, Else}
    end.

-spec add_kv(binary(), binary(), binary(), [#property{}], #writeOptions{}) -> integer() | term().
add_kv(Table, Key, Value, PropList, #writeOptions{}=Opts) ->
    Properties = properties(PropList),
    WriteOptions = parse_write_options(Opts),
    io:format("add_ky - Table: ~p, Key: ~p, Value: ~p, PropList: ~p, WriteOptions: ~p~n",
              [Table, Key, Value, Properties, WriteOptions]),
    TimeStamp = 1000000,
    TimeStamp.


-spec do_ops(binary(), [#op{}], #doOptions{}) -> ok.
do_ops(Table, DoOpList, #doOptions{}=_DoOptions) ->
    io:format("do_ops - Table: ~p~n", [Table]),
    lists:map(fun do_op/1, DoOpList),
    undefined.

-spec do_op(#op{}) -> ok.
do_op(#op{txn=#doTransaction{},
          add_kv=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    io:format("txn~n"),
    ok;
do_op(#op{add_kv=#doAdd{key=Key, value=Value, properties=PropList, write_options=Opts},
          txn=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    Properties = properties(PropList),
    WriteOptions = parse_write_options(Opts),
    io:format("add_kv(~p, ~p, ~p, ~p)~n", [Key, Value, Properties, WriteOptions]),
    ok;
do_op(#op{replace_kv=#doReplace{key=Key, value=Value, properties=PropList, write_options=Opts},
          txn=undefined,
          add_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    Properties = properties(PropList),
    WriteOptions = parse_write_options(Opts),
    io:format("replace_kv(~p, ~p, ~p, ~p)~n", [Key, Value, Properties, WriteOptions]),
    ok;
do_op(#op{set_kv=#doSet{},
          txn=undefined,
          add_kv=undefined,
          replace_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    io:format("set_kv()~n"),
    ok;
do_op(#op{rename_kv=#doRename{},
          txn=undefined,
          add_kv=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          get_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    io:format("rename_kv()~n"),
    ok;
do_op(#op{get_kv=#doGet{},
          txn=undefined,
          add_kv=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_many=undefined,
          delete_kv=undefined}) ->
    io:format("get_kv()~n"),
    ok;
do_op(#op{get_many=#doGetMany{},
          txn=undefined,
          add_kv=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          delete_kv=undefined}) ->
    io:format("get_many()~n"),
    ok;
do_op(#op{delete_kv=#doDelete{},
          txn=undefined,
          add_kv=undefined,
          replace_kv=undefined,
          set_kv=undefined,
          rename_kv=undefined,
          get_kv=undefined,
          get_many=undefined}) ->
    io:format("delete_kv()~n"),
    ok.

%% @TODO
-type write_option() :: term().
-type exp_time() :: integer().

-spec parse_write_options(#writeOptions{}) -> {exp_time(), [write_option()]}.
parse_write_options(#writeOptions{exp_time=ExpTime,
                                  test_set=TestSet,
                                  exp_time_directive=ExpTimeDirect,
                                  attrib_directive=AttrbDirect,
                                  value_in_ram=ValueInRam}) ->
    WriteOptions =
        [ {test_set, TestSet} || TestSet =/= undefined ]
        ++ [ {exp_time_directive, keep_or_replace(ExpTimeDirect)} || ExpTimeDirect =/= undefined ]
        ++ [ {attrb_directive,    keep_or_replace(AttrbDirect)}   || AttrbDirect =/= undefined ]
        ++ [ value_in_ram || ValueInRam ],
    {exp_time(ExpTime), WriteOptions}.


-spec exp_time('undefined' | non_neg_integer()) -> non_neg_integer().
exp_time(undefined) ->
    0;
exp_time(Int) ->
    Int.

-spec keep_or_replace(?KEEP..?REPLACE) -> 'keep' | 'replace'.
keep_or_replace(?KEEP) ->
    keep;
keep_or_replace(?REPLACE) ->
    replace.

-spec properties('undefined' | [#property{}]) -> [{binary(), binary()}].
properties(undefined) ->
    [];
properties(PropList) ->
    [ {list_to_binary(K), list_to_binary(V)} || {K, V} <- PropList ].
