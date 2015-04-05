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
add_kv(Table, Key, Value, PropList, #writeOptions{}=WriteOptions) ->
    io:format("add_ky - Table: ~p, Key: ~p, Value: ~p, PropList: ~p, WriteOptions: ~p~n",
              [Table, Key, Value, PropList, WriteOptions]),
    TimeStamp = 1000000,
    TimeStamp.


-spec do_ops(binary(), [#op{}], #doOptions{}) -> ok.
do_ops(Table, DoOpList, #doOptions{}=_DoOptions) ->
    io:format("do_ops - Table: ~p~n", [Table]),
    lists:map(fun do_op/1, DoOpList),
    undefined.

do_op(#op{txn=#doTransaction{}}) ->
    io:format("txn~n"),
    ok;
do_op(#op{add_kv=#doAdd{key=Key, value=Value, properties=PropList,
                        write_options=#writeOptions{}=WriteOptions}}) ->
    io:format("add_kv(~p, ~p, ~p, ~p)~n", [Key, Value, PropList, WriteOptions]),
    ok;
do_op(#op{replace_kv=#doReplace{}}) ->
    io:format("replace_kv()~n"),
    ok;
do_op(#op{set_kv=#doSet{}}) ->
    io:format("set_kv()~n"),
    ok;
do_op(#op{rename_kv=#doRename{}}) ->
    io:format("rename_kv()~n"),
    ok;
do_op(#op{get_kv=#doGet{}}) ->
    io:format("get_kv()~n"),
    ok;
do_op(#op{get_many=#doGetMany{}}) ->
    io:format("get_many()~n"),
    ok;
do_op(#op{delete_kv=#doDelete{}}) ->
    io:format("delete_kv()~n"),
    ok.
