%%
%% Autogenerated by Thrift Compiler (0.9.1)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(hibari_types).

-include("hibari_types.hrl").

-export([struct_info/1, struct_info_ext/1]).

struct_info('property') ->
  {struct, [{1, string},
          {2, string}]}
;

struct_info('writeOptions') ->
  {struct, [{1, i64},
          {2, i64},
          {3, i32},
          {4, i32},
          {5, bool}]}
;

struct_info('deleteOptions') ->
  {struct, [{1, i64},
          {2, bool},
          {3, bool}]}
;

struct_info('readOptions') ->
  {struct, [{1, i64},
          {2, bool},
          {3, bool},
          {4, bool},
          {5, bool}]}
;

struct_info('doTransaction') ->
  {struct, []}
;

struct_info('doAdd') ->
  {struct, [{1, string},
          {2, string},
          {3, {list, {struct, {'hibari_types', 'property'}}}},
          {4, {struct, {'hibari_types', 'writeOptions'}}}]}
;

struct_info('doReplace') ->
  {struct, [{1, string},
          {2, string},
          {3, {list, {struct, {'hibari_types', 'property'}}}},
          {4, {struct, {'hibari_types', 'writeOptions'}}}]}
;

struct_info('doSet') ->
  {struct, [{1, string},
          {2, string},
          {3, {list, {struct, {'hibari_types', 'property'}}}},
          {4, {struct, {'hibari_types', 'writeOptions'}}}]}
;

struct_info('doRename') ->
  {struct, [{1, string},
          {2, string},
          {3, {list, {struct, {'hibari_types', 'property'}}}},
          {4, {struct, {'hibari_types', 'writeOptions'}}}]}
;

struct_info('doGet') ->
  {struct, [{1, string},
          {2, {struct, {'hibari_types', 'readOptions'}}}]}
;

struct_info('doGetMany') ->
  {struct, [{1, string},
          {2, i32},
          {3, {struct, {'hibari_types', 'readOptions'}}}]}
;

struct_info('doDelete') ->
  {struct, [{1, string},
          {2, {struct, {'hibari_types', 'writeOptions'}}}]}
;

struct_info('op') ->
  {struct, [{1, {struct, {'hibari_types', 'doTransaction'}}},
          {2, {struct, {'hibari_types', 'doAdd'}}},
          {3, {struct, {'hibari_types', 'doReplace'}}},
          {4, {struct, {'hibari_types', 'doSet'}}},
          {5, {struct, {'hibari_types', 'doRename'}}},
          {6, {struct, {'hibari_types', 'doGet'}}},
          {7, {struct, {'hibari_types', 'doGetMany'}}},
          {8, {struct, {'hibari_types', 'doDelete'}}}]}
;

struct_info('doOptions') ->
  {struct, []}
;

struct_info('getResponse') ->
  {struct, [{1, i64},
          {2, string},
          {3, i64},
          {4, {list, {struct, {'hibari_types', 'property'}}}}]}
;

struct_info('getManyResponse') ->
  {struct, [{1, {list, {struct, {'hibari_types', 'getResponse'}}}},
          {2, bool}]}
;

struct_info('doFailure') ->
  {struct, []}
;

struct_info('doResponse') ->
  {struct, []}
;

struct_info('serviceNotAvailableException') ->
  {struct, []}
;

struct_info('notImplementedException') ->
  {struct, []}
;

struct_info('timedOutException') ->
  {struct, []}
;

struct_info('tSErrorException') ->
  {struct, []}
;

struct_info('keyExistisException') ->
  {struct, []}
;

struct_info('keyNotExistsException') ->
  {struct, []}
;

struct_info('invalidOptionPresentException') ->
  {struct, [{1, string},
          {2, string}]}
;

struct_info('transactionFailureException') ->
  {struct, [{1, i32},
          {2, {struct, {'hibari_types', 'doFailure'}}}]}
;

struct_info('i am a dummy struct') -> undefined.

struct_info_ext('property') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, optional, string, 'value', undefined}]}
;

struct_info_ext('writeOptions') ->
  {struct, [{1, optional, i64, 'exp_time', undefined},
          {2, optional, i64, 'test_set', undefined},
          {3, optional, i32, 'exp_time_directive', undefined},
          {4, optional, i32, 'attrib_directive', undefined},
          {5, optional, bool, 'value_in_ram', undefined}]}
;

struct_info_ext('deleteOptions') ->
  {struct, [{1, optional, i64, 'test_set', undefined},
          {2, optional, bool, 'must_exist', undefined},
          {3, optional, bool, 'must_not_exist', undefined}]}
;

struct_info_ext('readOptions') ->
  {struct, [{1, optional, i64, 'test_set', undefined},
          {2, optional, bool, 'is_witness', undefined},
          {3, optional, bool, 'get_all_attribs', undefined},
          {4, optional, bool, 'must_exist', undefined},
          {5, optional, bool, 'must_not_exist', undefined}]}
;

struct_info_ext('doTransaction') ->
  {struct, []}
;

struct_info_ext('doAdd') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, string, 'value', undefined},
          {3, optional, {list, {struct, {'hibari_types', 'property'}}}, 'properties', []},
          {4, required, {struct, {'hibari_types', 'writeOptions'}}, 'write_options', #writeOptions{}}]}
;

struct_info_ext('doReplace') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, string, 'value', undefined},
          {3, optional, {list, {struct, {'hibari_types', 'property'}}}, 'properties', []},
          {4, required, {struct, {'hibari_types', 'writeOptions'}}, 'write_options', #writeOptions{}}]}
;

struct_info_ext('doSet') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, string, 'value', undefined},
          {3, optional, {list, {struct, {'hibari_types', 'property'}}}, 'properties', []},
          {4, required, {struct, {'hibari_types', 'writeOptions'}}, 'write_options', #writeOptions{}}]}
;

struct_info_ext('doRename') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, string, 'new_key', undefined},
          {3, optional, {list, {struct, {'hibari_types', 'property'}}}, 'properties', []},
          {4, optional, {struct, {'hibari_types', 'writeOptions'}}, 'write_options', #writeOptions{}}]}
;

struct_info_ext('doGet') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, optional, {struct, {'hibari_types', 'readOptions'}}, 'read_options', #readOptions{}}]}
;

struct_info_ext('doGetMany') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, required, i32, 'max_keys', undefined},
          {3, optional, {struct, {'hibari_types', 'readOptions'}}, 'read_options', #readOptions{}}]}
;

struct_info_ext('doDelete') ->
  {struct, [{1, required, string, 'key', undefined},
          {2, optional, {struct, {'hibari_types', 'writeOptions'}}, 'write_options', #writeOptions{}}]}
;

struct_info_ext('op') ->
  {struct, [{1, optional, {struct, {'hibari_types', 'doTransaction'}}, 'txn', #doTransaction{}},
          {2, optional, {struct, {'hibari_types', 'doAdd'}}, 'add_kv', #doAdd{}},
          {3, optional, {struct, {'hibari_types', 'doReplace'}}, 'replace_kv', #doReplace{}},
          {4, optional, {struct, {'hibari_types', 'doSet'}}, 'set_kv', #doSet{}},
          {5, optional, {struct, {'hibari_types', 'doRename'}}, 'rename_kv', #doRename{}},
          {6, optional, {struct, {'hibari_types', 'doGet'}}, 'get_kv', #doGet{}},
          {7, optional, {struct, {'hibari_types', 'doGetMany'}}, 'get_many', #doGetMany{}},
          {8, optional, {struct, {'hibari_types', 'doDelete'}}, 'delete_kv', #doDelete{}}]}
;

struct_info_ext('doOptions') ->
  {struct, []}
;

struct_info_ext('getResponse') ->
  {struct, [{1, required, i64, 'timestamp', undefined},
          {2, optional, string, 'value', undefined},
          {3, optional, i64, 'exp_time', undefined},
          {4, optional, {list, {struct, {'hibari_types', 'property'}}}, 'proplist', []}]}
;

struct_info_ext('getManyResponse') ->
  {struct, [{1, required, {list, {struct, {'hibari_types', 'getResponse'}}}, 'records', []},
          {2, required, bool, 'is_truncated', undefined}]}
;

struct_info_ext('doFailure') ->
  {struct, []}
;

struct_info_ext('doResponse') ->
  {struct, []}
;

struct_info_ext('serviceNotAvailableException') ->
  {struct, []}
;

struct_info_ext('notImplementedException') ->
  {struct, []}
;

struct_info_ext('timedOutException') ->
  {struct, []}
;

struct_info_ext('tSErrorException') ->
  {struct, []}
;

struct_info_ext('keyExistisException') ->
  {struct, []}
;

struct_info_ext('keyNotExistsException') ->
  {struct, []}
;

struct_info_ext('invalidOptionPresentException') ->
  {struct, [{1, required, string, 'option', undefined},
          {2, required, string, 'value', undefined}]}
;

struct_info_ext('transactionFailureException') ->
  {struct, [{1, required, i32, 'do_op_index', undefined},
          {2, required, {struct, {'hibari_types', 'doFailure'}}, 'do_failure', #doFailure{}}]}
;

struct_info_ext('i am a dummy struct') -> undefined.

