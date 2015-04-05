#!/usr/local/bin/thrift --gen erl --gen go --gen hs --gen java --gen js:node --gen php --gen py --gen rb
#----------------------------------------------------------------------
# Copyright (c) 2008-2015 Hibari developers.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#----------------------------------------------------------------------

namespace java org.hibaridb.thrift

typedef i64 ExpTime
typedef i64 Timestamp

struct Property {
  1: required string key,
  2: optional string value,
}

enum KeepOrReplace {
  Keep    = 1,
  Replace = 2,
}

struct WriteOptions {
  1: optional ExpTime       exp_time,
  2: optional Timestamp     test_set,           // replace, set
  3: optional KeepOrReplace exp_time_directive, // replace, set
  4: optional KeepOrReplace attrib_directive,   // replace, set
  5: optional bool          value_in_ram,       // add, replace, set
}

struct DeleteOptions {
  1: optional Timestamp  test_set,
  2: optional bool       must_exist,
  3: optional bool       must_not_exist,
}

struct ReadOptions {
  1: optional Timestamp test_set,
  2: optional bool      is_witness,
  3: optional bool      get_all_attribs,
  4: optional bool      must_exist,
  5: optional bool      must_not_exist,
}

struct DoTransaction { }

struct DoAdd {
  1: required binary key,
  2: required binary value,
  3: optional list<Property> properties,
  4: required WriteOptions write_options,
}

struct DoReplace {
  1: required binary key,
  2: required binary value,
  3: optional list<Property> properties,
  4: required WriteOptions write_options,
}

struct DoSet {
  1: required binary key,
  2: required binary value,
  3: optional list<Property> properties,
  4: required WriteOptions write_options,
}

struct DoRename {
  1: required binary key,
  2: required binary new_key,
  3: optional list<Property> properties,
  4: optional WriteOptions write_options,
}

struct DoGet {
  1: required binary key,
  2: optional ReadOptions read_options,
}

struct DoGetMany {
  1: required binary key,
  2: required i32    max_keys,
  3: optional ReadOptions read_options,
}

struct DoDelete {
  1: required binary key,
  2: optional WriteOptions write_options,
}

union Op {
  1: optional DoTransaction txn,
  2: optional DoAdd         add_kv,
  3: optional DoReplace     replace_kv,
  4: optional DoSet         set_kv,
  5: optional DoRename      rename_kv,
  6: optional DoGet         get_kv,
  7: optional DoGetMany     get_many,
  8: optional DoDelete      delete_kv,
}

struct DoOptions {
  // TODO
}

struct GetResponse {
  1: required Timestamp      timestamp,
  2: optional binary         value,
  3: optional ExpTime        exp_time,
  4: optional list<Property> proplist,
}

struct GetManyResponse {
  1: required list<GetResponse> records,
  2: required bool              is_truncated,
}

struct DoFailure {
  // TODO
}

struct DoResponse {
  // TODO
}

exception ServiceNotAvailableException {}

exception NotImplementedException {}

exception TimedOutException {}

exception TSErrorException {}

exception KeyExistisException {}

exception KeyNotExistsException {}

exception InvalidOptionPresentException {
  1: required string option,
  2: required string value,
}

exception TransactionFailureException {
  1: required i32       do_op_index,
  2: required DoFailure do_failure,
}

service Hibari {

  /**
   * Check connection availability / keepalive
   */
  oneway void keepalive()

  /**
   * Hibari Server Info
   */
  string info()

  /**
   * Hibari Description
   */
  string description()

  /**
   * Hibari Contract
   */
  string contract()

  /**
   * add
   */
  Timestamp add_kv(1: required string table,
                   2: required binary key,
                   3: required binary value,
                   4: required list<Property> properties,
                   5: required WriteOptions write_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: InvalidOptionPresentException invalid_opt,
              4: KeyExistisException key_exsits)

  /**
   * replace
   */
  Timestamp replace_kv(1: required string table,
                       2: required binary key,
                       3: required binary value,
                       4: required list<Property> properties,
                       5: required WriteOptions write_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: InvalidOptionPresentException invalid_opt,
              4: KeyNotExistsException key_exsits,
              5: TSErrorException ts_error)

  /**
   * rename
   */
  Timestamp rename_kv(1: required string table,
                      2: required binary key,
                      3: required binary new_key,
                      4: required list<Property> properties,
                      5: required WriteOptions write_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: InvalidOptionPresentException invalid_opt,
              4: KeyNotExistsException key_not_exsits,
              5: TSErrorException ts_error)

  /**
   * set
   */
  Timestamp set_kv(1: required string table,
                   2: required binary key,
                   3: required binary value,
                   4: required list<Property> properties,
                   5: required WriteOptions write_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: InvalidOptionPresentException invalid_opt,
              4: TSErrorException ts_error)

  /**
   * delete
   */
  void delete_kv(1: required string table,
                 2: required binary key,
                 3: required WriteOptions write_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: InvalidOptionPresentException invalid_opt,
              4: KeyNotExistsException key_not_exsits,
              5: TSErrorException ts_error)

  /**
   * get
   */
  GetResponse get_kv(1: required string table,
                     2: required binary key,
                     3: required ReadOptions read_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: KeyNotExistsException key_not_exsits,
              4: TSErrorException ts_error)

  /**
   * get_many
   */
  GetManyResponse get_many(1: required string table,
                           2: required binary key,
                           3: required i32    max_keys,
                           4: required ReadOptions read_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: TSErrorException ts_error)

  /**
   * do
   */
  DoResponse do_ops(1: required string    table,
                    2: required list<Op>  do_operations,
                    3: required DoOptions do_options)
      throws (1: ServiceNotAvailableException not_avail,
              2: TimedOutException timeout,
              3: TransactionFailureException txn_fail)

}
