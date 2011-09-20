#!/usr/local/bin/thrift --gen erl --gen java --gen py
#----------------------------------------------------------------------
# Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
#
# File    : hibari.thrift
# Purpose : simplified Hibari "thrift" API for ntbf gdss plugin contract
#----------------------------------------------------------------------

namespace java com.hibari.thrift

enum StatusCode {
  OK = 0,
  ERROR = 8,
}

enum ErrorCode {
  UNKNOWN           = 1,
  UNKNOWN_ARGS      = 2,
  SERVICE_NOT_AVAIL = 3,
  NOT_IMPLEMENTED   = 4,
  TIME_OUT          = 5,
  TS_ERROR          = 6,
  KEY_EXISTS        = 101,
  KEY_NOT_EXISTS    = 102,
}

struct Add {
  1: required string table,
  2: required binary key,
  3: required binary value,
}

struct Replace {
  1: required string table,
  2: required binary key,
  3: required binary value,
}

struct Set {
  1: required string table,
  2: required binary key,
  3: required binary value,
}

struct Delete {
  1: required string table,
  2: required binary key,
  3: optional bool must_exist,
}

struct Get {
  1: required string table,
  2: required binary key,
  3: optional bool is_witness,
}

struct GetMany {
  1: required string table,
  2: required binary key,
  3: required i64 max_num,
  4: optional bool is_witness,
}

struct DoTxn {

}

struct DoAdd {
  1: required binary key,
  2: required binary value,
}

struct DoReplace {
  1: required binary key,
  2: required binary value,
}

struct DoSet {
  1: required binary key,
  2: required binary value,
}

struct DoDelete {
  1: required binary key,
  2: optional bool must_exist,
}

struct DoGet {
  1: required binary key,
  2: optional bool is_witness,
}

struct DoGetMany {
  1: required binary key,
  2: required i64 max_num,
  3: optional bool is_witness,
}

union DoOp {
  1: DoTxn     make_txn;
  2: DoAdd     make_add;
  3: DoReplace make_replace;
  4: DoSet     make_set;
  5: DoDelete  make_delete;
  6: DoGet     make_get;
  7: DoGetMany make_get_many;
}

struct Do {
  1: required string table,
  2: required list<DoOp> operations,
}

struct HibariResponse {
  1: optional i64 timestamp,
  2: optional binary key,
  3: optional binary value,
  4: optional StatusCode status,
}

struct HibariDoResponse {
  1: required list<HibariResponse> responses,
}

/**
 * Generic Exception thrown by Hibari
 * @param what = ErrorCode | A custom integer
 * @param why = a string to explain
 */
exception HibariException {
  1: optional i64 timestamp,
  2: required i32 what,
  3: required string why,
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
   * Add
   */
  HibariResponse Add(1: Add request)
      throws (1:HibariException ouch)

  /**
   * Replace
   */
  HibariResponse Replace(1: Replace request)
      throws (1:HibariException ouch)

  /**
   * Set
   */
  HibariResponse Set(1: Set request)
      throws (1:HibariException ouch)

  /**
   * Delete
   */
  HibariResponse Delete(1: Delete request)
      throws (1:HibariException ouch)

  /**
   * Get
   */
  HibariResponse Get(1: Get request)
      throws (1:HibariException ouch)

  /**
   * GetMany
   */
  HibariResponse GetMany(1: GetMany request)
      throws (1:HibariException ouch)

  /**
   * Do
   */
  HibariDoResponse Do(1: Do request)
      throws (1:HibariException ouch)

}
