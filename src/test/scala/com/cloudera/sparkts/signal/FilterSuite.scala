/**
  * Copyright (c) 2015, Cloudera, Inc. All Rights Reserved.
  *
  * Cloudera, Inc. licenses this file to you under the Apache License,
  * Version 2.0 (the "License"). You may not use this file except in
  * compliance with the License. You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * This software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
  * CONDITIONS OF ANY KIND, either express or implied. See the License for
  * the specific language governing permissions and limitations under the
  * License.
  */

package com.cloudera.sparkts.signal

import org.apache.spark.mllib.linalg.Vectors
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class FilterSuite extends FunSuite {

  test("filter with asymmetric signal and kernel") {
    val series = Vectors.dense(Array(1.0, 1.0, 2.0, 2.0, 1.0, 2.0))
    val kernel = Vectors.dense(Array(1.0, 2.0))

    val filtered = Filter.filterSeries(series, kernel)
    val gold_standard = Vectors.dense(Array(3.0,4.0,6.0,5.0,4.0,4.0))
    filtered should be (gold_standard)
  }

}
