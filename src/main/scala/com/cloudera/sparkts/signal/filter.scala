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

import com.cloudera.sparkts.MatrixUtil.{toBreeze, fromBreeze}
import breeze.signal._
import breeze.linalg.DenseVector
import breeze.signal.support._
import org.apache.spark.mllib.linalg.{DenseVector => SDV, SparseVector => SSV, Vector => SV}
import org.apache.spark.mllib.linalg.{DenseMatrix => SDM, SparseMatrix => SSM, Matrix => SM}
/**
  * Created by LNICOLAS on 24/05/2016.
  */
object Filter {
  def filterSeries(data: SV, kernel: SV,
                   overhang: OptOverhang = OptOverhang.PreserveLength,
                   padding: OptPadding = OptPadding.Zero): SV = {
    val s = toBreeze(data).asInstanceOf[DenseVector[Double]]
    val k = toBreeze(kernel).asInstanceOf[DenseVector[Double]]
    val o = filter(s, k, overhang, padding)
    fromBreeze(o)
  }

}
