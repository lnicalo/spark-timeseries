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

package com.cloudera.sparkts.models

import com.cloudera.sparkts.MatrixUtil._
import breeze.linalg.{DenseVector => BDV}
import org.apache.spark.mllib.linalg.{DenseVector, Vector}
import breeze.signal.fourierTr
import breeze.math.Complex
/**
 * Fits an Exponentially Weight Moving Average model (EWMA) (aka. Simple Exponential Smoothing) to
 * a time series. The model is defined as S_t = (1 - a) * X_t + a * S_{t - 1}, where a is the
 * smoothing parameter, X is the original series, and S is the smoothed series. For more
 * information, please see https://en.wikipedia.org/wiki/Exponential_smoothing.
 */
object FFT {
  /**
   * Fits an EWMA model to a time series. Uses the first point in the time series as a starting
   * value. Uses sum squared error as an objective function to optimize to find smoothing parameter
   * The model for EWMA is recursively defined as S_t = (1 - a) * X_t + a * S_{t-1}, where
   * a is the smoothing parameter, X is the original series, and S is the smoothed series
   * Note that the optimization is performed as unbounded optimization, although in its formal
   * definition the smoothing parameter is <= 1, which corresponds to an inequality bounded
   * optimization. Given this, the resulting smoothing parameter should always be sanity checked
   * https://en.wikipedia.org/wiki/Exponential_smoothing
   * @param ts the time series to which we want to fit an EWMA model
   * @return EWMA model
   */
  def fitModel(ts: DenseVector): FFTModel = {
    val x = dvSparkToBreeze(ts)
    val coeffs = fourierTr(x)
    new FFTModel(coeffs.toArray)
  }
}

class FFTModel(val coeffients: Array[Complex]) extends TimeSeriesModel {

  // def this(coef: Complex) = this(Array(coef))

  override def removeTimeDependentEffects(ts: Vector, dest: Vector = null): Vector = {
    val arr = dest.toArray
    new DenseVector(arr)
  }

  override def addTimeDependentEffects(ts: Vector, dest: Vector): Vector = {
    val arr = dest.toArray
    new DenseVector(arr)
  }
}
