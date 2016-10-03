/**
  * Created by LNICOLAS on 23/09/2016.
  */

package com.cloudera.sparkts.filter

import breeze.linalg.{DenseMatrix, DenseVector}


object Utils {
  val STATS_EPS = 1e-12
  val INV_SQRT_2PI = 1.0/Math.sqrt(2.0*Math.PI)
  def gauss(mean: Double, stdDev: Double, x: Double): Double = {
    require(Math.abs(stdDev) >= STATS_EPS,
      s"Stats.gauss, Gauss standard deviation $stdDev is close to zero")

    val y = (x - mean)/stdDev
    INV_SQRT_2PI*Math.exp(-0.5*y*y)/stdDev
  }

  val normal = gauss(0.0, 1.0, _: Double)

  def I(n: Int): DblMatrix = DenseMatrix.eye[Double](n)

  type DblMatrix = DenseMatrix[Double]
  type DblVector = DenseVector[Double]
  type DblPair = (Double, Double)
}