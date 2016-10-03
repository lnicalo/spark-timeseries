package com.cloudera.sparkts.filter

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import com.cloudera.sparkts.filter.Utils._

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

  def I(n: Int) = DenseMatrix.eye[Double](n)

  type DblMatrix = DenseMatrix[Double]
  type DblVector = DenseVector[Double]
  type DblPair = (Double, Double)
}



case class KalmanConfig(A: Utils.DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)
case class QRNoise(qr: DblPair, profile: Double => Double = normal) {
  /**
    * Compute the white noise for process Q
    * @return white noise (stochastic) value for process Q
    */
  private def q = profile(qr._1)

  /**
    * Compute the white noise for measurement R
    * @return white noise (stochastic) value for measurement R
    */
  private def r = profile(qr._2)

  /**
    * Compute the white noise of the measurement in Kalman filter
    * @return Array of two process noise value
    */
  lazy val noisyQ: DblVector = DenseVector[Double](q, q)

  /**
    * Compute the white noise of the measurement in Kalman filter
    * @return Array of two measurement noise value
    */
  lazy val noisyR: DblVector = DenseVector[Double](r, r)
}

class Kalman (config: KalmanConfig)
             (implicit qrNoise: QRNoise) {
  def this(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)
          (implicit qrNoise: QRNoise) =
    this(KalmanConfig(A, B, H, P))(qrNoise)
  private[this] val Q: DblMatrix =
    DenseMatrix.rand(config.A.rows, config.A.cols) * qrNoise.qr._1
  private[this] val R: DblMatrix =
    DenseMatrix.rand(config.A.rows, config.A.cols) * qrNoise.qr._2

  def filter(x: DblVector, u: DblVector, z: DblVector): (DblVector, DblMatrix) = {
    // Prediction for state vector and covariance
    var x_ = config.A * x + config.B * u
    var P = config.A * config.P * config.A.t + Q

    // Compute Kalman gain factor
    val K = config.P*config.H.t * inv(config.H*config.P*config.H.t + R)

    // Correction based on observation
    x_ = x_ + K * (z - config.H* x_)
    P = P - K*config.H*config.P
    (x_, P)
  }
}
