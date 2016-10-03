/**
  * Created by LNICOLAS on 23/09/2016.
  */

package com.cloudera.sparkts.filter

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import com.cloudera.sparkts.filter.Utils._

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


class KalmanFilter (config: KalmanConfig)
             (implicit qrNoise: QRNoise) {
  def this(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix) (implicit qrNoise: QRNoise) =
    this(KalmanConfig(A, B, H, P))(qrNoise)

  /**
    * Process related white noise (mean = 0.0)
    */
  private[this] val Q: DblMatrix = DenseMatrix.rand(config.A.rows, config.A.cols) * qrNoise.qr._1

  /**
    * Measurement related white noise (mean = 0.0)
    */
  private[this] val R: DblMatrix = DenseMatrix.rand(config.A.rows, config.A.cols) * qrNoise.qr._2

  def filter(x: DblVector, u: DblVector, z: DblVector): (DblVector, DblMatrix) = {
    // Prediction for state vector and covariance
    var x_ = config.A * x + config.B * u
    var P = config.A * config.P * config.A.t + Q

    // Compute Kalman gain factor
    val K = config.P*config.H.t * inv(config.H*config.P*config.H.t + R)

    // Correction based on observation
    x_ = x_ + K * (z - config.H * x_)
    P = P - K*config.H*config.P
    (x_, P)
  }
}

object KalmanFilter {
  /**
    * Constructor for the Kalman filter with Control Matrix B
    * @param A State transition matrix  [
    * @param B Control state matrix
    * @param H Matrix that defines the dependency of the measurement on the state of the system
    * @param P Covariance error matrix
    * @param qrNoise Implicit value representing the white noise for the process Q and the measurement P
    */
  def apply(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)
           (implicit qrNoise: QRNoise): KalmanFilter = new KalmanFilter(A, B, H,P)(qrNoise)

  /**
    * Constructor for the Kalman filter without Control Matrix B
    * @param A State transition matrix  [
    * @param H Matrix that defines the dependency of the measurement on the state of the system
    * @param P Covariance error matrix
    * @param qrNoise Implicit value representing the white noise for the process Q and the measurement P
    */
  def apply(A: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): KalmanFilter =
    new KalmanFilter(A, DenseMatrix.zeros(A.rows, A.cols), H, P)(qrNoise)
}
