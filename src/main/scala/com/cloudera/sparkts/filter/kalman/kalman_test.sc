import org.apache.commons.math3.filter._
import org.apache.commons.math3.linear._

type DblMatrix = Array[Array[Double]]
type DblVector = Array[Double]
type XYSeries = Array[(Double, Double)]

val a = new DblMatrix(2).map(_ =>  Array.fill(2)(1.0))
implicit def toMatrix(a: DblMatrix): Array2DRowRealMatrix = new Array2DRowRealMatrix(a)
val m = new Array2DRowRealMatrix(a)
val res = m.operate(Array(2.0,1.0))
res(1)
case class QRNoise(qr: (Double, Double), white: Double => Double) {
  // Compute the white noise for process Q
  private def q = white(qr._1)

  // Compute the white noise for measurement R
  private def r = white(qr._2)

  // Compute the white noise of the measurement
  def noisyQ: Array[Double] = Array[Double](q, q)

  // Compute the white noise on the measurement
  def noisyR: Array[Double] = Array[Double](r, r)
}

class DKalman(A: DblMatrix,
              B: DblMatrix,
              H: DblMatrix,
              P: DblMatrix)(implicit qrNoise: QRNoise) {


  // Process related white noise (mean = 0.0)
  private[this] val Q = new DblMatrix(A.size).map(_ =>
    Array.fill(A(0).size)(qrNoise.qr._1)
  )

  // Measurement related white noise (mean = 0.0)
  private[this] val R = new DblMatrix(A.size).map(_ =>
    Array.fill(A(0).size)(qrNoise.qr._2)
  )

  private var filter: KalmanFilter = _
  private var x: RealVector = _

  def initialize(input: DblVector): Unit = {
    val pModel = new DefaultProcessModel(A, B, Q, input, P)
    val mModel = new DefaultMeasurementModel(H, R)

    // Create a Kalman filter with a model pModel for
    // the process and a model mModel for the measurement.
    filter = new KalmanFilter(pModel, mModel)

    // Conversion to Apache Commons Math internal types
    x = new ArrayRealVector(input)
  }

  private def newState: DblVector = {

    // Update the filter with the predictive value for x
    // and update it with the A transition matrix with the
    // process noise qr.Q
    filter.predict
//    x = A.operate(x).add(qrNoise.noisyQ)
//
//    // Compute the measured value z with the new update value
//    // using the measurement-statement dependency matrix H
//    val z = H.operate(x).add(qrNoise.noisyR)
//
//    // Update the filter with the new estimated measure z
//    filter.correct(z)
    filter.getStateEstimation
  }

  def filter(xt: XYSeries): XYSeries = xt.map {
    case (x, y) => {

      // Initialize the filter
      initialize(Array[Double](x, y))

      // Extract the new state a two values vector
      val nState = newState
      (nState(0), nState(1))
    }
  }

  // Compute the least squared errors of two vectors of type 'RealVector'
  def lsError(x: RealVector, z: RealVector): Double = {
    val sumSqr = x.toArray.zip(z.toArray)
      .map(xz => (xz._1 - xz._2))
      .map( x => x*x).sum
    Math.sqrt(sumSqr)
  }
}


