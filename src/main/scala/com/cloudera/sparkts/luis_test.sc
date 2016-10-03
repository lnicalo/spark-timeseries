import breeze.linalg.{DenseVector, DenseMatrix, inv}
import java.lang.Math.{sqrt, exp}

type VectorDouble = DenseVector[Double]
type MatrixDouble = DenseMatrix[Double]

def eye(n: Int): MatrixDouble = {
  DenseMatrix.eye[Double](n)
}

def zeros(n: Int, m: Int): MatrixDouble = {
  DenseMatrix.zeros[Double](n,m)
}
val n = 2

class State(var x: Array[VectorDouble], var P: Array[MatrixDouble]) {
  // Constructor for State
  def this(state: State) = this(state.x, state.P)

  // filtering state
  def xkk(): VectorDouble = x(1)
  // filtering covariance
  def Pkk(): MatrixDouble = P(1)
  // prediction state
  def xkk_1(): VectorDouble = x(0)
  // predition covariance
  def Pkk_1(): MatrixDouble = P(0)
}

trait Dynamics {
  def sigma: Double = 50
  def v_max: Double = 335.41
  def q_max: Double = 9.14
  def theta: Double = 60

  // F is the Dynamic/Evolution/State Transition Matrix
  def F(deltaT: Double): MatrixDouble = {
    DenseMatrix.vertcat(
      DenseMatrix.horzcat(eye(n), eye(n) * deltaT , eye(n) * (deltaT * deltaT/2)),
      DenseMatrix.horzcat(zeros(n,n), eye(n), eye(n) * deltaT),
      DenseMatrix.horzcat(zeros(n,n), zeros(n,n), eye(n) * exp(-deltaT/theta)))
  }

  // Q is the Dynamic Covariance Matrix
  def Q(deltaT: Double): MatrixDouble =
    DenseMatrix.vertcat(
      DenseMatrix.zeros[Double](4, 6),
      DenseMatrix.horzcat(zeros(n, 2 * n),
        eye(n))
    ) * (q_max * q_max *( 1 - exp(-2 * deltaT/theta)))
}

class Initialization extends Dynamics {
  // R is measurement Covariance Matrix
  def R : MatrixDouble = eye(n) * sigma * sigma

  // initial State x(0|0)
  def init_state: VectorDouble = DenseVector[Double](0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  // initial Covariance P(0|0)
  def init_covar: MatrixDouble = {
    DenseMatrix.vertcat(
      DenseMatrix.horzcat( R, zeros(n, 2 * n)),
      DenseMatrix.horzcat( zeros(n,n), eye(n) * v_max * v_max, zeros(n,n)),
      DenseMatrix.horzcat( zeros(n, 2 * n), eye(n) * q_max * q_max)
    )
  }

  // number of iterations
  def iter: Int = 100
  // Observabilty Matrix
  def H: MatrixDouble = DenseMatrix.horzcat( eye(n), zeros(n,n), zeros(n,n))
}

class Transition(init: Initialization) extends Dynamics {
  var state = init.init_state
  // state transition
  def state_trans(deltaT: Double) = {
    state = F(deltaT) * state
  }
}

class SensorModel(val init:Initialization, val trans: Transition) extends Dynamics {
  val H = init.H
  val R = init.R

  // Sensor measurement
  def Z = List( H * trans.state + noise )

  // White Noise for measurement
  def noise = {
    DenseVector.rand(R.cols) * sigma
  }

}

class Kalman(init: Initialization, sensor: SensorModel) extends Dynamics {

  val R = init.R  // Measurement Covariance
  val H = init.H  // Observation Matrix

  // Kalman filter implementation
  def kalmanfilter(x: VectorDouble, P: MatrixDouble, deltaT: Double): State = {
    // Prediction cycle
    val xkk_1 = F(deltaT) * x
    val Pkk_1 = F(deltaT) * P * F(deltaT).t + Q(deltaT)

    // Correction cycle
    val vkk_1 = sensor.Z.last - H * xkk_1
    val Skk_1 = H * Pkk_1 * H.t + R
    val Wkk_1 = Pkk_1 * H.t * inv(Skk_1)

    val xkk = xkk_1 + Wkk_1 * vkk_1
    val Pkk = Pkk_1 - Wkk_1 * Skk_1 * Wkk_1.t

    val state = Array(xkk_1,xkk)
    val covar = Array(Pkk_1,Pkk)

    new State(state, covar)
  }
}

val init = new Initialization()
val trans = new Transition(init)
val sensor = new SensorModel(init,trans)
val kalman = new Kalman(init,sensor)

var state = new State(Array(init.init_state, init.init_state),
  Array(init.init_covar,init.init_covar))

val deltaT: Double = 1.0
val iter = init.iter

trans.state_trans(deltaT) 
var Z = sensor.Z
state = kalman.kalmanfilter(state.xkk, state.Pkk, deltaT)
var xkk_1 = state.xkk_1
var xkk = state.xkk
state.x.foreach(println)

trans.state_trans(deltaT)
Z = sensor.Z
state = kalman.kalmanfilter(state.xkk, state.Pkk, deltaT)
xkk_1 = state.xkk_1
xkk = state.xkk
state.x.foreach(println)

trans.state_trans(deltaT)
Z = sensor.Z
state = kalman.kalmanfilter(state.xkk, state.Pkk, deltaT)
xkk_1 = state.xkk_1
xkk = state.xkk
state.x.foreach(println)

