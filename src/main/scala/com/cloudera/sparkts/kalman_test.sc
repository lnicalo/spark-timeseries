import breeze.linalg.{DenseMatrix, DenseVector, inv}
import scala.util.Random

val dt = 0.1
val g = 9.8
def I = DenseMatrix.eye[Double](4)

val B = DenseMatrix(
  (0.0, 0.0, 0.0, 0.0),
  (0.0, 0.0, 0.0, 0.0),
  (0.0, 0.0, -1.0, 0.0),
  (0.0, 0.0, 0.0, -1.0)
)
val u = DenseVector(0, 0, g * dt * dt, g * dt)
val F = DenseMatrix(
  (1.0, dt,  0.0, 0.0),
  (0.0, 1.0, 0.0, 0.0),
  (0.0, 0.0, 1.0, dt),
  (0.0, 0.0, 0.0, 1.0)
)
val H = I
val Q = DenseMatrix.zeros[Double](4, 4)
val R = I * 0.2
var t = 0.0

// guess of state and variance
var s = DenseVector.zeros[Double](4)
var P = I
var x = DenseVector(1.0, 2.0, 3.0, 4.0)

def noisy(actual: Double) = actual + Random.nextGaussian * 50.0
val z = x

// actual simulation
x = F * x + B * u
t += dt

// prediction step
val predS = F * s + B * u
val predP = F * P * F.t + Q
// observation step
val innov = z - H * predS
val innov_cov = H * predP * H.t + R
// update step
val gain = predP * H.t * inv(innov_cov)
s = predS + gain * innov
P = (I - gain * H) * predP

/*

while (x(2) >= 0) {
  /*// Prediction for state vector and covariance
  s.x = s.A*s.x + s.B*s.u;
  s.P = s.A * s.P * s.A' + s.Q;

  // Compute Kalman gain factor:
  K = s.P*s.H'*inv(s.H*s.P*s.H'+s.R);

  // Correction based on observation:
    s.x = s.x + K*(s.z-s.H*s.x);
  s.P = s.P - K*s.H*s.P;*/
}*/
