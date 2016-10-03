import com.cloudera.sparkts.filter.Utils._
import breeze.linalg.{DenseMatrix, DenseVector, inv}
import com.cloudera.sparkts.filter.QRNoise
import scala.util.Random
import com.cloudera.sparkts.filter.Kalman

val dt = 0.005
val g = 9.81
val initialHeight = 100.0
val initialSpeed = 0.0
val variance = 0.014

val A: DblMatrix = DenseMatrix(
  (1.0, dt),
  (0.0, 1.0)
  )
val B: DblMatrix = DenseMatrix(
  0.5*g*dt*dt,
  g*dt
  )
val H: DblMatrix = DenseMatrix((1.0, 0.0), (0.0, 0.0))
val Q: DblMatrix = DenseMatrix(
  (0.25*variance*Math.pow(dt, 4), variance*Math.pow(dt,3)/2),
  (variance*Math.pow(dt,3)/2,     variance*dt*dt)
  )
val P0: DblMatrix = DenseMatrix(
  (0.02, 0.0),
  (0.0, 0.03)
  )

val x0 = DenseVector(initialHeight, initialSpeed)
implicit val qrNoise =
  new QRNoise((0.7, 0.3), (m: Double) => m*Random.nextGaussian)

var model = new Kalman(A, B, H, P0)
var z = H * x0
val u = DenseVector(-g)

var output = model.filter(x0, u, z)
var x_ = output._1
var P_ = output._2
var z_ = H * x0

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_

model = new Kalman(A, B, H, P_)
output = model.filter(x_, u, z_)
x_ = output._1
P_ = output._2
z_ = H * x_