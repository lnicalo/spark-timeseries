/**
  * Created by LNICOLAS on 23/09/2016.
  */

package com.cloudera.sparkts.filter.kalman.python
import com.cloudera.sparkts.TimeSeries

import breeze.linalg.{DenseMatrix, DenseVector, inv}
/**
  * Created by LNICOLAS on 02/10/2016.
  */
class KalmanFilter(transition_matrices: DenseMatrix[Double] = null,
           observation_matrices: DenseMatrix[Double] = null,
           transition_covariance: DenseMatrix[Double] = null,
           observation_covariance: DenseMatrix[Double] = null,
           transition_offsets: DenseMatrix[Double] =null,
           observation_offsets: DenseMatrix[Double] =null,
           initial_state_mean: DenseMatrix[Double] =null,
           initial_state_covariance: DenseMatrix[Double] =null,
           random_state: DenseMatrix[Double] =null,
           em_vars: List[String] = List("transition_covariance",
             "observation_covariance",
              "initial_state_mean",
              "initial_state_covariance"),
           n_dim_state: Int = 0,
           n_dim_obs: Int = 0) {

  private def determineDimensionality(m: List[String]): Int = 2
  private[this] def filterPredict(transition_matrix: DenseMatrix[Double],
                             transition_covariance: DenseMatrix[Double],
                             transition_offset: DenseMatrix[Double],
                             current_state_mean: DenseMatrix[Double],
                             current_state_covariance: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    /**
      * Calculate the mean and covariance of :math:`P(x_{t+1} | z_{0:t})`
      * Using the mean and covariance of :math:`P(x_t | z_{0:t})`, calculate the
      * mean and covariance of :math:`P(x_{t+1} | z_{0:t})`.
      *
      * Parameters
      * ----------
      * transition_matrix : [n_dim_state, n_dim_state} array
      * state transition matrix from time t to t+1
      * transition_covariance : [n_dim_state, n_dim_state] array
      * covariance matrix for state transition from time t to t+1
      * transition_offset : [n_dim_state] array
      * offset for state transition from time t to t+1
      * current_state_mean: [n_dim_state] array
      * mean of state at time t given observations from times [0...t]
      * current_state_covariance: [n_dim_state, n_dim_state] array
      * covariance of state at time t given observations from times
      * [0...t]
      * Returns
      * -------
      * predicted_state_mean : [n_dim_state] array
      * mean of state at time t+1 given observations from times [0...t]
      * predicted_state_covariance : [n_dim_state, n_dim_state] array
      * covariance of state at time t+1 given observations from times [0...t]
      */
    val predicted_state_mean = transition_matrix * current_state_mean + transition_offset
    val predicted_state_covariance =
      transition_matrix * current_state_covariance * transition_matrix.t + transition_covariance
    (predicted_state_mean, predicted_state_covariance)
  }

  private[this] def filterCorrect(observation_matrix: DenseMatrix[Double],
                      observation_covariance: DenseMatrix[Double],
                      observation_offset: DenseMatrix[Double],
                      predicted_state_mean: DenseMatrix[Double],
                      predicted_state_covariance: DenseMatrix[Double],
                      observation: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) = {
    if (true) { // not np.any(np.ma.getmask(observation))
      val predicted_observation_mean =
        observation_matrix * predicted_state_mean + observation_offset
      val predicted_observation_covariance =
        observation_matrix * predicted_state_covariance * observation_matrix.t + observation_covariance
      val kalman_gain = predicted_state_covariance * observation_matrix.t * inv(predicted_observation_covariance)
      val corrected_state_mean =
        predicted_state_mean + kalman_gain * (observation - predicted_observation_mean)
      val corrected_state_covariance =
        predicted_state_covariance - kalman_gain * observation_matrix * predicted_state_covariance
      (kalman_gain, corrected_state_mean, corrected_state_covariance)
    }
    else {
      val n_dim_state = predicted_state_covariance.rows
      val n_dim_obs = observation_matrix.rows
      val kalman_gain: DenseMatrix[Double] = DenseMatrix.zeros(n_dim_state, n_dim_obs)
      val corrected_state_mean = predicted_state_mean
      val corrected_state_covariance = predicted_state_covariance
      (kalman_gain, corrected_state_mean, corrected_state_covariance)
    }
  }
  private[this] def filter(): DenseMatrix[Double] = {
    transition_covariance
  }

  def filter(X: DenseVector[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val Z = parse_observations(X)

    (transition_matrices, transition_offsets, transition_covariance,
      observation_matrices, observation_offsets, observation_covariance,
      initial_state_mean, initial_state_covariance) = initialize_parameters()
      )

    (_, _, _, filtered_state_means,
      filtered_state_covariances) = (
      _filter(
        transition_matrices, observation_matrices,
        transition_covariance, observation_covariance,
        transition_offsets, observation_offsets,
        initial_state_mean, initial_state_covariance,
        Z
      )
      )
    return (filtered_state_means, filtered_state_covariances)
  }
}
