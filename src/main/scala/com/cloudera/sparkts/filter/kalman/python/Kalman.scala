/**
  * Created by LNICOLAS on 23/09/2016.
  */

package com.cloudera.sparkts.filter.kalman.python

import breeze.linalg.{DenseMatrix, DenseVector}
/**
  * Created by LNICOLAS on 02/10/2016.
  */
class Kalman(transition_matrices: DenseMatrix[Double] = null,
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
           n_dim_state: Int = null,
           n_dim_obs: Int = null) {
    """Initialize Kalman Filter"""

  private def determineDimensionality(m: List[String]): Int = 2
  private[this] def filterPredict(transition_matrix: DenseMatrix[Double],
                             transition_covariance: DenseMatrix[Double],
                             transition_offset: DenseMatrix[Double],
                             current_state_mean: DenseMatrix[Double],
                             current_state_covariance: DenseMatrix[Double]): Unit = {

    val predicted_state_mean = transition_matrix * current_state_mean + transition_offset
    val predicted_state_covariance =
      transition_matrix * current_state_covariance * transition_matrix.t + transition_covariance
    return (predicted_state_mean, predicted_state_covariance)
  }

  def _filter_correct(observation_matrix, observation_covariance,
                      observation_offset, predicted_state_mean,
                      predicted_state_covariance, observation):
}
