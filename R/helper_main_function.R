#' Calculate Residuals for the Model
#'
#' This function computes the residuals for the model by subtracting the fitted values from the observed values of the response variable.
#'
#' @param m An object of class `"hmc"` containing the observed values and fitted values for the location part of the model.
#'
#' @return The updated `"hmc"` object with residuals stored in \code{m$residuals}.
#'
#' @keywords internal
residuals <- function(m) {
  # Calculate the residuals by subtracting fitted values from observed values
  m$residuals <- m$y - m$fitted$location
  return(m)
}

#' Calculate Fitted Values for a Model
#'
#' This function calculates the fitted values for a model based on the coefficients for location and scale.
#' It updates the hmc object with the computed fitted values.
#'
#' @param m A list or model object containing the following elements:
#'   \itemize{
#'     \item \code{coeffs} A list with two elements: \code{location} (vector of location coefficients) and \code{scale} (vector of scale coefficients).
#'     \item \code{x} A matrix of predictors for the location.
#'     \item \code{z} A matrix of predictors for the scale.
#'     \item \code{fitted} A list where the computed fitted values will be stored.
#'   }
#'
#' @return The updated list or model object \code{m} with calculated fitted values in \code{m$fitted$location} and \code{m$fitted$scale}.
#'
#' @keywords internal
fitted_values <- function(m) {
  # Extract coefficients for location and scale
  beta <- m$coeffs$location
  gamma <- m$coeffs$scale

  # Calculate fitted values for the location part
  m$fitted$location <- as.vector(m$x %*% beta)

  # Calculate fitted values for the scale part based on the number of columns in z
  if (ncol(m$z) < 2) {
    # If only one column, apply exponential function to scale coefficient
    scale_fit <- rep(exp(gamma), length(m$z))
  } else {
    # If more than one column, compute exponential of linear combination
    scale_fit <- exp(gamma[1] + gamma[-1] * m$z[, -1])
  }

  # Store the fitted values for scale
  m$fitted$scale <- scale_fit

  return(m)
}

#' Compute Credible Intervals for Model Parameters
#'
#' This function calculates credible intervals for the location and scale parameters based on the MCMC samples stored in the HMC object. It computes the 2.5%, 50%, and 97.5% quantiles to provide a 95% credible interval.
#'
#' @param m An object of class `"hmc"` containing MCMC samples for the location and scale parameters.
#' @param p Numeric value between 0 and 1 specifying the probability for the credible interval. Default is 0.95 for a 95% credible interval.
#'
#' @details
#' The function computes the lower bound, median, and upper bound of the credible interval for each parameter. The results are stored in data frames with columns named "2.5%", "50%", and "97.5%" representing the quantiles of the credible interval.
#'
#' @return The updated `"hmc"` object with credible intervals for the location and scale parameters stored in `m$CI$location` and `m$CI$scale`.
#'
#'
#' @importFrom stats quantile
#'
#'
#' @keywords internal
compute_credible_intervals <- function(m, p = 0.95) {
  # Set the alpha level based on the desired credible interval
  alpha <- (1 - p) / 2

  # Define a function to compute quantiles for vectors or matrices
  quantile_func <- function(v) {
    if (is.null(dim(v))) {
      # Vector case
      return(stats::quantile(v, probs = c(alpha, 0.5, 1 - alpha)))
    } else {
      # Matrix case
      return(t(apply(v, 2, stats::quantile, probs = c(alpha, 0.5, 1 - alpha))))
    }
  }

  # Compute credible intervals for location and scale parameters
  ci_loc <- quantile_func(m$mcmc$location)
  ci_scale <- quantile_func(m$mcmc$scale)

  # Convert results to data frames with appropriate column names
  ci_loc <- as.data.frame(ci_loc)
  colnames(ci_loc) <- c("2.5%", "50%", "97.5%")

  ci_scale <- as.data.frame(ci_scale)
  colnames(ci_scale) <- c("2.5%", "50%", "97.5%")

  # Store the computed credible intervals in the model object
  m$CI$location <- ci_loc
  m$CI$scale <- ci_scale

  return(m)
}

#' Calculate Point Estimates for Model Parameters
#'
#' This function computes the point estimates (mean) of the location and scale parameters from the MCMC samples stored in the HMC object.
#'
#' @param m An object of class `"hmc"` containing the MCMC samples for the location and scale parameters.
#'
#' @return The updated `"hmc"` object with point estimates for the location and scale parameters stored in `m$coeffs$location` and `m$coeffs$scale`.
#'
#' @keywords internal
point_estimate <- function(m) {
  if (!is.null(dim(m$mcmc$location))) {
    # Compute mean of location parameters if MCMC samples are in matrix form
    beta_mean <- colMeans(m$mcmc$location)
  } else {
    # Compute mean of location parameters if MCMC samples are in vector form
    beta_mean <- mean(m$mcmc$location)
  }

  if (!is.null(dim(m$mcmc$scale))) {
    # Compute mean of scale parameters if MCMC samples are in matrix form
    gamma_mean <- colMeans(m$mcmc$scale)
  } else {
    # Compute mean of scale parameters if MCMC samples are in vector form
    gamma_mean <- mean(m$mcmc$scale)
  }

  # Assign column names based on predictors
  names(beta_mean) <- colnames(m$x)
  names(gamma_mean) <- colnames(m$z)

  # Update the model object with point estimates
  m$coeffs$location <- beta_mean
  m$coeffs$scale <- gamma_mean

  return(m)
}

#' Initialize Estimates for Model Parameters
#'
#' This function initializes the estimates for the model parameters by maximizing the log-likelihood function using the BFGS optimization algorithm. The resulting estimates are used as starting values for the Hamiltonian Monte Carlo (HMC) algorithm.
#'
#' @param m An object of class `"hmc"` containing model information, including dimensions of the location and scale parameters.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Creates initial parameter values (zeros) for both location and scale parts of the model.
#'   \item Uses the `stats::optim` function with the BFGS method to maximize the log-likelihood function and obtain initial estimates for the parameters.
#'   \item Updates the `coeffs` component of the input object `m` with the optimized parameter values for location and scale.
#'   \item Initializes the first row of the MCMC samples in `m$mcmc$location` and `m$mcmc$scale` with the estimated values.
#'   \item Prints a warning if the optimization does not converge.
#' }
#'
#' @return An updated `"hmc"` object with initial estimates for location and scale parameters stored in `m$coeffs$location` and `m$coeffs$scale`, respectively.
#'
#' @importFrom stats optim
#'
#' @keywords internal
init_estimates <- function(m) {
  initial_params <- c(rep(0, m$dim_beta), rep(0, m$dim_gamma))
  mle_result <- stats::optim(
    initial_params,
    function(beta_gamma) log_likelihood_2(par = beta_gamma, m = m, neg = TRUE),
    method = "BFGS",
    control = list(maxit = 10000)
  )

  # Extract optimized parameters
  init_beta <- mle_result$par[1:m$dim_beta]
  init_gamma <- mle_result$par[(m$dim_beta + 1):length(mle_result$par)]

  # Update model object with initial estimates
  m$coeffs$location <- init_beta
  m$coeffs$scale <- init_gamma
  m$mcmc$location[1, ] <- init_beta
  m$mcmc$scale[1, ] <- init_gamma

  # Print warning if optimization did not converge
  if (mle_result$convergence != 0) {
    print("WARNING: Maximum likelihood did not converge!")
  }
  return(m)
}

#' Calculate the Log-Likelihood for a Model
#'
#' This function computes the log-likelihood of a model given its parameters.
#'
#' @param par A numeric vector of parameters, including both location (`beta`) and scale (`gamma`) parameters. If `NULL`, the default parameters from the model `m` are used (default is `NULL`).
#' @param m A list or object representing the model, which contains data matrices (`x` and `z`), response variable (`y`), and coefficients (`coeffs`) for location (`beta`) and scale (`gamma`).
#' @param neg A logical value indicating whether to return the negative of the log-likelihood (default is `FALSE`).
#'
#' @return A numeric value representing the (negative) log-likelihood of the model.
#'
#' @details
#' The function calculates the log-likelihood of the model given the current or specified parameters. The parameters include the location coefficients (`beta`) and scale coefficients (`gamma`). If `par` is provided, it overrides the default coefficients in the model `m`. The log-likelihood is computed using a Gaussian error model where the standard deviation is parameterized as an exponential function of the linear predictor `z * gamma`.
#'
#'
#' @keywords internal
log_likelihood_2 <- function(par = NULL, m, neg = FALSE) {
  if (!is.null(par)) {
    # Extract parameters from the provided vector
    beta <- par[1:m$dim_beta]
    gamma <- par[(m$dim_beta + 1):length(par)]
  } else {
    # Use parameters from the model object if not provided
    beta <- m$coeffs$location
    gamma <- m$coeffs$scale
  }

  # Compute model predictions and standard deviation
  mu <- m$x %*% beta
  log_sigma <- m$z %*% gamma
  sigma <- exp(pmin(pmax(log_sigma, -700), 700))

  # Calculate the log-likelihood
  n <- length(m$y)  # Number of observations
  ll <- -0.5 * n * log(2 * pi) - sum(log_sigma) - sum((m$y - mu)^2 / sigma^2) / 2

  if (neg) {
    return(-ll)
  } else {
    return(ll)
  }
}

#' Compute the Analytical Gradient of the Log-Likelihood
#'
#' This function calculates the analytical gradient of the log-likelihood with respect to the model parameters (location `beta` and scale `gamma`).
#'
#' @param par A numeric vector of parameters, where the first `m$dim_beta` elements correspond to location parameters (`beta`) and the remaining elements correspond to scale parameters (`gamma`).
#' @param m An object representing the model, containing matrices `x` and `z`, and the response variable `y`.
#'
#' @return A numeric vector of the negative gradient of the log-likelihood with respect to the parameters. The negative gradient is returned to facilitate the maximization process.
#'
#' @details
#' The function computes the gradient of the log-likelihood function for both location and scale parameters. This gradient is used in optimization routines to find the maximum likelihood estimates of the parameters.
#'
#' @keywords internal
gradient_log_likelihood_2 <- function(par = NULL, m) {
  # Extract beta and gamma from the parameter vector
  beta <- par[1:m$dim_beta]
  gamma <- par[(m$dim_beta + 1):length(par)]

  # Compute model predictions and standard deviation
  mu <- m$x %*% beta
  log_sigma <- m$z %*% gamma
  sigma <- exp(log_sigma)

  # Calculate residuals
  residuals <- (m$y - mu)

  # Gradient with respect to beta (location parameters)
  grad_beta <- t(m$x) %*% (residuals / sigma^2)

  # Gradient with respect to gamma (scale parameters)
  grad_gamma <- t(m$z) %*% (1 - (residuals / sigma)^2)

  # Combine the gradients
  grad <- c(as.vector(grad_beta), as.vector(grad_gamma))

  return(-grad)  # Return the negative gradient for maximization
}

#' Find Optimal Stepsize for Hamiltonian Monte Carlo
#'
#' This function determines an appropriate stepsize (`epsilon`) for the Hamiltonian Monte Carlo (HMC) algorithm by performing a recursive search.
#'
#' @param m An object representing the model, including initial MCMC settings.
#' @param trajectory_length The length of the trajectory for the HMC algorithm.
#' @param num_trials The maximum number of iterations allowed for finding the optimal stepsize (default is 100).
#'
#' @return The optimal stepsize (`epsilon`) if found within the maximum number of iterations; otherwise, returns `NULL`.
#'
#' @details
#' The function initializes the stepsize and uses the `find_stepsize_recursive` function to search for the optimal stepsize that achieves a target acceptance rate. It adjusts the stepsize dynamically until it is within the desired tolerance of the target acceptance rate.
#'
#' @note This function relies on `find_stepsize_recursive` to perform the actual search. Ensure that `find_stepsize_recursive` is defined and available in your environment.
#'
#'
#' @keywords internal
find_stepsize2 <- function(m, trajectory_length, num_trials = 100){
  epsilon <-1 # runif(1, 0.1, 0.5)
  tolerance <- 0.10
  #  initial_decay_factor <- 3
  decay_factor <- 3
  decay_adj <- 0.2
  counter = 0
  target_acceptance_rate = 0.23
  turned = FALSE
  stepsize_shrinking = TRUE

  # Perform the recursive search for the optimal stepsize
  epsilon_last_state <- find_stepsize_recursive(
    m, trajectory_length, target_acceptance_rate, epsilon, counter, turned, stepsize_shrinking,
    decay_factor, decay_adj, tolerance, num_trials
  )

  return(epsilon_last_state)
}

#' Recursively Find Optimal Stepsize for Hamiltonian Monte Carlo
#'
#' This function recursively finds the optimal stepsize (`epsilon`) for the Hamiltonian Monte Carlo (HMC) algorithm, aiming for a specific acceptance rate.
#'
#' @param m An object representing the model, including initial MCMC settings.
#' @param trajectory_length The length of the trajectory for the HMC algorithm.
#' @param target_acceptance_rate The desired target acceptance rate for the HMC algorithm.
#' @param epsilon The initial stepsize for the HMC algorithm.
#' @param counter An integer representing the current iteration count.
#' @param turned A logical value indicating if the direction of stepsize adjustment has changed.
#' @param stepsize_shrinking A logical value indicating if the stepsize is currently being shrunk.
#' @param decay_factor A numeric value indicating the factor by which to adjust the stepsize.
#' @param decay_adj A numeric value used to adjust the decay factor.
#' @param tolerance A numeric value representing the tolerance range for the target acceptance rate (default is 0.05).
#' @param num_trials The maximum number of iterations allowed (default is 100).
#'
#' @return A list containing the optimal stepsize (`epsilon`) and the final MCMC samples if the target acceptance rate is achieved; otherwise, returns `NULL` if the maximum number of iterations is reached.
#'
#' @details
#' The function uses a recursive approach to adjust the stepsize (`epsilon`) based on the acceptance rate of the HMC algorithm. It dynamically changes the stepsize to bring the acceptance rate within the specified tolerance of the target rate. The search continues until the target acceptance rate is achieved or the maximum number of iterations is reached.
#'
#' @note This function assumes the existence of `build_chain_cpp` for running the HMC algorithm and computing the acceptance rate.
#'
#' @keywords internal
find_stepsize_recursive <- function(
    m, trajectory_length, target_acceptance_rate, epsilon, counter, turned,
    stepsize_shrinking, decay_factor, decay_adj, tolerance = 0.05, num_trials
) {
  if (turned) {
    # Adjust the decay factor if direction has changed
    decay_factor <- decay_factor * (1 - decay_adj)
    if (decay_factor < 1) {
      decay_factor <- 1 + decay_adj
      decay_adj <- decay_adj / 2
    }
    turned <- !turned
  }

  if (counter == num_trials) {
    stop("Could not find a reasonable epsilon in the specified number of iterations!")
  }

  # Create a copy of the model object for MCMC simulation
  M <- unserialize(serialize(m, NULL))

  # max von floor(.3 * nrow(M$mcmc$location - 1)) und 5000


  M <- build_chain_cpp(M, epsilon, trajectory_length, min(floor(.5 * (nrow(M$mcmc$location) - 1)), 5000), burn_in=0, thin=1)
  acceptance_rate <- M$mcmc$accepted

  if((acceptance_rate <= target_acceptance_rate+tolerance) &&( acceptance_rate >= (target_acceptance_rate) )){
    return(list(epsilon = epsilon, last_location = M$mcmc$location[min(floor(.3 * (nrow(M$mcmc$location - 1) - 1)), 5000),], last_scale = M$mcmc$scale[min(floor(.5 * nrow(M$mcmc$location - 1)), 5000),]))
  }

  if (acceptance_rate > target_acceptance_rate) {
    if(stepsize_shrinking){
      turned = TRUE
      stepsize_shrinking = FALSE
      m$mcmc$location[1, ] <- M$mcmc$location[min(floor(.5 * nrow(M$mcmc$location - 1)), 5000),]
  m$mcmc$scale[1, ] <- M$mcmc$scale[min(floor(.5 * nrow(M$mcmc$location - 1)), 5000),]
    }
    epsilon <- epsilon * decay_factor
    counter <- counter + 1
    return(find_stepsize_recursive(
      m, trajectory_length, target_acceptance_rate, epsilon, counter, turned,
      stepsize_shrinking, decay_factor, decay_adj, tolerance, num_trials
    ))
  } else {
    if(!stepsize_shrinking){
      turned = TRUE
      stepsize_shrinking = TRUE
      m$mcmc$location[1, ] <- M$mcmc$location[min(floor(.5 * (nrow(M$mcmc$location) - 1)), 5000),]
  m$mcmc$scale[1, ] <- M$mcmc$scale[min(floor(.5 * (nrow(M$mcmc$location) - 1)), 5000),]
    }
    epsilon <- epsilon / decay_factor
    counter <- counter + 1
    return(find_stepsize_recursive(
      m, trajectory_length, target_acceptance_rate, epsilon, counter, turned,
      stepsize_shrinking, decay_factor, decay_adj, tolerance, num_trials
    ))
  }
}
