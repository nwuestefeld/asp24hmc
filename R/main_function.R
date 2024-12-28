#' Initialize HMC Object
#'
#' This function initializes an HMC (Hamiltonian Monte Carlo) object required for fitting a Gaussian location-scale regression model. It sets up the necessary components, including matrices for the location and scale parts of the model, initial coefficient values, and data structures for storing MCMC results.
#'
#' @param location A formula specifying the model for the location part of the response variable. This formula should be in the form of `response ~ predictors`.
#' @param scale A formula specifying the model for the scale part of the response variable. This formula should be in the form of `~ predictors` (without a response variable).
#' @param data A data frame containing the data to be used for model fitting.
#' @param chain_length An integer specifying the length of the MCMC chain.
#'
#' @return A list of class `"hmc"` containing the initialized components needed for HMC:
#' \itemize{
#'   \item \code{y}: The response variable as a vector.
#'   \item \code{x}: The model matrix for the location part of the model.
#'   \item \code{z}: The model matrix for the scale part of the model.
#'   \item \code{nobs}: The number of observations.
#'   \item \code{dim_beta}: The dimension of the location model (number of predictors).
#'   \item \code{dim_gamma}: The dimension of the scale model (number of predictors).
#'   \item \code{df}: Degrees of freedom for the model.
#'   \item \code{df.residual}: Residual degrees of freedom.
#'   \item \code{coeffs}: A list of initial coefficient values for location and scale parts.
#'   \item \code{CI}: An empty list for storing credible intervals.
#'   \item \code{fitted}: A list containing matrices for storing fitted values for location and scale.
#'   \item \code{mcmc}: A list containing matrices and vectors for storing MCMC samples and acceptance rates.
#'   \item \code{call}: The function call.
#' }
#'
#' @importFrom stats model.response model.matrix model.frame
#' @keywords internal
init <- function(location, scale = ~1, data, chain_length) {
  # Create the model frame for the location formula
  model_frame <- model.frame(location, data)
  terms_location <- terms(model_frame)

  # Ensure the location formula includes a response variable formula
  if (attr(terms_location, "response") == 0) {
    stop("location formula must have a response variable.")
  }
  y <- stats::model.response(model_frame) # Extract response variable
  x <- stats::model.matrix(location, data) # Construct model matrix for location part

  # Check if the scale formula includes the response
  model_frame_scale <- stats::model.frame(scale, data)
  terms_scale <- stats::terms(model_frame_scale)
  if (attr(terms_scale, "response") == 1) {
    stop("scale formula must not have a response variable.")
  }

  # Dynamically create the scale formula by adding the response variable to it
  response_var <- all.vars(terms_location)[1]  # Extract the response variable from the location formula
  scale_formula <- stats::as.formula(paste(response_var, paste(as.character(scale)[2], collapse = ""), sep = " ~ "))

  # Create the model matrix for the scale part
  z <- stats::model.matrix(scale_formula, data)
  nobs <- length(y)
  dim_beta <- ncol(x)
  dim_gamma <- ncol(z)

  # Initialize coefficients for the location and scale parts
  beta_init <- rep(0, dim_beta)
  gamma_init <- rep(0, dim_gamma)

  # Calculate degrees of freedom
  df <- dim_beta + dim_gamma
  df.residual <- nobs - df

  # Build the HMC object structure
  m <- structure(
    list(
      y = y, # Response variable
      x = x, # Location matrix
      z = z, # Scale matrix
      nobs = nobs, # Number of observations
      dim_beta = dim_beta, # Dimension of location parameters
      dim_gamma = dim_gamma, # Dimension of scale parameters
      df = df, # Degrees of freedom
      df.residual = df.residual, # Residual degrees of freedom
      call = NULL,
      coeffs = list(location = beta_init, scale = gamma_init), # Initial coefficients
      CI = list(location = list(), scale = list()), # Credible intervals
      fitted = list(
        location = matrix(NA, nrow = nrow(x), ncol = 1),
        scale = matrix(NA, nrow = nrow(x), ncol = 1)
      ), # Fitted values storage
      mcmc = list(
        location = matrix(0, nrow = chain_length + 1, ncol = dim_beta, dimnames = list(NULL, colnames(x))),
        scale = matrix(0, nrow = chain_length + 1, ncol = dim_gamma, dimnames = list(NULL, colnames(z))),
        trajectory_length = NULL,
        stepsize = NULL,
        accepted = vector(length = chain_length + 1)
      ) # MCMC storage
    ),
    class = "hmc"
  )

  return(m)
}

#' Wrapper Function for the Hamiltonian Monte Carlo Algorithm
#'
#' This function is a wrapper for the Hamiltonian Monte Carlo (HMC) algorithm. It initializes the model, optionally tunes hyperparameters, and builds the Markov Chain.
#'
#' @param m An object of class `"hmc"` containing the model and initial parameters.
#' @param stepsize A numeric value specifying the step size for the HMC algorithm. If \code{NULL}, the step size will be determined through hyperparameter tuning.
#' @param trajectory_length A numeric value specifying the trajectory length for the HMC algorithm. If \code{NULL}, the trajectory length will be determined through hyperparameter tuning.
#' @param chain_length An integer specifying the number of iterations in the Markov Chain.
#' @param burn_in An integer specifying the number of initial iterations to discard (burn-in phase).
#' @param thin An integer specifying the thinning interval (i.e., how often to keep samples).
#'
#' @keywords internal
#'
#' @import Rcpp
#' @import RcppEigen
#'
#' @useDynLib asp24hmc, .registration = TRUE
#'
#' @return An updated `"hmc"` object containing the estimated parameters and the generated Markov Chain. If hyperparameter tuning is performed, a list with suggested values for \code{stepsize} and \code{trajectory_length} is returned.
build <- function(m, stepsize, trajectory_length, chain_length, burn_in, thin) {
  m <- init_estimates(m) # Estimate initial parameters

  # Perform hyperparameter tuning if necessary
  if (xor(is.null(trajectory_length), is.null(stepsize))) {
    stop("Both trajectory_length and stepsize must be specified or none of them such that a tuning can be performed.")
  } else if (is.null(trajectory_length) && is.null(stepsize)) {
    init_trajectory_length <- 30
    epsilon_last_state <- find_stepsize2(m, 15)
    stepsize <- epsilon_last_state$epsilon
    m$mcmc$location[1, ] <- epsilon_last_state$last_location
    m$mcmc$scale[1, ] <- epsilon_last_state$last_scale
  }

  trajectory_length <- 10 # Set default trajectory length
  m$mcmc$trajectory_length <- trajectory_length
  m$mcmc$stepsize <- stepsize

  # Build the Markov Chain
  m <- build_chain_cpp(m, stepsize, trajectory_length, chain_length, burn_in, thin)
  return(m)
}

#' Wrapper Function for Finalizing HMC Results
#'
#' This function finalizes the results of the Hamiltonian Monte Carlo (HMC) algorithm by fitting the model values, calculating credible intervals, and computing residuals.
#'
#' @param m An object of class `"hmc"` containing the model information, initial estimates, and MCMC samples.
#'
#' @keywords internal
#' @return An updated `"hmc"` object containing the fitted values, credible intervals, and residuals.
finish <- function(m) {
  m <- point_estimate(m) # Calculate point estimates
  m <- fitted_values(m) # Fit the values
  m <- compute_credible_intervals(m) # Calculate credible intervals
  m <- residuals(m) # Calculate raw residuals
  return(m)
}

#' Perform Hamiltonian Monte Carlo Regression
#'
#' This function initializes and runs a Hamiltonian Monte Carlo (HMC) algorithm for a Gaussian location-scale regression model. It estimates initial values, builds the chain, fits the model, and computes credible intervals for the parameters.
#'
#' @param location A formula specifying the location part of the model. This should be in the format of `y ~ x1 + x2` where `y` is the response variable and `x1`, `x2` are predictor variables.
#' @param scale A formula specifying the scale part of the model. This should be in the format of `~ x1 + x2` where `x1`, `x2` are predictor variables.
#' @param data A data frame containing the variables specified in the `location` and `scale` formulas.
#' @param chain_length An integer specifying the length of the Markov Chain.
#' @param stepsize A numeric value for the step size in the HMC algorithm.
#' @param trajectory_length A numeric value for the trajectory length in the HMC algorithm.
#' @param burn_in An integer specifying the number of iterations to discard as burn-in.
#' @param thin An integer specifying the thinning interval.
#'
#' @return An object of class `"hmc"` containing the estimated parameters, fitted values, credible intervals, and residuals for the model.
#' @examples
#' \dontrun{
#' # Example usage of GLSR_HMC
#' # Define a location model and a scale model
#' location_formula <- y ~ x1 + x2
#' scale_formula <- ~ x1
#'
#' # Fit the model with HMC
#' result <- GLSR_HMC(
#'   location = location_formula,
#'   scale = scale_formula,
#'   data = my_data,
#'   chain_length = 5000,
#'   stepsize = 0.01,
#'   trajectory_length = 1,
#'   burn_in = 1000,
#'   thin = 10
#' )
#'
#' # Inspect the results
#' print(result)
#' }
#'
#'
#'
#' @export
GLSR_HMC <- function(location, scale = ~1, data, chain_length = 5000, stepsize, trajectory_length, burn_in, thin) {

  # Step 1: Initialize the HMC object
  m <- init(location = location, scale = scale, data = data, chain_length = chain_length)
  # save model call
  m$call <- match.call()

  # Step 2: Build the Markov Chain
  m <- build(m = m, stepsize = stepsize, trajectory_length = trajectory_length, chain_length = chain_length, burn_in = burn_in, thin = thin)

  # Step 3: Finalize and return the results
  m <- finish(m = m)
  return(m)
}
