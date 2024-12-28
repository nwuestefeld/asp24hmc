#' Extracts Model Coefficients from HMC Object
#'
#' Extracts model coefficients from an object of class `hmc`. The function allows partial matching of coefficient types.
#'
#' @param object An object of class `hmc` from which to extract coefficients.
#' @param predictors A character vector specifying which coefficients to extract. Possible values are `"location"`, `"scale"`, or their abbreviations like `"loc"` and `"sca"`. Default is to extract all types.
#' @param ... Additional arguments (currently not used).
#'
#' @return A named list containing the requested coefficients. If multiple types are specified, the list will include entries for each type.
#'
#' @export
coef.hmc <- function(object, predictors = c("location", "scale"), ...) {
  # Perform partial matching for coefficient types
  type <- pmatch(predictors, c("location", "scale"), nomatch = 0)

  # Check if any invalid types are provided
  if (any(type == 0)) stop("Invalid type specified. Use 'location', 'scale', or abbreviations like 'loc', 'sca'.")

  # Convert matched indices to corresponding type names
  type_names <- c("location", "scale")[type]

  # Extract the coefficients matching the specified types
  coefficients <- object$coeffs[names(object$coeffs) %in% type_names]

  return(coefficients)
}

#' Compute Log-Likelihood for a Model Object
#'
#' Computes the log-likelihood of the model for an object of class `hmc`. This function uses the stored fitted values and residuals to calculate the log-likelihood assuming a normal distribution.
#'
#' @param object An object of class `hmc` containing the model information. The object must include the original response values (`y`), fitted location values, and fitted scale values.
#' @param ... Additionaly arguments (currently not used)
#'
#' @return An object of class `"logLik"` representing the log-likelihood of the model. The object also has attributes `"df"` (degrees of freedom) and `"nobs"` (number of observations).
#'
#' @details The log-likelihood is computed as the sum of the log of the normal density function evaluated at the observed values, using the fitted location and scale parameters. The resulting value is given the class `"logLik"` to be compatible with standard log-likelihood methods.
#'
#' @export
logLik.hmc <- function(object, ...) {
  # Compute the log-likelihood using internal function
  ll <- log_like(object)

  # Assign attributes for degrees of freedom and number of observations
  attr(ll, "df") <- object$df
  attr(ll, "nobs") <- object$nobs

  # Set the class to 'logLik'
  class(ll) <- "logLik"

  return(ll)
}
#' Summarize Model Fit for an HMC Object
#'
#' Provides a summary of the model fit for an object of class `hmc`. This includes summary statistics for residuals, coefficient estimates with confidence intervals, model fit statistics (log-likelihood, AIC, BIC), and other relevant information.
#'
#' @param object An object of class `hmc` containing the model results and fitted values.
#' @param ... Additional arguments (currently not used).
#'
#' @return An object of class `"summary.hmc"` containing a list with:
#' \describe{
#'   \item{call}{The call used to create the `hmc` object.}
#'   \item{residSUM}{Summary statistics of the residuals, including minimum, 1st quartile, median, mean, 3rd quartile, and maximum.}
#'   \item{location}{A matrix with location coefficient estimates and their confidence intervals.}
#'   \item{scale}{A matrix with scale coefficient estimates and their confidence intervals.}
#'   \item{N}{The number of observations used in the model.}
#'   \item{log_lik}{The log-likelihood of the model.}
#'   \item{AIC}{The Akaike Information Criterion for the model.}
#'   \item{BIC}{The Bayesian Information Criterion for the model.}
#' }
#'
#' @importFrom stats AIC BIC logLik terms
#'
#' @export
summary.hmc <- function(object, ...) {
  # Compute summary statistics for residuals
  residSUM <- c(min(object$residuals), stats::quantile(object$residuals, 0.25),
                stats::median(object$residuals), mean(object$residuals), stats::quantile(object$residuals, 0.75),
                max(object$residuals))
  names(residSUM) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  # Combine coefficients with confidence intervals for location and scale
  location_df <- cbind(object$coeffs$location, object$CI$location)
  names(location_df)[1] <- "Mean"

  scale_df <- cbind(object$coeffs$scale, object$CI$scale)
  names(scale_df)[1] <- "Mean"

  # Compute model fit statistics
  log_like <- log_like(object)
  log_lik_obj <- logLik(object)
  aic <- stats::AIC(log_lik_obj)
  bic <- stats::BIC(log_lik_obj)

  # Create a list summarizing the model
  summary_list <- structure(list(
    call = object$call,
    residSUM = residSUM,
    location = location_df,
    scale = scale_df,
    N = object$n_obs,
    log_lik = log_like,
    aic = aic,
    bic = bic,
    df.residual = object$df.residual
  ))

  # Assign S3 class to the summary list
  class(summary_list) <- "summary.hmc"

  return(summary_list)
}

#' Print Summary for an HMC Model
#'
#' Prints a summary of the model fit for an object of class `"summary.hmc"`. This includes the call, residuals summary, coefficient estimates with confidence intervals, log-likelihood, AIC, and BIC.
#'
#' @param x An object of class `"summary.hmc"` to print.
#' @param ... Additional arguments (currently not used).
#'
#' @return This function does not return a value but prints the summary information to the console.
#'
#'
#' @export
print.summary.hmc <- function(x, ...) {
  # Print model call
  cat("\nCall:\n")
  print(x$call)

  # Print residual summary
  cat("\nResiduals:\n")
  print(x$residSUM, digits = 5)

  # Print location coefficients if they exist
  if (length(x$location)) {
    cat("\nLocation coefficients (identity link):\n")
    print(x$location, digits = 5)
  } else {
    cat("No location coefficients\n")
  }
  cat("\n")

  # Print scale coefficients if they exist
  if (length(x$scale)) {
    cat("\nScale coefficients (log link):\n")
    print(x$scale, digits = 5)
  } else {
    cat("No scale coefficients\n")
  }
  cat("\n")

  # Print model fit statistics
  pretty <- function(x, y) {
    cat(x, ": ", format(signif(y), getOption("digits")), "\n", sep = "")
  }

  pretty("Log-likelihood", x$log_lik)
  pretty("Residual degrees of freedom:", x$df.residual)
  pretty("AIC", x$aic)
  pretty("BIC", x$bic)
}

#' Print Method for HMC Objects
#'
#' This function provides a custom print method for objects of class `hmc`.
#' It prints the call that generated the object, followed by the location and scale coefficients with their corresponding confidence intervals.
#'
#' @param x An object of class `hmc`, containing the results of a Hamiltonian Monte Carlo (HMC) analysis.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{NULL}. The function is used for its side effects (printing to the console).
#'
#'
#' @export
print.hmc <- function(x, ...) {
  # Print the call that generated the object
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )

  # Extract and display location coefficients with confidence intervals
  location_df <- cbind(x$coeffs$location, x$CI$location)
  names(location_df)[1] <- "Mean"

  # Extract and display scale coefficients with confidence intervals
  scale_df <- cbind(x$coeffs$scale, x$CI$scale)
  names(scale_df)[1] <- "Mean"

  # Print the location and scale coefficients
  cat("\nLocation coefficients (identity link):\n")
  print(location_df, digits = 5)
  cat("\nScale coefficients (log link):\n")
  print(scale_df, digits = 5)
}

#' Predict Method for HMC Model
#'
#' @description
#' This function generates predictions from a fitted Hamiltonian Monte Carlo (HMC) model.
#' It calculates the predicted values for both the location and scale parameters based on the provided new data.
#'
#' @param object An object of class `"hmc"` which is the result of fitting an HMC model. It should contain the fitted coefficients and model call.
#' @param newdata A data frame containing the new data for which predictions are to be made. The data frame must have the same structure as the data used to fit the model.
#' @param ... Additional arguments (currently not used).
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{location}: A numeric vector of predicted values for the location parameter.
#'   \item \code{scale}: A numeric vector of predicted values for the scale parameter.
#' }
#' The length of each vector corresponds to the number of rows in \code{newdata}.
#'
#' @importFrom stats as.formula update
#'
#' @export
predict.hmc <- function(object, newdata, ...) {
  # Check if newdata is provided
  if (missing(newdata)) {
    stop("New data must be provided for predictions.")
  }

  # Extract model call and estimates
  call <- object$call
  beta <- object$coeffs$location
  gamma <- object$coeffs$scale

  # Ensure extracted calls are treated as formulas
  formula_location <- stats::as.formula(call$location)
  formula_scale <- stats::as.formula(call$scale)

  # Remove response variable from formula for prediction
  formula_location_pred <- stats::update(formula_location, NULL ~ .)
  formula_scale_pred <- stats::update(formula_scale, NULL ~ .)

  # Create design matrices for new data
  X_new <- stats::model.matrix(formula_location_pred, newdata)
  Z_new <- stats::model.matrix(formula_scale_pred, newdata)

  # Compute predicted values for location
  location_hat <- as.vector(X_new %*% beta)
  names(location_hat) <- seq_along(location_hat)

  # Compute predicted values for scale
  if (ncol(Z_new) < 2 && sum(Z_new[,1] == object$nobs)) {
    scale_hat <- rep(gamma, length(Z_new))
  } else {
    scale_hat <- as.vector(Z_new %*% gamma)
  }
  names(scale_hat) <- seq_along(scale_hat)

  # Return the predictions as a list
  predictions <- list(location = location_hat, scale = scale_hat)
  return(predictions)
}


#' Plot Diagnostic Plots for HMC Model
#'
#' @description
#' This function generates diagnostic plots for a fitted Hamiltonian Monte Carlo (HMC) model.
#' Users can request different types of plots to evaluate the MCMC samples: trace plots, autocorrelation plots, and density plots.
#'
#' @param x An object of class `"hmc"`, which contains the MCMC samples and model details.
#' @param type A character vector specifying the type(s) of plots to generate. Options include:
#' \itemize{
#'   \item \code{"trace"}: Trace plots of the MCMC samples for both location and scale parameters.
#'   \item \code{"autocorrelation"}: Autocorrelation plots of the MCMC samples for location and scale parameters.
#'   \item \code{"density"}: Density plots of the marginal posterior distributions of the location and scale parameters.
#' }
#' Abbreviations (e.g., \code{"tr"} for \code{"trace"}) are also accepted.
#' @param ... Additional arguments passed to the underlying plotting functions.
#'
#' @details
#' The function checks which types of plots are specified and generates them accordingly. It uses separate helper functions to create trace, autocorrelation, and density plots:
#' \itemize{
#'   \item \code{plot_trace()}: Generates trace plots for the location and scale parameters.
#'   \item \code{autocorrelation_plot()}: Creates autocorrelation plots for the location and scale parameters.
#'   \item \code{density_plot()}: Produces density plots for the marginal posterior distributions of the location and scale parameters.
#' }
#' The plots are displayed in separate plot windows as specified by the types chosen.
#'
#'
#' @export
plot.hmc <- function(x, type = c("trace", "autocorrelation", "density"),...) {
  # Extract MCMC samples
  location_samples <- x$mcmc$location
  scale_samples <- x$mcmc$scale

  # Define supported plot types
  types <- c("trace", "autocorrelation", "density")

  # Validate plot type
  type_index <- pmatch(type,types, nomatch = 0)
  if (any(type_index == 0)) stop("Invalid type specified. Use 'trace', 'autocorrelation', 'density' or abbreviations like 'tr', 'auto'.")
  type_names <- types[type_index]

  # Generate requested plots
  if ("trace" %in% type_names) {
    trace_plot(x)
  }
  if ("autocorrelation" %in% type_names) {
    autocorrelation_plot(x)
  }
  if ("density" %in% type_names) {
    density_plot(x)
  }
}


#' Compute Covariance Matrices from Posterior Draws
#'
#' Computes the covariance matrices for the specified predictors (location and/or scale)
#' from the posterior draws of an HMC (Hamiltonian Monte Carlo) model.
#'
#' @param object An HMC model object containing posterior draws for the predictors.
#' @param predictors A character vector specifying the predictors for which to compute the covariance matrices.
#' Options are "location", "scale", or abbreviations like "loc", "sca". Defaults to both ("location" and "scale").
#' @param ... Additional arguments (currently not used).
#'
#' @return A named list of covariance matrices corresponding to the specified predictors.
#' If a predictor does not have any draws, its entry in the list will be NULL.
#'
#' @importFrom stats cov
#'
#' @export
vcov.hmc <- function(object, predictors = c("location", "scale"), ...) {
  # Match input predictors to valid options
  type <- pmatch(predictors, c("location", "scale"), nomatch = 0)
  if (any(type == 0)) stop("Invalid predictor specified. Use 'location', 'scale', or abbreviations like 'loc', 'sca'.")

  # Convert indices to names
  type_names <- c("location", "scale")[type]

  # List to store covariance matrices
  cov_matrices <- list()

  # Loop through each predictor and compute covariance matrix
  for (pred in type_names) {
    # Extract posterior draws for current predictor
    samples <- object$mcmc[[pred]]
    if (!is.null(samples)) {
      # Compute covariance matrix
      cov_matrix <- stats::cov(samples)
      cov_matrices[[pred]] <- cov_matrix
    } else {
      cov_matrices[[pred]] <- NULL
    }
  }

  # Return the named list of covariance matrices
  return(cov_matrices)
}
