#' Helper Function: Log-Likelihood Calculation
#'
#' This internal function computes the log-likelihood for an HMC model object.
#' It calculates the sum of log-likelihoods based on the observed data, fitted location,
#' and scale parameters from the model.
#'
#' @param object An HMC model object containing the observed data `y`,
#' fitted values for location, and scale parameters.
#' @importFrom stats dnorm
#'
#' @keywords internal
log_like <- function(object) {
  # Extract observed data and fitted parameters from the object
  y <- object$y
  location <- object$fitted$location
  scale <- object$fitted$scale

  # Compute the sum of log-likelihoods for a normal distribution
  sum(stats::dnorm(y, location, scale, log = TRUE))
}

#' Trace Plot for MCMC Samples
#'
#' This internal function generates trace plots for the MCMC samples of
#' location and scale parameters from an HMC model.
#'
#' @param m An HMC model object containing MCMC samples for the location and scale parameters.
#'
#' @keywords internal
trace_plot <- function(m) {
  # Extract MCMC samples for location and scale
  location_samples <- m$mcmc$location
  scale_samples <- m$mcmc$scale

  # Set the plot layout depending on the number of samples
  if (ncol(location_samples > ncol(scale_samples))) {
    par(mfrow = c(2, ncol(location_samples)))
  } else {
    par(mfrow = c(2, ncol(scale_samples)))
  }

  # Plot trace plots for location parameters
  for (i in 1:ncol(location_samples)) {
    main_label <- paste0("Location: ", colnames(m$x)[i])
    plot(
      1:nrow(location_samples),
      location_samples[, i],
      xlab = "Iterations",
      ylab = "",
      main = main_label,
      type = "l"
    )
  }

  # Plot trace plots for scale parameters
  for (i in 1:ncol(scale_samples)) {
    main_label <- paste0("Scale: ", colnames(m$z)[i])
    plot(
      1:nrow(scale_samples),
      scale_samples[, i],
      xlab = "Iterations",
      main = main_label,
      ylab = "",
      type = "l"
    )
  }

  # Reset plot layout to default
  graphics::par(mfrow = c(1, 1))
}


#' Autocorrelation Plot for MCMC Samples
#'
#' This internal function generates autocorrelation plots for the MCMC samples of
#' location and scale parameters from an HMC model.
#'
#' @param m An HMC model object containing MCMC samples for the location and scale parameters.
#'
#' @importFrom stats qnorm
#' @importFrom graphics par abline
#'
#' @keywords internal
autocorrelation_plot <- function(m) {
  # Extract MCMC samples for location and scale
  location_samples <- m$mcmc$location
  scale_samples <- m$mcmc$scale
  n <- nrow(location_samples)
  z <- stats::qnorm(0.974)

  # Set the plot layout depending on the number of samples
  if (ncol(location_samples > ncol(scale_samples))) {
    graphics::par(mfrow = c(2, ncol(location_samples)))
  } else {
    graphics::par(mfrow = c(2, ncol(scale_samples)))
  }

  # Plot autocorrelation plots for location parameters
  for (i in 1:ncol(location_samples)) {
    acf_values <- stats::acf(location_samples[, i], plot = FALSE)
    lags <- acf_values$lag
    acf_vals <- acf_values$acf
    plot(
      lags,
      acf_vals,
      type = "h",
      main = paste0("Autocorrelation plot location: ", colnames(m$x)[i]),
      ylim = c(ifelse(min(acf_vals) < 0, -1, - (z/sqrt(n) + 0.05)), 1),
      xlab = "Lag",
      ylab = "Autocorrelation",
      lwd = 2
    )
    graphics::abline(h = 0, col = "red", lty = 2) # Add reference line
    graphics::abline(h = z/sqrt(n),col = "darkblue" , lty = 2) # add confidence intervals assuming lags are white noise
    graphics::abline(h = - z/sqrt(n), col = "darkblue", lty = 2)
  }

  # Plot autocorrelation plots for scale parameters
  for (i in 1:ncol(scale_samples)) {
    acf_values <- stats::acf(scale_samples[, i], plot = FALSE)
    lags <- acf_values$lag
    acf_vals <- acf_values$acf
    plot(
      lags,
      acf_vals,
      type = "h",
      main = paste0("Autocorrelation plot scale: ", colnames(m$z)[i]),
      ylim = c(ifelse(min(acf_vals) < 0, -1, -(z/sqrt(n) + 0.05)), 1),
      xlab = "Lag",
      ylab = "Autocorrelation",
      lwd = 2
    )
    graphics::abline(h = 0, col = "red", lty = 2) # Add reference line
    graphics::abline(h = z/sqrt(n),col = "darkblue" , lty = 2) # add confidence intervals assuming lags are white noise
    graphics::abline(h = - z/sqrt(n), col = "darkblue", lty = 2)
  }

  # Reset plot layout to default
  graphics::par(mfrow = c(1, 1))
}

#' Density Plot for Marginal Posterior Distributions
#'
#' This internal function generates kernel density plots for the marginal posterior
#' distributions of location and scale parameters from an HMC model.
#'
#' @param m An HMC model object containing MCMC samples for the location and scale parameters.
#' @importFrom graphics polygon
#'
#' @keywords internal
density_plot <- function(m) {
  # Extract MCMC samples for location and scale
  location_samples <- m$mcmc$location
  scale_samples <- m$mcmc$scale

  # Set the plot layout depending on the number of samples
  if (ncol(location_samples > ncol(scale_samples))) {
    graphics::par(mfrow = c(2, ncol(location_samples)))
  } else {
    graphics::par(mfrow = c(2, ncol(scale_samples)))
  }

  # Plot density plots for location parameters
  for (i in 1:ncol(location_samples)) {
    main <- paste0("Location: ", colnames(m$x)[i])
    kde <- stats::density(location_samples[, i])
    plot(
      kde,
      main = main,
      col = "blue",
      lwd = 2
    )
    graphics::polygon(kde, col = "lightblue", border = NA)
    graphics::abline(v = m$coeffs$location[i], col = "red", lty = 2) # Vertical line for mean
  }

  # Plot density plots for scale parameters
  for (i in 1:ncol(scale_samples)) {
    main <- paste0("Scale: ", colnames(m$z)[i])
    kde <- stats::density(scale_samples[, i])
    plot(
      kde,
      main = main,
      col = "blue",
      lwd = 2
    )
    graphics::polygon(kde, col = "lightblue", border = NA)
    graphics::abline(v = m$coeffs$scale[i], col = "red", lty = 2) # Vertical line for mean
  }
  # Reset plot layout to default
  graphics::par(mfrow = c(1,1))
}
