#' function to calculate raw residuals
#'
#' @keywords internal
residuals <- function(y,yhat) {
  y - yhat
}

# function to estimate beta and gamma
estimate_beta_gamma <- function(samples, X, Z) {
  beta_index <- 1:ncol(X)
  beta_samples <- samples[, beta_index]
  gamma_samples <- samples[, -beta_index]
  if (!is.null(dim(beta_samples))) {
    beta_mean <- colMeans(beta_samples)
  } else {
    beta_mean <- mean(beta_samples)
  }
  if (!is.null(dim(gamma_samples))) {
    gamma_mean <- colMeans(gamma_samples)
  } else {
    gamma_mean <- mean(gamma_samples)
  }
  names(beta_mean) <- colnames(X)
  names(gamma_mean) <- colnames(Z)
  return(
    list(
      beta_mean = beta_mean,
      gamma_mean = gamma_mean,
      beta_samples = beta_samples,
      gamma_samples = gamma_samples
    )
  )
}

#' function to compute symmetric credible intervals (ci) with median
#'
#' @keywords internal
compute_credible_intervals <- function(x, p = 0.95) {
  alpha <- (1 - p) / 2
  if(!is.null(dim(x))) {
    med <- apply(x, 2, stats::quantile, probs = 0.5)
    low <- apply(x, 2, stats::quantile, probs = alpha)
    up <- apply(x, 2, stats::quantile, probs = 1 - alpha)
  } else {
    med <- stats::quantile(x, probs = 0.5)
    low <- stats::quantile(x, probs = alpha)
    up <- stats::quantile(x, probs = 1 - alpha)
  }
  ci <- data.frame(low,med,up)
  colnames(ci) <- c("2.5%", "50%", "97.5%")
  return(ci)
}

#' function to calculated fitted location values
#'
#' @keywords internal
location_fit <- function(X, beta) {
  X %*% beta
}
# function to fit scale
scale_est <- function(gamma, Z) {
  if(ncol(Z) < 2){
    scale_fit <- rep(exp(gamma), length(Z))
  } else {
    scale_fit <- exp(gamma[1] + gamma[-1] * Z[, -1])
  }
  return(scale_fit)
}

#' function to return dataframe containing beta/gamma estimates with ci
#'
#' @keywords internal
estimates_with_ci <- function(mean_estimate, samples) {
  ci <- compute_credible_intervals(samples)
  m_ci <- cbind(mean, ci)
  names(loc_coef)[1] <- "Mean"
  return(m_ci)
}
