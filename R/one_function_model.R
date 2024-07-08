#' function to compute log-likelihood
#'
#' @keywords internal
log_likeli <- function(beta_gamma, X, Y, Z, dim_beta, dim_gamma, n_data) {
  beta <- beta_gamma[1:dim_beta]
  gamma <- beta_gamma[(dim_beta + 1):(dim_beta + dim_gamma)]

  mu <- X %*% beta
  log_sigma <- Z %*% gamma
  sigma <- exp(log_sigma)

  ll <- - sum(log_sigma) - sum((Y - mu)^2 / (2 * sigma^2)) #-n * sum(log_sigma) removed

  return(ll)
}
#' function to compute negative log acceptance criterion
#'
#' @keywords internal
neg_log_likelihood <- function(beta_gamma, X, Y, Z, dim_beta, dim_gamma, n_data) {
  -log_likeli(beta_gamma, X, Y, Z, dim_beta, dim_gamma, n_data)
}
