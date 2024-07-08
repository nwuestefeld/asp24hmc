library(lmls)

#ggplot(abdom, aes(x, y)) + geom_point()

#X <- as.matrix(cbind(1, abdom$x)) # Add intercept term
#Y <- abdom$y
######

#abdom_posterior <- posterior(abdom)

# Define the log-likelihood function for the location-scale linear regression model
log_prob <- function(beta_gamma) {
  n <- length(Y)
  beta <- beta_gamma[1:ncol(X)]
  gamma <- beta_gamma[(ncol(X) + 1):(2 * ncol(X))]

  mu <- X %*% beta
  log_sigma <- X %*% gamma
  sigma <- exp(log_sigma)

  ll <- -sum(log_sigma) - sum((Y - mu)^2 / (2 * sigma^2))

  return(ll)
}
# Define the gradient of the log-posterior
gradient_log_prob <- function(beta_gamma) {
  grad(log_prob, beta_gamma)
}


# Define the log-likelihood function for the location-scale linear regression model # nolint
log_likeli <- function(params, X, Y) {
  n <- length(Y)
  beta <- params[1:ncol(X)] # nolint
  gamma <- params[(ncol(X) + 1):(2 * ncol(X))]

  mu <- X %*% beta
  log_sigma <- X %*% gamma
  sigma <- exp(log_sigma)

  ll <- - sum(log_sigma) - sum((Y - mu)^2 / (2 * sigma^2))

  return(ll)
}

# Define the negative log-likelihood function
neg_log_likelihood <- function(params, X, Y) {
  -log_likeli(params, X, Y)
}
