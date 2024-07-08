#' predict method for object of class "bayes_hmc_reg"
#'
#' @param object object of class "bayes_hmc_reg"
#' @param newdata data.frame
#' @param ... additional arguments
#'
#' @return list with predictions for location and scale
#' @export
#'
#' @examples to be added
predict.bayes_hmc_reg <- function(object, newdata, ...) {
  # check if newdata is provided
  if (missing(newdata)) {
    stop("New data must be provided for predictions.")
  }
  # create model matrix for the new data
  X_new <- stats::model.matrix(object$formula, newdata)
  # extract regression coefficients
  beta <- object$coefficients$location
  # extract corresponding gamma coefficients
  gamma <- object$coefficients$scale
  # generate predictions for the location of the response variable
  location_pred <- X_new %*% beta
  names(location_pred) <- as.character(1:length(location_pred))
  # generate predictions for the scale of the response variable
  scale_pred = gamma[1] + X_new[,2] * gamma[2] # only valid for simple regression model with intercept and one predictor for gamma
  names(scale_pred) = as.character(1:length(scale_pred))
  pred_result <- list(location = location_pred, scale = scale_pred)
  return(pred_results)
}

#' summary method for object of class "bayes_hmc_reg"
#'
#' @param object object of class "bayes_hmc_reg"
#' @param ... additional arguments
#'
#' @return summary statistics
#' @export
#'
#' @examples to be added
summary.bayes_hmc_reg <- function(object, ...) {
  # calculate residual summary
  residSUM <- c(min(object$residuals), stats::quantile(object$residuals,0.25),
                stats::median(object$residuals),mean(object$residuals), stats::quantile(object$residuals, 0.75),
                max(object$residuals))
  names(residSUM) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  # extract coefficient information from object
  #location_df <- estimates_with_ci(mean_estimate = object$coefficients$location, samples = object$samples$beta_samples)
  #scale_df <- estimates_with_ci(mean_estimate = object$coefficients$location, samples = object$samples$gamma_samples)

  location_df <- cbind(object$coefficients$location, object$credible_intervals$coef_loc)
  names(location_df)[1] <- "Mean"
  scale_df <- cbind(object$coefficients$scale, object$credible_intervals$coef_scale)
  names(scale_df)[1] <- "Mean"
  #acceptance_rate <- object$acceptance_rate

  # create summary list
  summary_list <- structure(list(
    call = object$call,
    residSUM = residSUM,
    location = location_df,
    scale = scale_df,
    N = object$n_obs
  ))
  # assign S3 class to summary list
  class(summary_list) <- "summary.bayes_hmc_reg"
  return(summary_list)
  # residual standard error
  # multiple r-squared
  # f-statstic?
  # AIC/BIC
}

#' print.summary method for object of class "bayes_hmc_reg"
#'
#' @param x object of class "bayes_hmc_reg"
#' @param ... additional arguments
#'
#' @return print table of summary statistics
#' @export
#'
#' @examples to be added
print.summary.bayes_hmc_reg <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nResiduals:\n")
  print(x$residSUM, digits = 5)
  cat("\nLocation coefficients:\n")
  print(x$location, digits = 5)
  cat("\nScale coefficients:\n")
  print(x$scale, digits = 5)
}
