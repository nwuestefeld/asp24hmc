library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(purrr)


df_full <- readRDS("all_parameters_tibble.rds")


### code generating MSE plot

order_simulations <- function(sim_tibble, n_sims) {

  cycle_length <- nrow(sim_tibble) / n_sims
  sim_list <- split(sim_tibble, rep(1:cycle_length, each = n_sims))

  return(sim_list)
}


erg <- order_simulations(df_full, 500)


all_list <- list()
for(i in 1:length(erg)) {
  # extract components of the cycle
  n_obs <- erg[[i]]$n_obs[1]
  n_params <- erg[[i]]$n_parameters[1]
  d_multi <- erg[[i]]$degree_multicolinearity[1]
  beta_sampled <- erg[[i]]$beta_sampled
  acceptance_rate <- erg[[i]]$acceptance_rate
  test <- erg[[i]]$hold_out
  all_list[[i]] <- tibble::tibble(n_obs = n_obs, n_params = n_params, d_multi = d_multi, beta_sampled, test = test, accept = acceptance_rate)
}

results <- list()
for(i in 1:length(all_list)) {
  n_obs <- all_list[[i]]$n_obs[1]
  n_params <- all_list[[i]]$n_params[1]
  d_m <- all_list[[i]]$d_multi[1]

  mse <-c()

  for(j in 1:length(all_list[[i]]$test)) {
    if(all_list[[i]]$accept[j] >= 0) {
      y_true <- all_list[[i]]$test[[j]]$V1
      X <- all_list[[i]]$test[[j]][,-1]
      intercept <- all_list[[i]]$beta_sampled[[j]][1]
      beta <- all_list[[i]]$beta_sampled[[j]][-1]
      y_pred <- intercept + beta %*% t(X)
      mse <- c(mse, (y_true - y_pred)^2)
    }
  }
  results[[i]] <- tibble::tibble(n_obs = n_obs, n_params = n_params, d_m = d_m, mse_avg=mean(mse))
}

all_tibble <- dplyr::bind_rows(results)

ggplot2::ggplot(all_tibble, aes(x = n_obs, y = mse_avg, color = interaction(n_params, d_m), group = interaction(n_params, d_m))) +
  geom_point() +
  geom_line(lty = 2) +
  labs(
    title = "Mean squared error for different combinations",
    x = "Number of Observations",
    y = "MSE",
    color = "Combination",
    shape = "Degree of Multicollinearity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),      # Title size
    axis.title.x = element_text(size = 16),                  # X-axis label size
    axis.title.y = element_text(size = 16),                  # Y-axis label size
    axis.text = element_text(size = 14),                     # Axis text size (ticks)
    legend.title = element_text(size = 16),                  # Legend title size
    legend.text = element_text(size = 14),                   # Legend text size
    strip.text = element_text(size = 15)                     # Facet label size
  )



### code generating bias plots

# Load required packages
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

df_full <- readRDS("all_parameters_tibble.rds")

filter <- df_full$acceptance_rate > 0.01
df <- df_full[filter,]

#process_and_plot_sim(df_full, 500)


order_simulations <- function(sim_tibble, n_sims) {

  cycle_length <- nrow(sim_tibble) / n_sims
  sim_list <- split(sim_tibble, rep(1:cycle_length, each = n_sims))

  return(sim_list)
}

erg <- order_simulations(df_full, 500)

all_list <- list()
for(i in 1:length(erg)) {
  # extract components of the cycle
  n_obs <- erg[[i]]$n_obs[1]
  n_params <- erg[[i]]$n_parameters[1]
  d_multi <- erg[[i]]$degree_multicolinearity[1]
  beta_true <- erg[[i]]$beta_true[[1]]
  gamma_true <- erg[[i]]$gamma_true[[1]]

  # extract matrix of estimated coefficients of the cycle
  sample_beta <- do.call(rbind, erg[[i]]$beta_sampled)
  sample_gamma <- do.call(rbind, erg[[i]]$gamma_sampled)

  # compute average estimate across simulations
  mean_beta <- colMeans(sample_beta, na.rm = TRUE)
  mean_gamma <- colMeans(sample_gamma, na.rm = TRUE)
  # compute bias
  bias_beta <- beta_true - mean_beta
  bias_gamma <- gamma_true - mean_gamma
  all_list[[i]] <- tibble::tibble(n_obs = n_obs, n_params = n_params, d_multi = d_multi, bias_beta = list(bias_beta), bias_gamma = list(bias_gamma))
}
all_tibble <- dplyr::bind_rows(all_list)

erg_long <- all_tibble %>%
  tidyr::unnest(bias_beta) %>%
  tidyr::unnest(bias_gamma) %>%
  dplyr::mutate(bias_type_beta = names(bias_beta)) %>%
  dplyr::mutate(bias_type_gamma = names(bias_gamma))
# plot bias location
ggplot2::ggplot(erg_long, aes(x = n_obs, y = bias_beta, color = interaction(n_params, d_multi), group = interaction(n_params, d_multi))) +
  geom_point() +
  geom_line(lty = 2) +
  facet_wrap(~ bias_type_beta, scales = "free_y") +
  labs(
    title = "Bias of the model for location parameters for different combinations",
    x = "Number of Observations",
    y = "Bias",
    color = "Combination",
    shape = "Degree of Multicollinearity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),      # Title size
    axis.title.x = element_text(size = 16),                  # X-axis label size
    axis.title.y = element_text(size = 16),                  # Y-axis label size
    axis.text = element_text(size = 14),                     # Axis text size (ticks)
    legend.title = element_text(size = 16),                  # Legend title size
    legend.text = element_text(size = 14),                   # Legend text size
    strip.text = element_text(size = 15)                     # Facet label size
  )

ggplot2::ggplot(erg_long, aes(x = n_obs, y = bias_gamma, color = interaction(n_params, d_multi), group = interaction(n_params, d_multi))) +
  geom_point() +
  geom_line(lty = 2) +
  facet_wrap(~ bias_type_gamma, scales = "free_y") +
  labs(
    title = "Bias of the model for scale parameters for different combinations",
    x = "Number of Observations",
    y = "Bias",
    color = "Combination",
    shape = "Degree of Multicollinearity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),      # Title size
    axis.title.x = element_text(size = 16),                  # X-axis label size
    axis.title.y = element_text(size = 16),                  # Y-axis label size
    axis.text = element_text(size = 14),                     # Axis text size (ticks)
    legend.title = element_text(size = 16),                  # Legend title size
    legend.text = element_text(size = 14),                   # Legend text size
    strip.text = element_text(size = 15)                     # Facet label size
  )

