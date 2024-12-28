# Load necessary packages
library(asp24hmc)
library(doParallel)
library(foreach)
library(MASS)
library(tibble)
library(dplyr)
library(purrr)
library(Rcpp)
library(profvis)
#source("R/methods.R")
#source("R/main_function.R")
#source("R/helper_main_function.R")
#source("find_stepsize.R")
#Rcpp::sourceCpp("src/hmc_sampler.cpp")
#Rcpp::sourceCpp("src/utils.cpp")


# check time
start <- Sys.time()
# Set seed
set.seed(1415)

# Parameters
n_obs <- c(30, 100, 300) # c(30, 100, 500, 1000)
n_parameters <- c(2, 5) # , 10) # c(2, 5, 10)
degree_multicolinearity <- c(0, 0.5, 0.9) # c(0, 0.5, 0.9, 0.99)
n_simulations <- 500

# Create an empty list to store all simulations
all_simulations <- list()


# Loop over number of observations
for (i_obs in n_obs) {
  for (j_parameters in n_parameters) {
    for (l_multicolinearity in degree_multicolinearity) {

      # True parameters for beta and gamma
      beta_true <- seq(length.out = (1 + j_parameters), from = 1, to = 4)
      gamma_true <- seq(length.out = (1 + j_parameters), from = 0.05, to = 0.5)

      # Covariate matrix with multicollinearity
      Sigma_X <- (1 - l_multicolinearity) * diag(j_parameters) +
        l_multicolinearity * matrix(1, j_parameters, j_parameters)

      # Simulate data n_simulations times
      simulations <- map(1:n_simulations, function(k) {
        X <- MASS::mvrnorm(i_obs, mu = rep(0, j_parameters), Sigma = Sigma_X)
        X <- cbind(rep(1, i_obs), X)  # Add intercept

        # Sigma structure for error term
        Sigma_epsilon <- exp(X %*% gamma_true)

        # Sample error terms and calculate response Y
        epsilon <- rnorm(i_obs, mean = 0, sd = Sigma_epsilon)
        Y <- X %*% beta_true + epsilon

        # Return simulation result as a list
        list(X = X, Y = Y)
      })

      # Store the data into a tibble for better organization
      all_simulations <- append(all_simulations, list(
        tibble(
          n_obs = i_obs,
          n_parameters = j_parameters,
          degree_multicolinearity = l_multicolinearity,
          beta_true = list(beta_true),   # Make these list columns
          gamma_true = list(gamma_true), # Make these list columns
          simulation = simulations # Store simulations as a list column
        )
      ))
    }
  }
}


# Combine all into a single tibble
simulation_results <- bind_rows(all_simulations)


all_parameters <- list()

num_core <- detectCores() - 1
cl <- makeCluster(num_core)
clusterSetRNGStream(cl, iseed = 141)
registerDoParallel(cl)
#export to cluster
clusterExport(cl, c("%>%", "tibble", "filter", "simulation_results"))


all_parameters <- foreach(i_obs = n_obs, .combine = 'rbind', .packages = "asp24hmc") %:%
  foreach(j_parameters = n_parameters, .combine = 'rbind', .packages = "asp24hmc") %:%
  foreach(l_multicolinearity = degree_multicolinearity, .combine = 'rbind', .packages = "asp24hmc") %:%
  foreach(k_sim = 1:n_simulations, .combine = 'rbind', .packages = "asp24hmc") %dopar% {

    sim <- simulation_results %>% filter(n_obs == i_obs, n_parameters == j_parameters, degree_multicolinearity == l_multicolinearity)
    beta_true <- sim$beta_true[[1]]
    gamma_true <- sim$gamma_true[[1]]
    sim <- sim$simulation[[k_sim]]

    sim <- as.data.frame(cbind(sim$Y, sim$X[, 2:(1+j_parameters)]))

    split <- floor(0.8 * i_obs)
    # split sim into training and test data

    hold_out <- sim[-(1:split), ]
    sim <- sim[1: split, ]

    # write trycatch around the hmc function
    tryCatch({
      hmc_result <- GLSR_HMC(V1 ~ ., ~., data = sim, NULL, NULL, chain_length = 5000, burn_in = 0, thin = 1)
    }, error = function(e) {
      hmc_result <<- list(mcmc = list(accepted = -1), coeffs = list(location = NA, scale = NA))
    })

    tibble(
      n_obs = i_obs,
      n_parameters = j_parameters,
      degree_multicolinearity = l_multicolinearity,
      beta_true = list(beta_true),
      gamma_true = list(gamma_true),
      n_simulations = k_sim,
      hold_out = list(hold_out),
      #      location_samples = list(hmc_result$mcmc$location),
      #     scale_samples = list(hmc_result$mcmc$scale),
      #    stepsize = hmc_result$mcmc$stepsize,
      acceptance_rate = hmc_result$mcmc$accepted,
      beta_sampled = list(hmc_result$coeffs$location),
      gamma_sampled = list(hmc_result$coeffs$scale),
      #      m_object = list(hmc_result)
      #      location_fit = list(hmc_result$fitted$location_fit),
      #     scale_fit = list(hmc_result$fitted$scale_fit)
    )
  }


stopCluster(cl)


all_parameters_tibble <- bind_rows(all_parameters)

saveRDS(all_parameters_tibble, "all_parameters_tibble.rds")
