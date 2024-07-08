#' function to generate hmc chain
#'
#' @keywords internal
build_chain <-
  function(target_dist,
           gradient_dist,
           theta_0,
           stepsize,
           trajectory_length ,
           chain_length,
           burn_in,
           thin) {

    chain_matrix <- matrix(NA, nrow = chain_length + 1, ncol = length(theta_0))
    chain_matrix[1,] <- theta_0 # initialize x
    counter_accept <- 0

    for (i in 1:(chain_length)) {
      # call sample function
      result <-
        hmc_sample(
          previous_state = chain_matrix[i,],
          stepsize = stepsize,
          trajectory_length = trajectory_length,
          log_prob = target_dist,
          gradient_lp = gradient_dist
        )
      new_value <- result$x
      accept <- result$accept

      chain_matrix[1 + i,] <- new_value
      if (accept) {
        counter_accept <- counter_accept + 1
      }
    }
    acceptance_rate <- counter_accept / chain_length

    # remove initial values (first row) from the chain
    chain_matrix = chain_matrix[-1,]
    # apply burn-in and thinning (activation condition -> following part only applied then)
    chain_matrix = chain_matrix[-(1:burn_in),]
    # apply thinning
    chain_matrix = chain_matrix[seq(1,nrow(chain_matrix), by = thin),]

    return(list(samples = chain_matrix, acceptance_rate = acceptance_rate))
  }

#' function to generate hmc sample
#'
#' @keywords internal
hmc_sample <-
  function(previous_state,
           stepsize,
           trajectory_length,
           log_prob,
           gradient_lp) {
    impuls_old <- stats::rnorm(length(previous_state), 0, 1)
    result <-
      leapfrog_hmc(previous_state,
               impuls_old,
               trajectory_length,
               stepsize,
               gradient_lp)
    new_state <- result$x
    impuls_new <- result$v

    log_a_unif <- log(stats::runif(1))
    accept <-
      ifelse((
        log_a_unif < log_acceptance_criterion(previous_state, impuls_old, new_state, impuls_new, log_prob)
      ), TRUE, FALSE)

    if (accept) {
      return(list(x = new_state, accept = accept))
    } else{
      return(list(x = previous_state, accept = accept))
    }
  }

#' leapfrog hmc function
#'
#' @keywords internal
leapfrog_hmc <-
  function(x,
           v,
           trajectory_length,
           stepsize,
           gradient_lp) {
    v <- v + 1 / 2 * stepsize * gradient_lp(x)
    simulation_state <- list() # empty list to store value updates
    simulation_impuls <- list(v) # list to store impuls updates
    for(i in 1:(trajectory_length - 1)) {
      x <- x + stepsize * v
      v <- v + stepsize * gradient_lp(x)
      simulation_state[[i]] <- x
      simulation_impuls[[i + 1]] <- v
    }
    x <- x + stepsize * v
    v <- v + stepsize * gradient_lp(x)
    simulation_state[[trajectory_length]] <- x
    simulation_impuls[[trajectory_length + 1]] <- v
    return(list(x = x, v = v, simulation_state = simulation_state, simulation_impuls = simulation_impuls))
  }

#' function to compute log acceptance criterion
#'
#' @keywords internal
log_acceptance_criterion <-
  function(x_old, v_old, x_new, v_new, log_prob) {
    #acceptance_prob_log <-
      #min(0,-(H(log_prob, x_new, v_new) - H(log_prob, x_old, v_old)))
    #return(acceptance_prob_log)
    H_old <- H(log_prob, x_old, v_old)
    H_new <- H(log_prob, x_new, v_new)
    return(min(0, -(H_new - H_old)))
  }

#' function to compute Hamiltonian
#'
#' @keywords internal
H <- function(log_prob, x, v) {
  #return(-log_prob(x) + 1 / 2 * sum(v ^ 2))
  return(-log_prob(x) + 0.5 * sum(v ^ 2))
}
