#' Power Analysis Using Permutation Testing
#'
#' This function performs a power analysis for a two-way factorial design using permutation testing.
#' It estimates the statistical power for the main effects and their interaction by running multiple simulations.
#'
#' @name power_analysis_permutation_future
#' @param n_per_group Numeric. Number of subjects per group in the two-way factorial design.
#' @param effect_size Numeric. The effect size for the interaction term between factors A and B.
#' @param num_simulations Numeric. The number of simulations to run.
#' @param alpha Numeric. The significance level (default is 0.05).
#' @param B Numeric. The number of permutations to perform in each simulation.
#'
#' @return A list containing the estimated power for each effect (main effects and interaction).
#' \itemize{
#'   \item estimated_power_A: Estimated power for the main effect of A.
#'   \item estimated_power_B: Estimated power for the main effect of B.
#'   \item estimated_power_interaction: Estimated power for the interaction effect.
#'   \item num_simulations: Number of simulations performed.
#'   \item alpha: Significance level used for the power calculation.
#' }
#' @export
#'
#' @examples
#' set.seed(123)
#' power_results <- power_analysis_permutation_future(
#'   n_per_group = 100,
#'   effect_size = 0.3,
#'   num_simulations = 200,
#'   alpha = 0.05,
#'   B = 500
#' )
#' print(power_results)

library(future)
library(future.apply)

power_analysis_permutation_future <- function(n_per_group, effect_size,
                                              num_simulations, alpha, B) {
  # Validate B
  B <- as.integer(B)
  if (is.na(B) || B <= 0) stop("B must be a positive integer")

  # Run simulations using parallel processing or sequential if parallel fails
  results <- tryCatch({
    future_lapply(1:num_simulations, function(i) {
      A <- rep(c("A1", "A2"), each = n_per_group * 2)
      B_factor <- rep(c("B1", "B2"), times = n_per_group * 2)

      # Simulate normal data
      Y <- rnorm(length(A), mean = ifelse(A == "A1" & B_factor == "B1", 0 + effect_size, 0), sd = 1)
      data <- data.frame(A = factor(A), B = factor(B_factor), Y = Y)

      # Perform stratified permutation test (make sure it's defined)
      res <- stratified_perm_test_sequential(data, B = B)

      # Return whether the p-values are less than the alpha threshold
      c(p_A = as.numeric(res$p_eta_A < alpha),
        p_B = as.numeric(res$p_eta_B < alpha),
        p_interaction = as.numeric(res$p_eta_interaction < alpha))
    })
  }, error = function(e) {
    message("Parallel execution failed, using sequential fallback.")

    # Fallback to sequential if parallel fails
    lapply(1:num_simulations, function(i) {
      A <- rep(c("A1", "A2"), each = n_per_group * 2)
      B_factor <- rep(c("B1", "B2"), times = n_per_group * 2)

      # Simulate normal data
      Y <- rnorm(length(A), mean = ifelse(A == "A1" & B_factor == "B1", 0 + effect_size, 0), sd = 1)
      data <- data.frame(A = factor(A), B = factor(B_factor), Y = Y)

      # Perform stratified permutation test (make sure it's defined)
      res <- stratified_perm_test_sequential(data, B = B)

      # Return whether the p-values are less than the alpha threshold
      c(p_A = as.numeric(res$p_eta_A < alpha),
        p_B = as.numeric(res$p_eta_B < alpha),
        p_interaction = as.numeric(res$p_eta_interaction < alpha))
    })
  })

  # Combine results into a matrix
  results <- do.call(rbind, results)

  # Calculate the estimated power
  return(list(
    estimated_power_A = mean(results[, "p_A"]),
    estimated_power_B = mean(results[, "p_B"]),
    estimated_power_interaction = mean(results[, "p_interaction"]),
    num_simulations = num_simulations,
    alpha = alpha
  ))
}

