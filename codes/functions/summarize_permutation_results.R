#' Summarize Permutation Results
#'
#' This function summarizes the permutation results by calculating the mean,
#' standard deviation, and 95% confidence intervals for the eta-squared values
#' of the main effects (`A`, `B`) and the interaction effect (`A * B`).
#'
#' @param results A list of length `B` where each element is a list containing
#'   the eta-squared values for `A`, `B`, and the interaction effect from the
#'   permutation procedure.
#'
#' @return A list containing the summary statistics for each effect:
#'   \itemize{
#'     \item `summary_A`: A named vector with mean, standard deviation, and
#'       95% confidence intervals for eta-squared values of `A`.
#'     \item `summary_B`: A named vector with mean, standard deviation, and
#'       95% confidence intervals for eta-squared values of `B`.
#'     \item `summary_interaction`: A named vector with mean, standard deviation,
#'       and 95% confidence intervals for eta-squared values of the interaction effect (`A * B`).
#'   }
#'
#' @details
#'   This function assumes that the input `results` is generated from the
#'   `permute_and_fit()` function, which returns a list of eta-squared values.
#'   It computes the summary statistics for each effect, providing insight into
#'   the variability and central tendency of the permutation results.
#'
#' @examples
#' # Example permutation results
#' results <- list(
#'   list(eta_A = 0.2, eta_B = 0.3, eta_interaction = 0.1),
#'   list(eta_A = 0.25, eta_B = 0.35, eta_interaction = 0.15),
#'   list(eta_A = 0.22, eta_B = 0.32, eta_interaction = 0.12)
#' )
#'
#' # Summarize the results
#' summary_results <- summarize_permutation_results(results)
#'
#' @export

summarize_permutation_results <- function(results) {
  eta_A_values <- sapply(results, function(x) x$eta_A)
  eta_B_values <- sapply(results, function(x) x$eta_B)
  eta_interaction_values <- sapply(results, function(x) x$eta_interaction)

  # Extract quantile values correctly
  ci_A <- quantile(eta_A_values, c(0.025, 0.975), names = FALSE)
  ci_B <- quantile(eta_B_values, c(0.025, 0.975), names = FALSE)
  ci_interaction <- quantile(eta_interaction_values, c(0.025, 0.975), names = FALSE)

  summary_A <- c(mean = mean(eta_A_values), sd = sd(eta_A_values), ci_lower = ci_A[1], ci_upper = ci_A[2])
  summary_B <- c(mean = mean(eta_B_values), sd = sd(eta_B_values), ci_lower = ci_B[1], ci_upper = ci_B[2])
  summary_interaction <- c(mean = mean(eta_interaction_values), sd = sd(eta_interaction_values), ci_lower = ci_interaction[1], ci_upper = ci_interaction[2])

  return(list(summary_A = summary_A, summary_B = summary_B, summary_interaction = summary_interaction))
}
