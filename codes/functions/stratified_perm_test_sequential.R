#' Stratified Permutation Test with Sequential Permutations
#'
#' This function performs a stratified permutation test with sequential permutations,
#' where data for the main effects (`A` and `B`) and interaction (`A * B`) are permuted separately.
#' It calculates eta-squared values for each effect, computes p-values by comparing
#' permuted values with the observed eta-squared values, and generates density plots for visualization.
#'
#' @param data A data.table containing the columns `A`, `B`, and `Y`.
#' @param B An integer specifying the number of permutations to perform. Default is 1000.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `eta_A`: The observed eta-squared value for the main effect `A`.
#'     \item `eta_B`: The observed eta-squared value for the main effect `B`.
#'     \item `eta_interaction`: The observed eta-squared value for the interaction effect (`A * B`).
#'     \item `p_eta_A`: The p-value for the main effect `A`.
#'     \item `p_eta_B`: The p-value for the main effect `B`.
#'     \item `p_eta_interaction`: The p-value for the interaction effect (`A * B`).
#'   }
#'
#' @details
#'   The function runs `B` permutations, where data for `A`, `B`, and their interaction
#'   are permuted separately. For each permutation, eta-squared values are calculated and
#'   compared with the observed values. P-values are computed as the proportion of
#'   permuted values greater than or equal to the observed values.
#'
#'   The function also generates density plots for the eta-squared values of `A`, `B`,
#'   and their interaction, with the observed values shown as dashed lines.
#'
#' @import ggplot2
#' @import data.table
#' @importFrom stats lm
#'
#' @examples
#' # Example data.table
#' data <- data.table(A = factor(c('low', 'medium', 'high', 'low', 'high')),
#'                    B = factor(c('red', 'blue', 'red', 'blue', 'red')),
#'                    Y = c(10, 20, 30, 40, 50))
#'
#' # Perform stratified permutation test with 1000 permutations
#' results <- stratified_perm_test_sequential(data, B = 1000)
#' print(results)
#'
#' # Create a data frame to mimic an ANOVA table
#' anova_table <- data.frame(
#'   Term = c("A", "B", "A:B (Interaction)"),
#'   Eta_Squared = c(results$eta_A, results$eta_B, results$eta_interaction),
#'   P_Value = c(results$p_eta_A, results$p_eta_B, results$p_eta_interaction),
#'   Significance = c(
#'     ifelse(results$p_eta_A < 0.05, "***", ""),
#'     ifelse(results$p_eta_B < 0.05, "***", ""),
#'     ifelse(results$p_eta_interaction < 0.05, "**", "")
#'   )
#' )
#'
#' # Print the table in a clear format
#' print(anova_table)
#'
#' @export
stratified_perm_test_sequential <- function(data, B = 1000) {

  B <- as.integer(B[1])  # Ensure B is a single integer
  if (is.na(B) || B <= 0) stop("B must be a positive integer")

  cat("B inside stratified_perm_test_sequential:", B, "\n")  # Debugging print

  fit <- lm(Y ~ A * B, data = data)
  eta_squared_obs <- calculate_eta_squared(fit, data)

  # Ensure eta_squared_obs values are numeric
  eta_squared_obs$eta_A <- as.numeric(eta_squared_obs$eta_A)
  eta_squared_obs$eta_B <- as.numeric(eta_squared_obs$eta_B)
  eta_squared_obs$eta_interaction <- as.numeric(eta_squared_obs$eta_interaction)

  # Run permutations sequentially
  perm_results <- vector("list", B)
  for (i in 1:B) {
    perm_data_A <- permute_data(data, "main_A")
    perm_data_B <- permute_data(data, "main_B")
    perm_data_AB <- permute_data(data, "interaction")

    fit_A <- lm(Y ~ A * B, data = perm_data_A)
    fit_B <- lm(Y ~ A * B, data = perm_data_B)
    fit_AB <- lm(Y ~ A * B, data = perm_data_AB)

    perm_eta_A <- calculate_eta_squared(fit_A, perm_data_A)$eta_A
    perm_eta_B <- calculate_eta_squared(fit_B, perm_data_B)$eta_B
    perm_eta_interaction <- calculate_eta_squared(fit_AB, perm_data_AB)$eta_interaction

    # Ensure perm_eta values are numeric
    perm_eta_A <- as.numeric(perm_eta_A)
    perm_eta_B <- as.numeric(perm_eta_B)
    perm_eta_interaction <- as.numeric(perm_eta_interaction)

    perm_results[[i]] <- list(
      eta_A = perm_eta_A,
      eta_B = perm_eta_B,
      eta_interaction = perm_eta_interaction
    )
  }

  # Extract permuted values
  perm_eta_A <- sapply(perm_results, `[[`, "eta_A")
  perm_eta_B <- sapply(perm_results, `[[`, "eta_B")
  perm_eta_interaction <- sapply(perm_results, `[[`, "eta_interaction")

  # Compute p-values (checking greater than observed)
  p_eta_A <- mean(perm_eta_A >= eta_squared_obs$eta_A)
  p_eta_B <- mean(perm_eta_B >= eta_squared_obs$eta_B)
  p_eta_interaction <- mean(perm_eta_interaction >= eta_squared_obs$eta_interaction)

  # Create separate data frames for visualization
  perm_df_A <- data.frame(eta = perm_eta_A, type = "A")
  perm_df_B <- data.frame(eta = perm_eta_B, type = "B")
  perm_df_interaction <- data.frame(eta = perm_eta_interaction, type = "Interaction")

  # Main Effect A Plot
  plot_A <- ggplot(perm_df_A, aes(x = eta)) +
    geom_density(fill = "blue", alpha = 0.5) +
    geom_vline(aes(xintercept = eta_squared_obs$eta_A), color = "darkblue", linetype = "dashed", linewidth = 1) +
    labs(title = "Main Effect A", x = "Eta Squared for Main Effect A", y = "Density") +
    theme_minimal()

  # Main Effect B Plot
  plot_B <- ggplot(perm_df_B, aes(x = eta)) +
    geom_density(fill = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = eta_squared_obs$eta_B), color = "darkred", linetype = "dashed", linewidth = 1) +
    labs(title = "Main Effect B", x = "Eta Squared for Main Effect B", y = "Density") +
    theme_minimal()

  # Interaction Plot
  plot_interaction <- ggplot(perm_df_interaction, aes(x = eta)) +
    geom_density(fill = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = eta_squared_obs$eta_interaction), color = "darkgreen", linetype = "dashed", linewidth = 1) +
    labs(title = "Interaction (A:B)", x = "Eta Squared for Interaction A:B", y = "Density") +
    theme_minimal()

  # Show the plots
  print(plot_A)
  print(plot_B)
  print(plot_interaction)

  # Return the results and p-values
  return(list(
    eta_A = eta_squared_obs$eta_A,
    eta_B = eta_squared_obs$eta_B,
    eta_interaction = eta_squared_obs$eta_interaction,
    p_eta_A = p_eta_A,
    p_eta_B = p_eta_B,
    p_eta_interaction = p_eta_interaction
  ))
}
