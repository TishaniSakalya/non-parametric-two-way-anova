#' Permute Data and Fit Linear Model
#'
#' This function repeatedly permutes a dataset, fits a linear model (`lm`), 
#' and calculates eta-squared values for the main effects and interaction term.
#'
#' @param data A data.table containing the columns `A`, `B`, and `Y`.
#' @param type A character string specifying the type of permutation:
#'   \itemize{
#'     \item `"main_A"`: Permutes column `A`.
#'     \item `"main_B"`: Permutes column `B`.
#'     \item `"interaction"`: Permutes column `Y`.
#'   }
#' @param B An integer specifying the number of permutations to perform.
#'
#' @return A list of length `B`, where each element is a list containing:
#'   \itemize{
#'     \item `eta_A`: Eta-squared value for `A`.
#'     \item `eta_B`: Eta-squared value for `B`.
#'     \item `eta_interaction`: Eta-squared value for the interaction effect (`A * B`).
#'   }
#'
#' @details
#'   The function uses the `permute_data()` function to shuffle the specified column
#'   and then fits a linear model (`lm(Y ~ A * B)`) to the permuted data. The 
#'   `calculate_eta_squared()` function is used to extract the eta-squared values 
#'   from the model fit.
#'
#' @examples
#' # Sample data.table
#' data <- data.table(A = factor(c('low', 'medium', 'high', 'low', 'high')),
#'                    B = factor(c('red', 'blue', 'red', 'blue', 'red')),
#'                    Y = c(10, 20, 30, 40, 50))
#'
#' # Perform 100 permutations and fit models
#' results <- permute_and_fit(data, type = "main_A", B = 100)
#'
#' @export
permute_and_fit <- function(data, type, B) {
  results <- replicate(B, {
    permuted_data <- permute_data(data, type)
    fit_perm <- lm(Y ~ A * B, data = permuted_data)
    ss_eta <- calculate_eta_squared(fit_perm, permuted_data)
    return(list(eta_A = ss_eta$eta_A, eta_B = ss_eta$eta_B, eta_interaction = ss_eta$eta_interaction))
  }, simplify = FALSE)
  
  return(results)
}
