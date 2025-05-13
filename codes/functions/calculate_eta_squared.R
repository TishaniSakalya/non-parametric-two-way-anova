#' Calculate Eta-Squared for a Two-Way ANOVA
#'
#' This function computes eta-squared (η²) effect sizes for the main effects (A and B)
#' and their interaction (A:B) in a two-way ANOVA.
#'
#' @param fit A fitted model object from `aov()`, representing a two-way ANOVA model.
#' @param data A data frame containing the dependent variable `Y` and the factors `A` and `B`.
#'
#' @return A list with the following components:
#' \item{eta_A}{Eta-squared for the main effect of A.}
#' \item{eta_B}{Eta-squared for the main effect of B.}
#' \item{eta_interaction}{Eta-squared for the interaction effect (A:B).}
#'
#' @details
#' Eta-squared is calculated as the sum of squares (SS) for each factor divided by the total SS:
#' \deqn{\eta^2 = SS_{factor} / SS_{total}}
#' The function assumes a two-way ANOVA model with factors `A` and `B`.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' my_data <- data.frame(
#'   A = factor(rep(1:2, each=10)),
#'   B = factor(rep(1:2, times=10)),
#'   Y = rnorm(20)
#' )
#'
#' # Fit ANOVA model
#' fit <- aov(Y ~ A * B, data = my_data)
#'
#' # Compute eta-squared values
#' calculate_eta_squared(fit, my_data)
#' }
#'
#' @export
calculate_eta_squared <- function(fit, data) {
  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("Error: The dataset is empty.")
  }

  # Ensure data has Y and it's numeric
  if (!"Y" %in% colnames(data) || !is.numeric(data$Y)) {
    stop("Error: Y must be a numeric column in the dataset.")
  }

  # Check if model fit is valid
  if (!inherits(fit, "lm")) {
    stop("Error: Input model must be a linear model (lm object).")
  }

  # Try computing ANOVA safely
  anova_table <- tryCatch(anova(fit), error = function(e) stop("Error in ANOVA computation."))

  # Ensure ANOVA table contains expected terms
  required_terms <- c("A", "B", "A:B")
  missing_terms <- setdiff(required_terms, rownames(anova_table))
  if (length(missing_terms) > 0) {
    stop(paste("Error: ANOVA table is missing terms:", paste(missing_terms, collapse = ", ")))
  }

  # Compute sum of squares
  ss_A <- anova_table["A", "Sum Sq"]
  ss_B <- anova_table["B", "Sum Sq"]
  ss_interaction <- anova_table["A:B", "Sum Sq"]

  # Compute total sum of squares
  total_ss <- sum((data$Y - mean(data$Y))^2)
  if (total_ss == 0) {
    stop("Error: Total sum of squares is zero, likely due to constant Y values.")
  }

  # Compute effect sizes
  eta_A <- ss_A / total_ss
  eta_B <- ss_B / total_ss
  eta_interaction <- ss_interaction / total_ss

  return(list(eta_A = eta_A, eta_B = eta_B, eta_interaction = eta_interaction))
}
