#' Choose the Best Bootstrap Type
#'
#' This function determines the most appropriate bootstrap method based on 
#' heteroscedasticity, normality, and presence of outliers in the data.
#'
#' @param data A data frame containing variables Y (response), A, and B (categorical predictors).
#' @return A string indicating the recommended bootstrap method: 
#'   \itemize{
#'     \item "winsorized" - Recommended when heteroscedasticity and outliers are detected.
#'     \item "wild" - Suggested for handling heteroscedasticity without outliers.
#'     \item "residual" - Selected if residuals are normally distributed.
#'     \item "pairwise" - Default fallback when residuals are non-normal.
#'   }
#' @import lmtest car stats
#' @export
#' @examples
#' data <- data.frame(
#'   A = factor(rep(c("A1", "A2"), each = 10)),
#'   B = factor(rep(c("B1", "B2"), times = 10)),
#'   Y = rnorm(20)
#' )
#' choose_bootstrap_type(data)
choose_bootstrap_type <- function(data) {
  fit <- lm(Y ~ A * B, data = data)
  
  # Homoscedasticity tests
  bp_test <- bptest(fit)$p.value
  levene_test <- leveneTest(Y ~ A * B, data = data)[1, "Pr(>F)"]
  
  # Normality check for residuals (Shapiro-Wilk test)
  shapiro_test <- if (nrow(data) < 5000) shapiro.test(residuals(fit))$p.value else NA
  
  # Outlier detection
  outliers <- sum(abs(scale(residuals(fit))) > 3)
  
  if (bp_test < 0.05 || levene_test < 0.05) {  # Heteroscedasticity detected
    if (outliers > 0) {
      return("winsorized")  # Robust to outliers
    } else {
      return("wild")  # Handles heteroscedasticity
    }
  } else {  # Homoscedasticity detected
    if (!is.na(shapiro_test) && shapiro_test > 0.05) {
      return("residual")  # Residuals are normally distributed
    } else {
      return("pairwise")  # More general case
    }
  }
}
