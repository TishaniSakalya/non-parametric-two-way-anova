#' Compute Eta Squared Effect Size
#'
#' This function calculates eta squared (\eqn{\eta^2}), which measures the proportion
#' of total variance in the response variable explained by the fitted model.
#'
#' @param fit A fitted linear model object from \code{lm()}.
#' @return A numeric value representing the eta squared effect size.
#' @details Eta squared (\eqn{\eta^2}) is computed as:
#' \deqn{\eta^2 = \frac{SS_{effect}}{SS_{total}}}
#' where \eqn{SS_{effect}} is the sum of squares due to the model, and
#' \eqn{SS_{total}} is the total sum of squares.
#'
#' @examples
#' data <- data.frame(
#'   A = factor(rep(c("A1", "A2"), each = 10)),
#'   B = factor(rep(c("B1", "B2"), times = 10)),
#'   Y = rnorm(20)
#' )
#' fit <- lm(Y ~ A * B, data = data)
#' eta_squared(fit)
#'
#' @export

eta_squared <- function(fit) {
  # Check if the fit object is a linear model (lm) or a generalized linear model (glm)
  if (!inherits(fit, c("lm", "glm"))) {
    stop("Error: The input must be a linear model (lm) or generalized linear model (glm).")
  }

  # Check if the model has a response variable Y
  if (is.null(fit$model$Y)) {
    stop("Error: The model does not contain a response variable 'Y'.")
  }

  # Calculate the sum of squares for effect (SS_effect)
  ss_effect <- sum((fitted(fit) - mean(fit$model$Y))^2)

  # Calculate the total sum of squares (SS_total)
  ss_total <- sum((fit$model$Y - mean(fit$model$Y))^2)

  # Check if the total sum of squares is zero (which would lead to a division by zero)
  if (ss_total == 0) {
    stop("Error: The total sum of squares is zero. This might indicate constant values for Y.")
  }

  # Return eta squared (effect size)
  return(ss_effect / ss_total)
}
