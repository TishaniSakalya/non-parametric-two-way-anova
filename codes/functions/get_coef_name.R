#' Extract Coefficient Name for a Given Term
#'
#' This function retrieves the coefficient name from a fitted model object
#' that matches a specified term.
#'
#' @param fit A fitted model object (e.g., from `lm` or `glm`).
#' @param term A character string specifying the term for which to find the coefficient name.
#'
#' @return A character string representing the coefficient name if found.
#' @return An error if the specified term is not found in the model.
#'
#' @examples
#' model <- lm(mpg ~ hp + wt, data = mtcars)
#' get_coef_name(model, "hp")
#'
#' @export

get_coef_name <- function(fit, term) {
  coef_names <- names(coef(fit))

  # Match coefficients that contain the term
  match <- grep(term, coef_names, value = TRUE)

  if (length(match) > 0) {
    return(match[1])  # Return the first match
  } else {
    stop(paste("Coefficient", term, "not found in model"))
  }
}

