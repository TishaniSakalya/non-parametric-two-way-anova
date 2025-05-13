#' Permute Columns of a Data Table
#'
#' This function performs permutation (shuffling) of columns in a data table
#' based on the specified type of permutation. The types available are "main_A",
#' "main_B", and "interaction", which control how the columns are shuffled:
#'
#' \itemize{
#'   \item "main_A": Shuffles values in column `A`.
#'   \item "main_B": Shuffles values in column `B`.
#'   \item "interaction": Shuffles values in column `Y`.
#' }
#'
#' The function ensures that the factor levels of columns `A` and `B` are preserved
#' after shuffling.
#'
#' @param data A data.table containing the columns to be permuted.
#' @param type A character string specifying the type of permutation. One of:
#'   \item "main_A": Permutes column `A`.
#'   \item "main_B": Permutes column `B`.
#'   \item "interaction": Permutes column `Y`.
#' @return A data.table with the specified column permuted, maintaining the factor
#'   levels of columns `A` and `B`.
#' @details
#'   The function creates a copy of the input data to prevent modifying the original
#'   dataset. After the shuffling, it ensures that factor levels of columns `A` and `B`
#'   are maintained for consistency.
#'
#' @examples
#' # Sample data.table
#' data <- data.table(A = factor(c('low', 'medium', 'high', 'low', 'high')),
#'                    B = factor(c('red', 'blue', 'red', 'blue', 'red')),
#'                    Y = c(10, 20, 30, 40, 50))
#'
#' # Permute column A
#' permuted_data_A <- permute_data(data, "main_A")
#'
#' # Permute column B
#' permuted_data_B <- permute_data(data, "main_B")
#'
#' # Permute column Y
#' permuted_data_Y <- permute_data(data, "interaction")
#' @export

permute_data <- function(data, type) {
  permuted_data <- as.data.table(copy(data))

  if (type == "main_A") {
    permuted_data[, A := sample(A)]
  } else if (type == "main_B") {
    permuted_data[, B := sample(B)]
  } else if (type == "interaction") {
    permuted_data[, Y := sample(Y)]
  }

  # Ensure factor levels are re-applied correctly after permuting
  permuted_data[, A := factor(A, levels = unique(data$A))]
  permuted_data[, B := factor(B, levels = unique(data$B))]

  return(permuted_data)
}
