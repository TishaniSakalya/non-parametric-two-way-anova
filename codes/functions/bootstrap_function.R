#' Bootstrap Resampling Functions
#'
#' A collection of functions to perform different bootstrap resampling techniques
#' for estimating effect sizes in a linear model with interaction terms.
#'
#' @param data A data frame containing the variables `Y`, `A`, and `B`.
#' @param num_samples Integer, the number of bootstrap resamples (default: 1000).
#' @param trim Numeric, proportion of residuals to winsorize in the winsorized bootstrap (default: 0.05).
#'
#' @return A numeric vector containing bootstrap estimates of effect size (eta squared).
#'
#' @details
#' This list includes the following bootstrap methods:
#'
#' - **residual**: Resamples residuals from the fitted model and adds them back to fitted values.
#' - **wild**: Uses randomly chosen multipliers (-1 or 1) on residuals to create resampled responses.
#' - **pairwise**: Performs resampling by drawing observations with replacement.
#' - **winsorized**: Trims extreme residuals to reduce the influence of outliers before resampling.
#'
#' @examples
#' data <- data.frame(Y = rnorm(100), A = factor(rep(1:2, 50)), B = factor(rep(1:2, each = 50)))
#' results <- bootstrap_functions$residual(data)
#' hist(results)
#'
#' # Obtaining results using the function
#'
#' bootstrap_type <- choose_bootstrap_type(data)
#' print(paste("Using", bootstrap_type, "bootstrap"))
#' results <- bootstrap_functions[[bootstrap_type]](data, num_samples = 1000)
#' boot_ci <- quantile(results, c(0.025, 0.975))  # 95% confidence interval
#' boot_se <- sd(results)  # Standard error
#' print(paste("Bootstrap 95% CI:", boot_ci[1], "to", boot_ci[2]))
#' print(paste("Bootstrap Standard Error:", boot_se))
#'
#' # Store results in a data frame
#'
#' boot_df <- data.frame(eta = results, type = "Bootstrap")
#'
#' # Visualizing the bootstrap distribution
#' ggplot(boot_df, aes(x = eta)) +
#'   geom_density(fill = "lightblue", color = "blue", alpha = 0.5) +
#'   labs(title = "Bootstrap Distribution of Eta Squared",
#'        x = "Eta Squared (Effect Size)",
#'        y = "Density") +
#'   theme_minimal()
#'
#' # Summarizing bootstrap results in a data frame
#' results_df <- data.frame(
#'   bootstrap_type = bootstrap_type,
#'   mean_eta = mean(results),
#'   ci_lower = boot_ci[1],
#'   ci_upper = boot_ci[2],
#'   se_eta = boot_se
#' )
#'
#'
bootstrap_functions <- list(
  residual = function(data, num_samples = 1000) {
    if (!is.numeric(data$Y)) {
      stop("Error: 'Y' must be numeric for bootstrap resampling.")
    }

    fit <- lm(Y ~ A * B, data = data)
    residuals <- residuals(fit)
    fitted_values <- fitted(fit)

    boot_stats <- replicate(num_samples, {
      data$Y <- fitted_values + sample(residuals, replace = TRUE)
      eta_squared(lm(Y ~ A * B, data = data))
    })
    return(boot_stats)
  },

  wild = function(data, num_samples = 1000) {
    if (!is.numeric(data$Y)) {
      stop("Error: 'Y' must be numeric for bootstrap resampling.")
    }
    fit <- lm(Y ~ A * B, data = data)
    residuals <- residuals(fit)
    fitted_values <- fitted(fit)

    boot_stats <- replicate(num_samples, {
      weights <- sample(c(-1, 1), length(residuals), replace = TRUE)
      data$Y <- fitted_values + residuals * weights
      eta_squared(lm(Y ~ A * B, data = data))
    })
    return(boot_stats)
  },

  pairwise = function(data, num_samples = 1000) {
    if (!is.numeric(data$Y)) {
      stop("Error: 'Y' must be numeric for bootstrap resampling.")
    }
    boot_stats <- replicate(num_samples, {
      boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
      eta_squared(lm(Y ~ A * B, data = boot_sample))
    })
    return(boot_stats)
  },

  winsorized = function(data, num_samples = 1000, trim = 0.05) {
    if (!is.numeric(data$Y)) {
      stop("Error: 'Y' must be numeric for bootstrap resampling.")
    }
    fit <- lm(Y ~ A * B, data = data)
    residuals <- residuals(fit)
    fitted_values <- fitted(fit)

    lower_bound <- quantile(residuals, trim)
    upper_bound <- quantile(residuals, 1 - trim)
    winsorized_residuals <- pmax(pmin(residuals, upper_bound), lower_bound)

    boot_stats <- replicate(num_samples, {
      data$Y <- fitted_values + sample(winsorized_residuals, replace = TRUE)
      eta_squared(lm(Y ~ A * B, data = data))
    })
    return(boot_stats)
  }
)
