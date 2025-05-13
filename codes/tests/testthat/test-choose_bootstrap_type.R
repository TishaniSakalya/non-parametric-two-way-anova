
library(testthat)
library(car)  # For leveneTest
library(lmtest)  # For bptest

test_that("choose_bootstrap_type returns a valid bootstrap method", {
  # Generate a simple dataset
  set.seed(123)
  data <- data.frame(
    Y = rnorm(100),
    A = factor(rep(1:2, 50)),
    B = factor(rep(1:2, each = 50))
  )

  # Run the function
  bootstrap_type <- choose_bootstrap_type(data)

  # Check the output is one of the expected bootstrap types
  expect_true(bootstrap_type %in% c("residual", "wild", "pairwise", "winsorized"))
})

test_that("choose_bootstrap_type handles heteroscedasticity", {
  set.seed(123)
  data <- data.frame(
    Y = c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 0, sd = 5)),  # Different variances
    A = factor(rep(1:2, 50)),
    B = factor(rep(1:2, each = 50))
  )

  expect_true(choose_bootstrap_type(data) %in% c("wild", "winsorized"))
})

test_that("choose_bootstrap_type detects normal residuals", {
  set.seed(123)
  data <- data.frame(
    Y = rnorm(100),  # Normally distributed residuals
    A = factor(rep(1:2, 50)),
    B = factor(rep(1:2, each = 50))
  )

  expect_equal(choose_bootstrap_type(data), "residual")
})

test_that("choose_bootstrap_type detects non-normal residuals", {
  set.seed(123)
  data <- data.frame(
    Y = rt(100, df = 3),  # Heavy-tailed distribution (t-distribution with df=3)
    A = factor(rep(1:2, 50)),
    B = factor(rep(1:2, each = 50))
  )

  expect_equal(choose_bootstrap_type(data), "pairwise")
})

test_that("choose_bootstrap_type handles large data without Shapiro-Wilk test", {
  set.seed(123)
  data <- data.frame(
    Y = rnorm(5000),
    A = factor(rep(1:2, length.out = 5000)),
    B = factor(rep(1:2, length.out = 5000))
  )

  expect_true(choose_bootstrap_type(data) %in% c("residual", "pairwise"))
})

