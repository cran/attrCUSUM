#library(HanwoolMisc)
#library(testthat)

# Invalid argument gives an error

# Check : n (positive integer value)
# NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
# Real, Complex, Character, List, Expression

context("Tests related to xxxzipois")

test_that("Arguments checking : dzipois", {
  # Invalid argument gives an error

  # Check : x (non-negative integer value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzipois(x = NULL, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = NA, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = TRUE, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = NaN, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = Inf, rho = 0.5, lambda = 5, log = FALSE))
  # Invalid integer value
  expect_error(dzipois(x = -5L, rho = 0.5, lambda = 5, log = FALSE))
  # Real, Complex, Character, List, Expression
  expect_error(dzipois(x = 5.1, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5 + 1i, rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = "5", rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = list(5L), rho = 0.5, lambda = 5, log = FALSE))
  expect_error(dzipois(x = expression(5L), rho = 0.5, lambda = 5, log = FALSE))

  # Check : rho (on [0,1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzipois(x = 5L, rho = NULL, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = NA, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = TRUE, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = NaN, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = Inf, lambda = 5, log = FALSE))
  # Invalid numeric values
  expect_error(dzipois(x = 5L, rho = 5L, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = -0.0001, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 1.0001, lambda = 5, log = FALSE))
  # Complex, Character, List, Expression
  expect_error(dzipois(x = 5L, rho = 0.5 + 1i, lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = "0.5", lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = list(0.5), lambda = 5, log = FALSE))
  expect_error(dzipois(x = 5L, rho = expression(0.5), lambda = 5, log = FALSE))

  # Check : lambda (>0)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = NULL, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = NA, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = TRUE, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = NaN, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = Inf, log = FALSE))
  # Invalid numeric values
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = -1L, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 0, log = FALSE))
  # Complex, Character, List, Expression
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5 + 1i, log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = "5", log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = list(5), log = FALSE))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = expression(5), log = FALSE))

  # Check : log (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = NULL))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = NA))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = NaN))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = 1L))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = 0))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = 1 + 1i))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = "TRUE"))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = list(TRUE)))
  expect_error(dzipois(x = 5L, rho = 0.5, lambda = 5, log = expression(TRUE)))
})

test_that("Results : dzipois", {
  # Agruments setting
  x <- seq(0, 10)
  rho <- 0
  lambda <- 5

  expect_equal(dzipois(x, rho, lambda), stats::dpois(x, lambda))
  expect_equal(dzipois(x, rho, lambda, TRUE), stats::dpois(x, lambda, TRUE))

  # Agruments setting
  x <- seq(1, 10)
  rho <- 0.5
  lambda <- 10
  expect_equal(dzipois(x, rho, lambda), (1 - rho) * stats::dpois(x, lambda))
  expect_equal(dzipois(x, rho, lambda, TRUE), log((1 - rho) * stats::dpois(x, lambda)))
  expect_equal(dzipois(0, rho, lambda, TRUE),
               log(rho + (1 - rho) * stats::dpois(0, lambda)))
})
##

test_that("Arguments checking : pzipois", {
  # Invalid argument gives an error

  # Check : q (numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(pzipois(q = NULL, rho = 0.2, lambda = 5))
  expect_error(pzipois(q = NA, rho = 0.2, lambda = 5))
  expect_error(pzipois(q = TRUE, rho = 0.2, lambda = 5))
  expect_error(pzipois(q = NaN, rho = 0.2, lambda = 5))
  expect_error(pzipois(q = Inf, rho = 0.2, lambda = 5))
  # Complex, Character, List, Expression
  expect_error(pzipois(q = 1 + 1i, rho = 0.2, lambda = 5))
  expect_error(pzipois(q = "1", rho = 0.2, lambda = 5))
  expect_error(pzipois(q = list(1), rho = 0.2, lambda = 5))
  expect_error(pzipois(q = expression(1), rho = 0.2, lambda = 5))

  # Check : rho (on [0,1])
  # Already done at tests for dzipois

  # Check : lambda (>0)
  # Already done at tests for dzipois

  # Check : lower.tail (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = NULL))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = NA))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = NaN))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = 1L))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = 1.5))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = 1 + 1i))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = "TRUE"))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, lower.tail = list(TRUE)))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5,
                       lower.tail = expression(TRUE)))

  # Check : log.p (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = NULL))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = NA))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = NaN))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = 1L))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = 1.5))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = 1 + 1i))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = "TRUE"))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5, log.p = list(TRUE)))
  expect_error(pzipois(q = 1, rho = 0.2, lambda = 5,
                       log.p = expression(TRUE)))
})

test_that("Results : pzipois", {
  # Agruments setting
  x <- seq(0, 10)
  rho <- 0
  lambda <- 5
  expect_equal(pzipois(x, rho, lambda), stats::ppois(x, lambda))
  expect_equal(pzipois(x, rho, lambda, FALSE), stats::ppois(x, lambda, FALSE))
  expect_equal(pzipois(x, rho, lambda, FALSE, TRUE),
               stats::ppois(x, lambda, FALSE, TRUE))

  # Agruments setting
  x <- seq(0, 10)
  rho <- 0.5
  lambda <- 10
  expect_equal(pzipois(x, rho, lambda), rho + (1 - rho) * stats::ppois(x, lambda))
})

test_that("Arguments checking : qzipois", {
  # Invalid argument gives an error

  # Check : p (on [0,1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(qzipois(p = NULL, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = NA, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = TRUE, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = NaN, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = Inf, rho = 0.1, lambda = 1))
  # Invalid numeric values
  expect_error(qzipois(p = -0.1, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = 1.1, rho = 0.1, lambda = 1))
  # Complex, Character, List, Expression
  expect_error(qzipois(p = 1 + 1i, rho = 0.1, lambda = 1))
  expect_error(qzipois(p = "0.5", rho = 0.1, lambda = 1))
  expect_error(qzipois(p = list(0.5), rho = 0.1, lambda = 1))
  expect_error(qzipois(p = expression(0.5), rho = 0.1, lambda = 1))

  # Check : rho (on [0,1])
  # Already done at tests for dzipois

  # Check : lambda (> 0)
  # Already done at tests for dzipois

  # Check : lower.tail (TRUE/FALSE)
  # Already done at tests for pzipois

  # Check : log.p (TRUE/FALSE)
  # Already done at tests for pzipois
})

test_that("Results : qzipois", {
  # Agruments setting
  p <- seq(0.0, 1, len = 7)
  rho <- 0
  lambda <- 5
  expect_equal(qzipois(p, rho, lambda), stats::qpois(p, lambda))
  expect_equal(qzipois(p, rho, lambda, FALSE), stats::qpois(p, lambda, FALSE))
  expect_equal(qzipois(log(p), rho, lambda, FALSE, TRUE),
               stats::qpois(log(p), lambda, FALSE, TRUE))

  # Agruments setting
  rho <- 0.5
  lambda <- 5
  p <- seq(0.0, rho - 1e-8, len = 7)
  expect_equal(qzipois(p, rho, lambda), numeric(7))
})

test_that("Arguments checking : rzipois", {
  # Invalid argument gives an error

  # Check : n (positive integer value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(rzipois(n = NULL, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = NA, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = TRUE, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = NaN, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = Inf, rho = 0.5, lambda = 5))
  # Invalid integer value
  expect_error(rzipois(n = 0L, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = -1L, rho = 0.5, lambda = 5))
  # Real, Complex, Character, List, Expression
  expect_error(rzipois(n = 10.5, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = 10 + 1i, rho = 0.5, lambda = 5))
  expect_error(rzipois(n = "10L", rho = 0.5, lambda = 5))
  expect_error(rzipois(n = list(10L), rho = 0.5, lambda = 5))
  expect_error(rzipois(expression(10L), 0.5, 5))

  # Check : rho (on [0,1])
  # Already done at tests for dzipois

  # Check : lambda (> 0)
  # Already done at tests for dzipois
})

test_that("Results : rzipois", {
  # Agruments setting
  n <- 1e+4
  rho <- 0.2
  lambda <- 5

  x <- rzipois(n, rho, lambda); y <- rzipois(n, rho, lambda)
  expect_equal(mean(x), lambda * (1 - rho), tolerance = 0.1) # mean
  expect_lt(stats::cor(x, y), 0.1) # corr

  set.seed(1L)
  x <- rzipois(n, rho, lambda)
  set.seed(1L)
  y <- rzipois(n, rho, lambda)
  expect_equal(x, y) # The same seeds must give the same results
})
