#library(attrCUSUM)
#library(testthat)

context("Tests related to xxxzibinom")

test_that("Arguments checking : dzibinom", {
  # Invalid argument gives an error

  # Check : x (non-negative integer value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzibinom(x = NULL, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = NA, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = TRUE, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = NaN, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = Inf, rho = 0.5, size = 5, prob = 0.5))
  # Invalid integer value
  expect_error(dzibinom(x = -5L, rho = 0.5, size = 5, prob = 0.5))
  # Real, Complex, Character, List, Expression
  expect_error(dzibinom(x = 5.1, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5 + 1i, rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = "5", rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = list(5L), rho = 0.5, size = 5, prob = 0.5))
  expect_error(dzibinom(x = expression(5L), rho = 0.5, size = 5, prob = 0.5))

  # Check : rho (on [0,1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzibinom(x = 5L, rho = NULL, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = NA, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = TRUE, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = NaN, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = Inf, size = 5, prob = 0.5))
  # Invalid numeric values
  expect_error(dzibinom(x = 5L, rho = 1.2, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = -0.0001, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 1.0001, size = 5, prob = 0.5))
  # Complex, Character, List, Expression
  expect_error(dzibinom(x = 5L, rho = 0.5 + 1i, size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = "0.5", size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = list(0.5), size = 5, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = expression(0.5), size = 5, prob = 0.5))

  # Check : size (>0)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(dzibinom(x = 5L, rho = 0.5, size = NULL, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = NA, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = TRUE, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = NaN, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = Inf, prob = 0.5))
  # Invalid numeric values
  expect_error(dzibinom(x = 5L, rho = 0.5, size = -1, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5.5, prob = 0.5))
  # Complex, Character, List, Expression
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5 + 1i, prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = "5", prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = list(5), prob = 0.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = expression(5), prob = 0.5))

  # Check : log (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = NULL))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = NA))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = NaN))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = 1L))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = 1.5))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = 1 + 1i))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = "TRUE"))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = list(TRUE)))
  expect_error(dzibinom(x = 5L, rho = 0.5, size = 5, prob = 0.5, log = expression(TRUE)))
})
##

test_that("Results : dzibinom", {
  x <- seq(0, 10)
  rho <- 0
  size <- 5
  prob <- 0.2  # Agruments setting
  expect_equal(dzibinom(x, rho, size, prob), dbinom(x, size, prob))
  expect_equal(dzibinom(x, rho, size, prob, TRUE), dbinom(x, size, prob, TRUE))

  x <- seq(1, 10)
  rho <- 0.5
  size <- 10
  prob <- 0.1  # Agruments setting
  expect_equal(dzibinom(x, rho, size, prob),
               (1 - rho) * dbinom(x, size, prob))
  expect_equal(dzibinom(x, rho, size, prob, TRUE),
               log((1 - rho) * dbinom(x, size, prob)))
  expect_equal(dzibinom(0, rho, size, prob, TRUE),
               log(rho + (1 - rho) * dbinom(0, size, prob)))
})
##

test_that("Arguments checking : pzibinom", {
  # Invalid argument gives an error

  # Check : q (numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(pzibinom(q = NULL, rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = NA, rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = TRUE, rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = NaN, rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = Inf, rho = 0.2, size = 5, prob = 0.5))

  # Complex, Character, List, Expression
  expect_error(pzibinom(q = 1 + 1i, rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = "1", rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = list(1), rho = 0.2, size = 5, prob = 0.5))
  expect_error(pzibinom(q = expression(1), rho = 0.2, size = 5, prob = 0.5))

  # Check : rho (on [0,1])
  # Already done at tests for dzibinom

  # Check : size (>0)
  # Already done at tests for dzibinom

  # Check : lower.tail (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = NULL))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = NA))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = NaN))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = 1L))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = 1.5))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = 1 + 1i))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = "TRUE"))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = list(TRUE)))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        lower.tail = expression(TRUE)))

  # Check : log.p (TRUE or FALSE)
  # NULL, NA, NaN, Inf/-Inf
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = NULL))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = NA))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = NaN))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = Inf))
  # Integer, Real, Complex, Character, List, Expression
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = 1L))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = 1.5))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = 1 + 1i))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5, log.p = "TRUE"))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        log.p = list(TRUE)))
  expect_error(pzibinom(q = 1, rho = 0.2, size = 5, prob = 0.5,
                        log.p = expression(TRUE)))
})

test_that("Results : pzibinom", {
  x <- seq(0, 10)
  rho <- 0
  size <- 5
  prob <- 0.5  # Agruments setting
  expect_equal(pzibinom(x, rho, size, prob), pbinom(x, size, prob))
  expect_equal(pzibinom(x, rho, size, prob, FALSE), pbinom(x, size, prob, FALSE))
  expect_equal(pzibinom(x, rho, size, prob, FALSE, TRUE),
               stats::pbinom(x, size, prob, FALSE, TRUE))

  x <- seq(0, 10)
  rho <- 0.5
  size <- 10
  prob <- 0.2  # Agruments setting
  expect_equal(pzibinom(x, rho, size, prob),
               rho + (1 - rho) * stats::pbinom(x, size, prob))
})
##

test_that("Arguments checking : qzibinom", {
  # Invalid argument gives an error

  # Check : p (on [0,1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(qzibinom(p = NULL, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = NA, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = TRUE, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = NaN, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = Inf, rho = 0.1, size = 1, prob = 0.5))
  # Invalid numeric values
  expect_error(qzibinom(p = -0.1, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = 1.1, rho = 0.1, size = 1, prob = 0.5))
  # Complex, Character, List, Expression
  expect_error(qzibinom(p = 1 + 1i, rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = "0.5", rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = list(0.5), rho = 0.1, size = 1, prob = 0.5))
  expect_error(qzibinom(p = expression(0.5), rho = 0.1, size = 1, prob = 0.5))

  # Check : rho (on [0,1])
  # Already done at tests for dzibinom

  # Check : size (> 0)
  # Already done at tests for dzibinom

  # Check : lower.tail (TRUE/FALSE)
  # Already done at tests for pzibinom

  # Check : log.p (TRUE/FALSE)
  # Already done at tests for pzibinom
})

test_that("Results : qzibinom", {
  # Agruments setting
  p <- seq(0.0, 1, len = 7)
  rho <- 0
  size <- 5
  prob <- 0.2
  expect_equal(qzibinom(p, rho, size, prob), stats::qbinom(p, size, prob))
  expect_equal(qzibinom(p, rho, size, prob, FALSE), stats::qbinom(p, size, prob, FALSE))
  expect_equal(qzibinom(log(p), rho, size, prob, FALSE, TRUE),
               stats::qbinom(log(p), size, prob, FALSE, TRUE))

  # Agruments setting
  rho <- 0.5
  size <- 5
  p <- seq(0.0, rho - 1e-8, len = 7)
  expect_equal(qzibinom(p, rho, size, prob), numeric(7))
})


test_that("Arguments checking : rzibinom", {
  # Invalid argument gives an error

  # Check : n (positive integer value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(rzibinom(n = NULL, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = NA, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = TRUE, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = NaN, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = Inf, rho = 0.5, size = 5, prob = 0.2))

  # Invalid integer value
  expect_error(rzibinom(n = 0L, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = -1L, rho = 0.5, size = 5, prob = 0.2))
  # Real, Complex, Character, List, Expression
  expect_error(rzibinom(n = 10.5, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = 10 + 1i, rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = "10L", rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = list(10), rho = 0.5, size = 5, prob = 0.2))
  expect_error(rzibinom(n = expression(10), rho = 0.5, size = 5, prob = 0.2))

  # Check : rho (on [0,1])
  # Already done at tests for dzibinom

  # Check : size (> 0)
  # Already done at tests for dzibinom
})

test_that("Results : rzibinom", {
  # Agruments setting
  n <- 1e+5
  rho <- 0.2
  size <- 5
  prob <- 0.2

  x <- rzibinom(n, rho, size, prob); y <- rzibinom(n, rho, size, prob)
  expect_equal(mean(x), (1 - rho) * size * prob, tolerance = 0.1) # mean
  expect_lt(cor(x, y), 0.1) # corr

  set.seed(1L)
  x <- rzibinom(n, rho, size, prob)
  set.seed(1L)
  y <- rzibinom(n, rho, size, prob)
  expect_equal(x, y) # The same seeds must give the same results
})

