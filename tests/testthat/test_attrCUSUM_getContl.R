#library(attrCUSUM)
#library(testthat)

context("Tests related to getContl")

# NULL, NA, TRUE/FALSE, NaN, Inf/-Inf, Int, Real, Complex, Character, List, Expression
test_that("Arguments checking : getContl", {
  # Check : anss.target (positive numeric value in (1,50000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl(anss.target = NULL, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = NA, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = TRUE, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = NaN, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = Inf, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getContl(anss.target = 1, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = .Machine$double.xmax, refv = 5,
                        c.zero = 0, process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200 + 1i, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = "200", refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = list(200), refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = expression(200), refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))

  # Check : refv (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl(anss.target = 200, refv = NULL, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = NA, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = TRUE, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = NaN, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = Inf, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getContl(anss.target = 200, refv = 0, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = -1.5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200, refv = 5 + 1i, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = "5", c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = list(5), c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = expression(5), c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))

  # Check : c.zero (positive numeric value in [-'refv', 'contl'))
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = NULL,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = NA,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = TRUE,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = NaN,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = Inf,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # c.zero < -'refv'
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = -5.1,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0 + 1i,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = "0",
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = list(0),
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = expression(0),
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))

  # Check : process (function)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf, Numeric, Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = NULL,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = NA,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = TRUE,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = NaN,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = Inf,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = 10.1,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = 1L,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = 1 + 1i,
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = "1",
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = list(1),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = expression(1),
                        maxndec = 7L, maxnumsubI = 1000L))

  # Invalid pmf
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) -dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) 100 * dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) ppois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) rpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L))

  # Check : maxndec (non-negative integer value <= 7L)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = NULL, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = NA, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = TRUE, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = NaN, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = Inf, maxnumsubI = 1000L))

  # Invalid integer value
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = -1L, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 8L, maxnumsubI = 1000L))
  # Real, Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 5.5, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 5L + 1i, maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = "7L", maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = list(7L), maxnumsubI = 1000L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = expression(7L), maxnumsubI = 1000L))

  # Check : maxnumsubI (positive integer value in [100,6000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = NULL))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = NA))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = TRUE))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = NaN))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = Inf))

  # Invalid integer value
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 99L))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 10000L))
  # Real, Complex, Character, List, Expression
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 5000.1))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = 1000L + i))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = "1000L"))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = list(1000L)))
  expect_error(getContl(anss.target = 200, refv = 5, c.zero = 0,
                        process = function(x) dpois(x, lambda = 4),
                        maxndec = 7L, maxnumsubI = expression(1000L)))
})

test_that("Results : getContl", {
  # Test for results
  anss.target <- 200L
  refv <- 5
  c.zero <- 0
  process <- function(x) dpois(x, lambda = 4)
  maxndec <- 1L
  maxnumsubI <- 500L
  res <- getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                  process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
  expect_equal(res$refv.act, 5)
  expect_equal(res$c.zero.act, 0)
  expect_equal(unname(round(res$sol1, 2L)), c(8, 171.78))
  expect_equal(unname(round(res$sol2, 2L)), c(9, 270.01))

  anss.target <- 100L
  refv <- 5 + 1/3; c.zero <- 1 + 1/3;
  process <- function(x) dpois(x, lambda = 4.1);
  maxndec <- 1L; maxnumsubI <- 1000L;
  res <- getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                  process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
  expect_equal(round(res$refv.act, 1L), 5.3)
  expect_equal(round(res$c.zero.act, 1L), 1.3)
  expect_equal(unname(round(res$sol1, 2L)), c(5.8, 97.9))
  expect_equal(unname(round(res$sol2, 2L)), c(5.9, 105.47))
})

