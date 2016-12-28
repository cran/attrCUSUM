#library(attrCUSUM)
#library(testthat)

context("Tests related to getContl_pois")

# NULL, NA, TRUE/FALSE, NaN, Inf/-Inf, Int, Real, Complex, Character, List, Expression
test_that("Arguments checking : getContl_pois", {
  # Check : lambda (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = NULL, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = NA, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = TRUE, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = NaN, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = Inf, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getContl_pois(lambda = 0, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = -1, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 1 + 1i, anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = "1", anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = list(1), anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = expression(1), anss.target = 200, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))

  # Check : anss.target (positive numeric value in (1,50000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = 4, anss.target = NULL, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = NA, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = TRUE, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = NaN, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = Inf, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getContl_pois(lambda = 4, anss.target = 1, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = .Machine$double.xmax,
                             refv = 5, c.zero = 0, maxndec = 7L,
                             maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 4, anss.target = 200 + 1i, refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = "200", refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = list(200), refv = 5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = expression(200),
                             refv = 5, c.zero = 0, maxndec = 7L,
                             maxnumsubI = 1000L))

  # Check : refv (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = NULL,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = NA,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = TRUE,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = NaN,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = Inf,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 0,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = -1.5,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5 + 1i,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = "5",
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = list(5),
                             c.zero = 0, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200,
                             refv = expression(5), c.zero = 0, maxndec = 7L,
                             maxnumsubI = 1000L))

  # Check : c.zero (positive numeric value in [-'refv', 'contl'))
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = NULL, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = NA, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = TRUE, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = NaN, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = Inf, maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # c.zero < -'refv'
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = -5.1, maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = 0 + 1i, maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = "0", maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = list(0), maxndec = 7L,
                             maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5,
                             c.zero = expression(0), maxndec = 7L,
                             maxnumsubI = 1000L))

  # Check : maxndec (non-negative integer value <= 7L)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = NULL, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = NA, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = TRUE, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = NaN, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = Inf, maxnumsubI = 1000L))

  # Invalid integer value
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = -1L, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 8L, maxnumsubI = 1000L))
  # Real, Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 5.5, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 5L + 1i, maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = "7L", maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = list(7L), maxnumsubI = 1000L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = expression(7L), maxnumsubI = 1000L))

  # Check : maxnumsubI (positive integer value in [100,6000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = NULL))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = NA))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = TRUE))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = NaN))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = Inf))

  # Invalid integer value
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = 99L))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = 10000L))
  # Real, Complex, Character, List, Expression
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = 5000.1))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = 1000L + i))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = "1000L"))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = list(1000L)))
  expect_error(getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0,
                             maxndec = 7L, maxnumsubI = expression(1000L)))
})

test_that("Results : getContl_pois", {
  # Test for results
  lambda <- 4
  anss.target <- 200L
  refv <- 5
  c.zero <- 0
  maxndec <- 7L
  maxnumsubI <- 1000L
  res <- getContl_pois(lambda = lambda, anss.target = anss.target, refv = refv,
                       c.zero = c.zero, maxndec = maxndec,
                       maxnumsubI = maxnumsubI)
  expect_equal(res$refv.act, 5)
  expect_equal(res$c.zero.act, 0)
  expect_equal(unname(round(res$sol1, 2L)), c(8, 171.78))
  expect_equal(unname(round(res$sol2, 2L)), c(9, 270.01))

  lambda <- 4.1
  anss.target <- 100L
  refv <- 5 + (1 / 3)
  c.zero <- 1 + (1 / 3)
  maxndec <- 1L
  maxnumsubI <- 1000L
  res <- getContl_pois(lambda = lambda, anss.target = anss.target,
                       refv = refv, c.zero = c.zero,
                       maxndec = maxndec, maxnumsubI = maxnumsubI)
  expect_equal(round(res$refv.act, 2L), 5.3)
  expect_equal(round(res$c.zero.act, 2L), 1.3)
  expect_equal(unname(round(res$sol1, 2L)), c(5.8, 97.90))
  expect_equal(unname(round(res$sol2, 2L)), c(5.9, 105.47))
})
