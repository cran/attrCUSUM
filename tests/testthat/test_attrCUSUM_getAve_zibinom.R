#library(attrCUSUM)
#library(testthat)

context("Tests related to getAve_zibinom")

test_that("Arguments checking : getAve_zibinom", {
  # Check : rho (numeric value in [0, 1))
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = NULL, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = NA, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = TRUE, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = NaN, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = Inf, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = -0.1, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 1, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5 + 1i, size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = "0.5", size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = list(0.5), size = 10, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = expression(0.5), size = 10, prob = 0.5,
                              refv = 5, contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : size (positive integer value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = NULL, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = NA, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = TRUE, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = NaN, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = Inf, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 0, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10.5, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10 + 1i, prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = "10", prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = list(10), prob = 0.5, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = expression(10), prob = 0.5,
                              refv = 5, contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : prob (numeric value in (0,1))
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = NULL, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = NA, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = TRUE, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = NaN, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = Inf, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 0, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 1, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 1 + 1i, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = "1", refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = list(1), refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = expression(1),
                              refv = 5, contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : refv (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = NULL,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = NA,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = TRUE,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = NaN,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = Inf,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 0,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = -1.5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5 + 1i,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = "5",
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = list(5),
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4,
                              refv = expression(5), contl = 8, c.zero = 0,
                              warnl = 0, ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : contl (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = NULL, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = NA, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = TRUE, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = NaN, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = Inf, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 0, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = -1.5, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8 + 1i, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = "8", c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = list(8), c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = expression(8), c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : c.zero (positive numeric value >= -'refv')
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = NULL, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = NA, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = TRUE, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = NaN, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = Inf, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # c.zero < -'refv'
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = -5.1, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = "0", warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0 + 1i, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = list(0), warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = expression(0), warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : warnl (positive numeric value in [-'refv', 'contl'])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = NULL,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = NA,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = TRUE,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = NaN,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = Inf,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # warnl < -'refv'
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = -5.1,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # warnl > 'contl'
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 8.1,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0 + 1i,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = "0",
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = list(0),
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = expression(0),
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : ds (positive numeric value in (0, 1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = NULL, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = NA, dl = NULL, di = NULL,

                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = TRUE, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = NaN, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = Inf, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 1.05, dl = NULL, di = NULL,

                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1 + 1i, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = "0.1", dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = list(0.1), dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = expression(0.1), dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : dl (NULL or numeric value >= 'ds')
  # NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NA, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = TRUE, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NaN, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = Inf, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # 'dl' < 'ds'
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = 0.1 / 2, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = 1 + 1i, di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = "NULL", di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = list(1), di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = expression(1), di = NULL,
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : di (NULL or positive numeric value)
  # NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NA,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = TRUE,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NaN,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = Inf,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = 0,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = -1,
                              maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = 1 + 1i,
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = "1",
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = list(1),
                              maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = expression(1),
                              maxndec = 7L, maxnumsubI = 1000L))

  # Check : maxndec (non-negative integer value <= 7L)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = NULL, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = NA, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = TRUE, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = NaN, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = Inf, maxnumsubI = 1000L))
  # Invalid integer value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = -1L, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 8L, maxnumsubI = 1000L))
  # Real, Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 5.5, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 5L + 1i, maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = "5L", maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = list(5L), maxnumsubI = 1000L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = expression(5L), maxnumsubI = 1000L))

  # Check : maxnumsubI (positive integer value in [100,6000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = NULL))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = NA))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = TRUE))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = NaN))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = Inf))
  # Invalid integer value
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 99L))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 10000L))
  # Real, Complex, Character, List, Expression
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 500.1))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = 500L + 1i))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = "500L"))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = list(500L)))
  expect_error(getAve_zibinom(rho = 0.5, size = 10, prob = 4, refv = 5,
                              contl = 8, c.zero = 0, warnl = 0,
                              ds = 0.1, dl = NULL, di = NULL,
                              maxndec = 7L, maxnumsubI = expression(500L)))
})

test_that("Results : getAve_zibinom", {
  # Test for results
  rho <- 0.1
  size <- 10
  prob <- 0.5
  refv <- 5
  contl <- 12
  c.zero <- 0
  warnl <- 1
  ds <- 0.1
  dl <- di <- NULL
  maxndec <- 7L
  maxnumsubI <- 1000L
  res <- getAve_zibinom(rho = rho, size = size, prob = prob, refv = refv,
                        contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
                        dl = dl, di = di, maxndec = maxndec,
                        maxnumsubI = maxnumsubI)
  expect_equal(round(res$ANSS, 2L), 216.75)
  expect_equal(round(res$ATS, 4L), round(res$ANSS, 4L))
  expect_equal(round(res$dl, 4L), 2.4915)
  expect_equal(res$numsubI, 17)
  expect_equal(res$lensubI, 1)
  expect_equal(res$endpoints, seq(-refv, contl))
  expect_true(all(res$Q >= 0, res$Q <= 1, round(rowSums(res$Q), 15L) <=  1))
  expect_equal(res$I_minus_Q, diag(nrow(res$Q)) - res$Q)
  expect_equal(as.vector(res$I_minus_Q_inv_1),
               unname(rowSums(res$I_minus_Q_inv)))
  expect_true(all(res$initpr >= 0, res$initpr <= 1,
                  round(sum(res$initpr), 15L) <= 1))
  expect_true(all(res$ds <= res$dl, res$ds <= res$di, res$di <= res$dl))
  expect_equal(res$psi.s + res$psi.l + 1,res$ANSS)
  expect_equal(res$psi.s * res$ds + res$psi.l * res$dl + res$di, res$ATS)
  expect_equal(res$refv.act, refv)
  expect_equal(res$contl.act, contl)
  expect_equal(res$c.zero.act, c.zero)
  expect_equal(res$warnl.act, warnl)

  dl <- res$dl
  prob <- 0.5 * 1.5
  res <- getAve_zibinom(rho = rho, size = size, prob = prob, refv = refv,
                        contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
                        dl = dl, di = di, maxndec = maxndec,
                        maxnumsubI = maxnumsubI)
  expect_equal(round(res$ANSS, 1L), 6.8)
  expect_equal(round(res$ATS, 1L), 4.3)
  expect_equal(res$numsubI, 17)
  expect_equal(res$psi.s + res$psi.l + 1,res$ANSS)
  expect_equal(res$psi.s * res$ds + res$psi.l * res$dl + res$di, res$ATS)
})
