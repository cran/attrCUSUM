#library(attrCUSUM)
#library(testthat)

context("Tests related to getAve")

test_that("Arguments checking : getAve", {
  # Check : refv (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = NULL, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = NA, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = TRUE, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = NaN, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = Inf, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve(refv = 0, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = -1.5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5 + 1i, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = "5", contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = list(5), contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = expression(5), contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : contl (positive numeric value)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = NULL, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = NA, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = TRUE, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = NaN, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = Inf, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve(refv = 5, contl = 0, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = -1.5, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8 + 1i, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = "8", c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = list(8), c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = expression(8), c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : c.zero (positive numeric value >= -'refv')
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = NULL, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = NA, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = TRUE, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = NaN, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = Inf, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # c.zero < -'refv'
  expect_error(getAve(refv = 5, contl = 8, c.zero = -5.1, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = "0", warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0 + 1i, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = list(0), warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = expression(0), warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : warnl (positive numeric value in [-'refv', 'contl'])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = NULL,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = NA,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = TRUE,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = NaN,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = Inf,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # warnl < -'refv'
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = -5.1,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # warnl > 'contl'
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 8.1,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0 + 1i,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = "0",
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = list(0),
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = expression(0),
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : ds (positive numeric value in (0, 1])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = NULL, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = NA, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = TRUE, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = NaN, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = Inf, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 1.05, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1 + 1i, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = "0.1", dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = list(0.1), dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = expression(0.1), dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : dl (NULL or numeric value >= 'ds')
  # NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NA, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = TRUE, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NaN, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = Inf, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  # 'dl' < 'ds'
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = 0.1 / 2, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = 1 + 1i, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = "NULL", di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = list(1), di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = expression(1), di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : di (NULL or positive numeric value)
  # NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NA,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = TRUE,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NaN,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = Inf,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid numeric value
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = 0,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = -1,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = 1 + 1i,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = "1",
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = list(1),
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = expression(1),
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : process (function)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf, Numeric, Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = NULL,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = NA,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = TRUE,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = NaN,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = Inf,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = c(0.2, 0.8),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = 0.5 + 1i,
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = "0.5",
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = list(0.5),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = expression(0.5),
                      maxndec = 7L, maxnumsubI = 1000L))
  # Invalid pmf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) -dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) 100 * dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) ppois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) rpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 1000L))

  # Check : maxndec (non-negative integer value <= 7L)
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = NULL, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = NA, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = TRUE, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = NaN, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = Inf, maxnumsubI = 1000L))
  # Invalid integer value
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = -1L, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 8L, maxnumsubI = 1000L))
  # Real, Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 5.5, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 5L + 1i, maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = "5L", maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = list(5L), maxnumsubI = 1000L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = expression(5L), maxnumsubI = 1000L))

  # Check : maxnumsubI (positive integer value in [100,6000])
  # NULL, NA, TRUE/FALSE, NaN, Inf/-Inf
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = NULL))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = NA))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = TRUE))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = NaN))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = Inf))
  # Invalid integer value
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 99L))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 10000L))
  # Real, Complex, Character, List, Expression
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 500.1))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = 500L + 1i))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = "500L"))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = list(500L)))
  expect_error(getAve(refv = 5, contl = 8, c.zero = 0, warnl = 0,
                      ds = 0.1, dl = NULL, di = NULL,
                      process = function(x) dpois(x, lambda = 4),
                      maxndec = 7L, maxnumsubI = expression(500L)))
})

test_that("Results : getAve", {
  #Test for results
  refv <- 5
  contl <- 8
  c.zero <- 0
  warnl <- 1
  ds <- 0.1
  process <- function(x) dpois(x, lambda = 4)
  maxndec <- 7L
  maxnumsubI <- 1000
  res <- getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                ds = ds, process = process,
                maxndec = maxndec, maxnumsubI = maxnumsubI)
  expect_equal(round(res$ANSS, 2L), 171.78)
  expect_equal(round(res$ATS, 4L), round(res$ANSS, 4L))
  expect_equal(round(res$dl, 4L), 1.4851)
  expect_equal(res$numsubI, 13)
  expect_equal(res$lensubI, 1)
  expect_equal(res$endpoints, seq(-refv, contl))
  expect_true(all(res$Q >= 0, res$Q <= 1, rowSums(res$Q) <= 1))
  expect_equal(res$I_minus_Q, diag(nrow(res$Q)) - res$Q)
  expect_equal(as.vector(res$I_minus_Q_inv_1), unname(rowSums(res$I_minus_Q_inv)))
  expect_true(all(res$initpr >= 0, res$initpr <= 1, sum(res$initpr) <= 1))
  expect_true(all(res$ds <= res$dl, res$ds <= res$di, res$di <= res$dl))
  expect_equal(res$psi.s + res$psi.l + 1,res$ANSS)
  expect_equal(res$psi.s * res$ds + res$psi.l * res$dl + res$di, res$ATS)
  expect_equal(res$refv.act, refv)
  expect_equal(res$contl.act, contl)
  expect_equal(res$c.zero.act, c.zero)
  expect_equal(res$warnl.act, warnl)

  dl <- res$dl
  process <- function(x) dpois(x, lambda = 4 + 2 * 0.1);
  res <- getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                ds = ds, dl = dl, process = process, maxndec = maxndec,
                maxnumsubI = maxnumsubI)
  expect_equal(round(res$ANSS, 1L), 99.3)
  expect_equal(round(res$ATS, 1L), 91.0)
  expect_equal(res$numsubI, 13)
  expect_equal(res$psi.s + res$psi.l + 1,res$ANSS)
  expect_equal(res$psi.s * res$ds + res$psi.l * res$dl + res$di, res$ATS)

  refv <- 5.2
  contl <- 8.1
  c.zero <- 0
  warnl <- 1
  ds <- 0.1
  process <- function(x) dpois(x, lambda = 4)
  maxndec <- 7L
  maxnumsubI <- 1000L
  res <- getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                ds = ds, process = process,
                maxndec = maxndec, maxnumsubI = maxnumsubI)
  expect_equal(round(res$ANSS, 2L), 350.73)
  expect_equal(round(res$ATS, 4L), round(res$ANSS, 4L))
  expect_equal(round(res$dl, 4L), 1.269)
  expect_equal(res$numsubI, 133)
  expect_equal(res$lensubI, 0.1)
  expect_equal(res$endpoints, seq(-refv, contl, 0.1))
  expect_true(all(res$Q >= 0, res$Q <= 1, rowSums(res$Q) <= 1))
  expect_equal(res$I_minus_Q, diag(nrow(res$Q)) - res$Q)
  expect_equal(as.vector(res$I_minus_Q_inv_1), unname(rowSums(res$I_minus_Q_inv)))
  expect_true(all(res$initpr >= 0, res$initpr <= 1, sum(res$initpr) <= 1))
  expect_true(all(res$ds <= res$dl, res$ds <= res$di, res$di <= res$dl))
  expect_equal(res$psi.s + res$psi.l + 1,res$ANSS)
  expect_equal(res$psi.s * res$ds + res$psi.l * res$dl + res$di, res$ATS)
  expect_equal(res$refv.act, refv, tolerance = 0.1)
  expect_equal(res$contl.act, contl, tolerance = 0.1)
  expect_equal(res$c.zero.act, c.zero, tolerance = 0.1)
  expect_equal(res$warnl.act, warnl, tolerance = 0.1)
})
#
