
#' @name zibinom
#' @title The Zero Inflated Binomial Distribution
#' @description Density, distribution function, quantile function and
#'    random generation for the zero inflated binomial distribution
#'    with parameters (rho, size, prob).
#' @details The probability mass function of X is given by
#'    \deqn{P(X=x) = rho I(x = 0) + (1 - rho) P(Y=x), x=0,1,2,...,}
#'    where Y is distributed binomial(size, prob).
#' @param x A non-negative integer-valued vector of quantiles.
#' @param q A numeric vector of quantiles.
#' @param p A vector of probabilities.
#' @param n Number of random values to return,
#'    a length-one positive integer-valued vector.
#' @param rho A length-one vector of zero inflation parameter on [0,1].
#' @param size A length-one vector of number of trials (zero or more).
#' @param prob A length-one vector of probability of success on each trial.
#' @param log,log.p A length-one logical vector;
#'    if TRUE, probabilities p are given as log(p).
#' @param lower.tail A length-one logical vector;
#'    if TRUE (the default), probabilities are \eqn{P(X \le x)},
#'    otherwise, \eqn{P(X > x).}
#' @return \code{dzibinom} gives the (log) density,
#'    \code{pzibinom} gives the (log) distribution function,
#'    \code{qzibinom} gives the quantile function,
#'    and \code{rzibinom} generates random deviates.
#'
#'    Invalid arguments rise an error.
#' @seealso \link{Binomial} for the binomial distribution.
#' @examples
#' # Example 1: dzibinom
#' dzibinom(x = 0:10, rho = 0.1, size = 5, prob = 0.5)
#'
#' # Example 2: pzibinom
#' pzibinom(q = 2, rho = 0.1, size = 5, prob = 0.5)
#'
#' # Example 3: qzibinom
#' qzibinom(p = pzibinom(2, 0.1, 5, 0.5), rho = 0.1, size = 5, prob = 0.5)
#'
#' # Example 4: rzibinom
#' n <- 1e+5
#' rho <- 0.2
#' size <- 5
#' prob <- 0.5
#' mean(rzibinom(n, rho, size, prob)) # Sample mean
#' (size * prob) * (1 - rho) # Theoretical mean
#' @usage NULL
zibinom <- function(x, q, p, n, rho, size, prob, log, log.p, lower.tail) {
  NULL
}
CheckArgs_zibinom <-
  function(x = 1L, q = 0.5, p = 0, n = 1L, rho = 0.5,
           size = 1L, prob = 0.5,
           I = c(1, 2),
           log = FALSE, log.p = FALSE, lower.tail = FALSE,
           leftmost.closed = TRUE, rightmost.closed = TRUE,
           msg.tryCatch.x = paste("Argument 'x' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.x = paste("Argument 'x'",
                              "must be a non-negative integer-valued vector"),
           msg.tryCatch.q = paste("Argument 'q' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.q = paste("Argument 'q'",
                              "must be a numeric vector"),
           msg.tryCatch.p = paste("Argument 'p' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.p = paste("Argument 'p'",
                              "must be a vector of (log) probabilities"),
           msg.tryCatch.n = paste("Argument 'n' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.n = paste("Argument 'n'",
                              "must be a positive integer value"),
           msg.tryCatch.rho = paste("Argument 'rho' is missing",
                                    "or corresponding object cannot be found"),
           msg.stop.rho = paste("Argument 'rho'",
                                "must be a numeric value in [0,1)"),
           msg.tryCatch.size = paste("Argument 'size' is missing",
                                     "or corresponding object cannot be found"),
           msg.stop.size = paste("Argument 'size'",
                                 "must be a",
                                 "positive integer value"),
           msg.tryCatch.prob = paste("Argument 'prob' is missing",
                                     "or corresponding object cannot be found"),
           msg.stop.prob = paste("Argument 'prob'",
                                 "must be a non-negative numeric value in (0,1)"),
           msg.tryCatch.I = paste("Argument 'I' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.I = paste("Argument 'I' (l, u)",
                              "must be a length-two numeric vector",
                              "being l leq u"),
           msg.tryCatch.log = paste("Argument 'log' is missing",
                                    "or corresponding object cannot be found"),
           msg.stop.log = paste("Argument 'log'",
                                "must be either TRUE or FALSE"),
           msg.tryCatch.log.p = paste("Argument 'log.p' is missing",
                                      "or corresponding object cannot be found"),
           msg.stop.log.p = paste("Argument 'log.p'",
                                  "must be either TRUE or FALSE"),
           msg.tryCatch.lower.tail = paste("Argument 'lower.tail' is missing",
                                           "or corresponding object cannot be found"),
           msg.stop.lower.tail = paste("Argument 'lower.tail'",
                                       "must be either TRUE or FALSE"),
           msg.tryCatch.leftmost.closed = paste("Argument 'leftmost.closed' is missing",
                                                "or corresponding object cannot be found"),
           msg.stop.leftmost.closed = paste("Argument 'leftmost.closed'",
                                            "must be either TRUE or FALSE"),
           msg.tryCatch.rightmost.closed = paste("Argument 'rightmost.closed' is missing",
                                                 "or corresponding object cannot be found"),
           msg.stop.rightmost.closed = paste("Argument 'rightmost.closed'",
                                             "must be either TRUE or FALSE"),
           eval.stop = TRUE, Check.err = NULL) {
    # Declaration
    if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()

    # Check : x
    Check.x <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = x)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.x,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.x)) {
      Check.x <- ifelse(all(x >= 0), TRUE, FALSE)
    }
    if (isFALSE(Check.x)) {
      CheckArgs_AddError(Msg = msg.stop.x,
                              CheckArgs.Envir = Check.err)
    }

    # Check : q
    Check.q <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = q)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.q,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.q)) {
      CheckArgs_AddError(Msg = msg.stop.q,
                              CheckArgs.Envir = Check.err)
    }

    # Check : p
    # Note : if p equal 0, then both p and log(p) belong to [0,1]
    if (isTRUE(log.p)) {
      tryCatch(p <- exp(p), error = function(e) NULL)
    }
    Check.p <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = p)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.p,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.p)) {
      Check.p <- ifelse(all(p >= 0, p <= 1), TRUE, FALSE)
    }
    if (isFALSE(Check.p)) {
      CheckArgs_AddError(Msg = msg.stop.p,
                              CheckArgs.Envir = Check.err)
    }

    # Check : n
    Check.n <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = n, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.n,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.n)) {
      Check.n <- ifelse(n > 0, TRUE, FALSE)
    }
    if (isFALSE(Check.n)) {
      CheckArgs_AddError(Msg = msg.stop.n,
                              CheckArgs.Envir = Check.err)
    }

    # Check : rho
    Check.rho <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = rho, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.rho,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.rho)) {
      Check.rho <- ifelse(all(rho >= 0, rho < 1), TRUE, FALSE)
    }
    if (isFALSE(Check.rho)) {
      CheckArgs_AddError(Msg = msg.stop.rho,
                              CheckArgs.Envir = Check.err)
    }

    # Check : size
    Check.size <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = size, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.size,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.size)) {
      Check.size <- ifelse(size >= 1L, TRUE, FALSE)
    }
    if (isFALSE(Check.size)) {
      CheckArgs_AddError(Msg = msg.stop.size,
                              CheckArgs.Envir = Check.err)
    }

    # Check : prob
    Check.prob <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = prob, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.prob,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.prob)) {
      Check.prob <- ifelse(all(prob > 0, prob < 1), TRUE, FALSE)
    }
    if (isFALSE(Check.prob)) {
      CheckArgs_AddError(Msg = msg.stop.prob,
                              CheckArgs.Envir = Check.err)
    }

    # Check : I
    Check.I <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = I, min.length = 2L, max.length = 2L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.I,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.I)) {
      Check.I <- ifelse(I[1] <= I[2], TRUE, FALSE)
    }
    if (isFALSE(Check.I)) {
      CheckArgs_AddError(Msg = msg.stop.I,
                              CheckArgs.Envir = Check.err)
    }

    # Check : log
    Check.log <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = log, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.log,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.log)) {
      CheckArgs_AddError(Msg = msg.stop.log,
                              CheckArgs.Envir = Check.err)
    }

    # Check : log.p
    Check.log.p <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = log.p, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.log.p,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.log.p)) {
      CheckArgs_AddError(Msg = msg.stop.log.p,
                              CheckArgs.Envir = Check.err)
    }

    # Check : lower.tail
    Check.lower.tail <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = lower.tail, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.lower.tail,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.lower.tail)) {
      CheckArgs_AddError(Msg = msg.stop.lower.tail,
                              CheckArgs.Envir = Check.err)
    }

    # Check : leftmost.closed
    Check.leftmost.closed <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = leftmost.closed, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.leftmost.closed,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.leftmost.closed)) {
      CheckArgs_AddError(Msg = msg.stop.leftmost.closed,
                              CheckArgs.Envir = Check.err)
    }

    # Check : rightmost.closed
    Check.rightmost.closed <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = rightmost.closed, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.rightmost.closed,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.rightmost.closed)) {
      CheckArgs_AddError(Msg = msg.stop.rightmost.closed,
                              CheckArgs.Envir = Check.err)
    }

    # Finish
    list(Check.err = Check.err,
         Check.xxx = c(Check.x = Check.x,
                       Check.q = Check.q,
                       Check.p = Check.p,
                       Check.n = Check.n,
                       Check.rho = Check.rho,
                       Check.size = Check.size,
                       Check.prob = Check.prob,
                       Check.I = Check.I,
                       Check.log = Check.log,
                       Check.log.p = Check.log.p,
                       Check.lower.tail = Check.lower.tail,
                       Check.leftmost.closed = Check.leftmost.closed,
                       Check.rightmost.closed = Check.rightmost.closed))
  }

#' @rdname zibinom
#' @export
dzibinom <- function(x, rho, size, prob, log = FALSE) {
  # Error handling
  CheckArgs_dzibinom(x, rho, size, prob, log)
  # Main
  res <- rho * as.numeric(x == 0) + (1 - rho) *
    stats::dbinom(x = x, size = size, prob = prob)
  if (log) res <- log(res)
  res
}
CheckArgs_dzibinom <- function(x = 1L, rho = 0.5, size = 1L, prob = 0.5,
                               log = FALSE, eval.stop = TRUE,
                               Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(x = x, rho = rho, size = size, prob = prob,
                                 log = log, eval.stop = eval.stop,
                                 Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.x",
                                               "Check.rho",
                                               "Check.size",
                                               "Check.prob",
                                               "Check.log")]
  CheckArgs
}

#' @rdname zibinom
#' @export
pzibinom <- function(q, rho, size, prob, lower.tail = TRUE, log.p = FALSE) {
  CheckArgs_pzibinom(q, rho, size, prob, lower.tail, log.p)
  cdf.pois <- stats::pbinom(q = q, size = size, prob = prob,
                     lower.tail = TRUE, log.p = FALSE)
  res <- rho + (1 - rho) * cdf.pois
  res <- ifelse(q >= 0, res, 0)
  if (isFALSE(lower.tail)) res <- 1 - res
  if (log.p) res <- log(res)
  res
}
CheckArgs_pzibinom <- function(q = 1, rho = 0.5, size = 1L, prob = 0.5,
                               lower.tail = FALSE, log.p = FALSE,
                               eval.stop = TRUE, Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(q = q, rho = rho,
                                 size = size, prob = prob,
                                 lower.tail = lower.tail, log.p = log.p,
                                 eval.stop = eval.stop, Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.q",
                                               "Check.rho",
                                               "Check.size",
                                               "Check.prob",
                                               "Check.lower.tail",
                                               "Check.log.p")]
  CheckArgs
}

#' @rdname zibinom
#' @export
qzibinom <- function(p, rho, size, prob, lower.tail = TRUE, log.p = FALSE) {
  CheckArgs_qzibinom(p, rho, size, prob, lower.tail, log.p)
  if (log.p)  p <- exp(p)
  if (isFALSE(lower.tail)) p <- 1 - p

  p.adj <- pmax(((p - rho) / (1 - rho)), 0)
  res <- stats::qbinom(p = p.adj, size = size, prob = prob)
  res
}
CheckArgs_qzibinom <- function(p = 0, rho = 0.5, size = 1L, prob = 0.5,
                               lower.tail = FALSE, log.p = FALSE,
                               eval.stop = TRUE, Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(p = p, rho = rho, size = size, prob = prob,
                                 lower.tail = lower.tail,
                                 log.p = log.p,
                                 eval.stop = eval.stop, Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.p",
                                               "Check.rho",
                                               "Check.size",
                                               "Check.prob",
                                               "Check.lower.tail",
                                               "Check.log.p")]
  CheckArgs
}

#' @rdname zibinom
#' @export
rzibinom <- function(n, rho, size, prob) {
  CheckArgs_rzibinom(n, rho, size, prob)
  p <- stats::runif(n)
  res <- qzibinom(p = p, rho = rho, size = size, prob = prob)
  res
}
CheckArgs_rzibinom <- function(n = 1L, rho = 0.5, size = 1L, prob = 0.5,
                               eval.stop = TRUE, Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(n = n, rho = rho, size = size, prob = prob,
                                 eval.stop = eval.stop, Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.n",
                                               "Check.rho",
                                               "Check.size",
                                               "Check.prob")]
  CheckArgs
}

CheckArgs_dbinom <- function(x = 1L, size = 10, prob =0.5,
                             log = FALSE, eval.stop = TRUE, Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(x = x, size = size, prob = prob,
                                 log = log, eval.stop = eval.stop,
                                 Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.x",
                                               "Check.size",
                                               "Check.prob",
                                               "Check.log")]
  CheckArgs
}



# Under construction ------------------------------------------------------
mzibinom <- function(rho, size, prob) {
  CheckArgs_mzipois(rho, size, prob)
  (1 - rho) * size * prob
}
CheckArgs_mzibinom <- function(rho = 0.5, size = 1L, prob = 0.5,
                               eval.stop = TRUE, Check.err = NULL) {
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_zibinom(rho = rho, size = size, prob = prob,
                                 eval.stop = eval.stop, Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.rho",
                                               "Check.size",
                                               "Check.prob")]
  CheckArgs
}

