#' @name getAve_binomial
#' @title Compute ATSs and Other Informations on (Zero-Inflated) Binomial CUSUM Chart
#' @description Computation of ANSSs, ATSs and other related informations on
#'   (zero-inflated) binomial CUSUM chart.
#' @details \code{\link{getAve_binom}} and \code{\link{getAve_zibinom}} are
#'   wrapper functions of \code{\link{getAve}}
#'   for (zero-inflated) binomial CUSUM chart.
#' @param rho A length-one numeric vector of zero-inflated parameter in [0,1).
#' @param size A length-one non-negative integer-valued vector
#'   of number of trials.
#' @param prob A length-one numeric vector of probability of success in [0,1].
#' @inheritParams getAve
#' @return A list from \code{\link{getAve}}.
#'   See \code{\link{getAve}} for more information.
#' @references Rakitzis et al. (2016).
#'    CUSUM Control Charts for the Monitoring of Zero-inflated Binomial Processes,
#'    \emph{Quality and Reliability Engineering International},
#'    \strong{32}, 465-483.
#' @seealso \code{\link{getAve}}, \link{getAve_Poisson}.
#' @examples
#' # Example 1: Reproduction of results from Rakitzis et al. (2016)
#' rho <- 0.9
#' size <- 100
#' prob0 <- 0.01
#' refv <- 0.26
#' contl <- 3.86
#' deltas <- seq(1, 2, 0.1)
#' ANSSs <- numeric(length(deltas))
#' for(i in seq(deltas)) {
#'   prob1 <- deltas[i] * prob0
#'   ANSSs[i] <- getAve_zibinom(rho = rho, size = size, prob = prob1,
#'                              refv = refv, contl = contl)$ANSS
#' }
#' names(ANSSs) <- deltas
#' ANSSs <- round(ANSSs, 2)
#' ANSSs
#'
#' # Example 2: ANSS profiles (h in seq(10L))
#' rho <- 0.9
#' size <- 100
#' prob0 <- 0.01
#' refv <- 0.26
#' h <- seq(10L)
#' ANSSs <- numeric(10)
#' for(i in seq(ANSSs)) {
#'   ANSSs[i] <- getAve_zibinom(rho = rho, size = size, prob = prob0,
#'                              refv = refv, contl = h[i], ds = 1)$ANSS
#' }
#' ANSSs
#'
#' @usage NULL
getAve_binomial <- function(rho, size, prob, refv, contl, c.zero, warnl, ds, dl,
                            di, maxndec, maxnumsubI, mean.inc, maxobs) {
  NULL
}


#' @rdname getAve_binomial
#' @export
getAve_binom <- function(size, prob, refv, contl, c.zero = 0, warnl = 0, ds = 1,
                         dl = NULL, di = NULL, maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dbinom(size = size, prob = prob)
  process <- function(x) dzibinom(x = x, rho = 0, size = size, prob = prob)
  CheckArgs_getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                   ds = ds, dl = dl, di = di, process = process,
                   maxndec = maxndec, maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getAve = FALSE)
  on.exit(options(op.old), add = TRUE)
  getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
         dl = dl, di = di, process = process, maxndec = maxndec,
         maxnumsubI = maxnumsubI)
}

#' @rdname getAve_binomial
#' @export
getAve_zibinom <- function(rho, size, prob, refv, contl, c.zero = 0, warnl = 0,
                           ds = 1, dl = NULL, di = NULL, maxndec = 7L,
                           maxnumsubI = 6000L) {
  CheckArgs_dzibinom(rho = rho, size = size, prob = prob)
  process <- function(x) dzibinom(x = x, rho = rho, size = size, prob = prob)
  CheckArgs_getAve(refv = refv, contl = contl, c.zero = c.zero,
                   warnl = warnl, ds = ds, dl = dl, di = di, process = process,
                   maxndec = maxndec, maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getAve = FALSE)
  on.exit(options(op.old), add = TRUE)
  getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
         dl = dl, di = di, process = process, maxndec = maxndec,
         maxnumsubI = maxnumsubI)
}

#' @name getContl_binomial
#' @title Compute Control Limits for FSI binomial CUSUM Control Chart
#' @description Computation of suitable control limits
#'    for (zero-inflated) FSI binomial CUSUM control chart based on
#'    specified reference value and
#'    in-control ANSS/ARL.
#' @details \code{\link{getContl_binom}} and \code{\link{getContl_zibinom}} are
#'   wrapper functions of \code{\link{getContl}}
#'   (zero-inflated) for binomial distribution.
#' @inheritParams getAve_binomial
#' @inheritParams getContl
#' @return A list from \code{\link{getContl}}.
#'   See \code{\link{getContl}} for more information.
#' @references Rakitzis et al. (2016).
#'    CUSUM Control Charts for the Monitoring of Zero-inflated Binomial Processes,
#'    \emph{Quality and Reliability Engineering International},
#'    \strong{32}, 465-483.
#' @seealso \code{\link{getContl}}, \link{getContl_Poisson}.
#' @examples
#' # Example 1: zero-inflated binomial distribution
#' getContl_zibinom(rho = 0.9, size = 100, prob = 0.01,
#'                  anss.target = 370.4, refv = 0.26, c.zero = 0)
#' @usage NULL
getContl_binomial <- function(rho, size, prob, anss.target, refv, c.zero,
                              maxndec, maxnumsubI) {
  NULL
}

#' @rdname getContl_binomial
#' @export
getContl_binom <- function(size, prob, anss.target = 370.4, refv, c.zero = 0,
                           maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dbinom(size = size, prob = prob)
  process <- function(x) dzibinom(x = x, rho = 0, size = size, prob = prob)
  CheckArgs_getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                     process = process, maxndec = maxndec,
                     maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getContl = FALSE)
  on.exit(options(op.old), add = TRUE)

  getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
           process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
}


#' @rdname getContl_binomial
#' @export
getContl_zibinom <- function(rho, size, prob, anss.target = 370.4, refv,
                             c.zero = 0, maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dzibinom(rho = rho, size = size, prob = prob)
  process <- function(x) dzibinom(x = x, rho = rho, size = size, prob = prob)
  CheckArgs_getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                     process = process, maxndec = maxndec,
                     maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getContl = FALSE)
  on.exit(options(op.old), add = TRUE)
  getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
           process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
}
