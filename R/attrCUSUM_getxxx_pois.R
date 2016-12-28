
#' @name getAve_Poisson
#' @title Compute ATSs and Other Informations on (Zero-Inflated) Poisson CUSUM Chart
#' @description Computation of ANSSs, ATSs and other related informations on
#'   (zero-inflated) Poisson CUSUM chart.
#' @details \code{\link{getAve_pois}} and \code{\link{getAve_zipois}} are
#'   wrapper functions of \code{\link{getAve}}
#'   for (zero-inflated) Poisson CUSUM chart.
#' @param rho A length-one numeric vector of zero-inflated parameter in [0,1).
#' @param lambda A length-one positive numeric vector of mean.
#' @inheritParams getAve
#' @return A list from \code{\link{getAve}}.
#'   See \code{\link{getAve}} for more information.
#' @references White et al. (1997).
#'    POISSON CUSUM VERSUS c CHART FOR DEFECT DATA,
#'    \emph{Quality Engineering}, \strong{9}:4, 673-679.
#' @seealso \code{\link{getAve}}, \link{getAve_binomial}.
#' @examples
#' # Example 1: Reproduction of results from White et al. (1997)
#' tbl <- list()
#' arglist <- list(list(mu.a = 1, k = 2, h = 2),
#'                 list(mu.a = 4, k = 5, h = 8),
#'                 list(mu.a = 8, k = 10, h = 10),
#'                 list(mu.a = 12, k = 15, h = 11))
#' deltas <- seq(0, 2.5, 0.5)
#' for(i in seq_along(arglist)) {
#'   argument <- arglist[[i]]
#'   arl <- numeric(length(deltas))
#'   for(j in seq_along(deltas)) {
#'     std.a <- sqrt(argument$mu.a)
#'     arl[j] <- getAve_pois(lambda = argument$mu.a + (std.a * deltas[j]),
#'                           refv = argument$k,
#'                           contl = argument$h)$ANSS
#'   }
#'   tbl[[i]] <- round(arl, 2)
#' }
#' tbl <- data.frame(tbl)
#' colnames(tbl) <- c("CUSUM(2,2)", "CUSUM(5,8)", "CUSUM(10,10)", "CUSUM(15,11)")
#' rownames(tbl) <- as.character(deltas)
#' cat("colnames stand for CUSUM(k,h)\n",
#'     "rownames stand for delta\n", sep = "")
#' tbl
#'
#' # Example 2: ANSS profiles (h in seq(20L))
#' mu.a <-  4
#' k  <-  5
#' h <- seq(20)
#' ANSSs <- numeric(20)
#' for(i in seq(ANSSs)) {
#'   ANSSs[i] <- getAve_pois(lambda = mu.a, refv = k, contl = h[i], c.zero = 0,
#'                           warnl = 1, ds = 1)$ANSS
#' }
#' ANSSs
#'
#' @usage NULL
getAve_Poisson <- function(rho, lambda, refv, contl, c.zero, warnl, ds, dl, di,
                           maxndec, maxnumsubI, mean.inc, maxobs) {
  NULL
}

#' @rdname getAve_Poisson
#' @export
getAve_pois <- function(lambda, refv, contl, c.zero = 0, warnl = 0, ds = 1,
                        dl = NULL, di = NULL, maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dpois(lambda = lambda)
  process <- function(x) stats::dpois(x = x, lambda = lambda)
  CheckArgs_getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                   ds = ds, dl = dl, di = di, process = process,
                   maxndec = maxndec, maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getAve = FALSE)
  on.exit(options(op.old), add = TRUE)
  getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
         dl = dl, di = di, process = process, maxndec = maxndec,
         maxnumsubI = maxnumsubI)
}

#' @rdname getAve_Poisson
#' @export
getAve_zipois <- function(rho, lambda, refv, contl, c.zero = 0, warnl = 0,
                          ds = 1, dl = NULL, di = NULL, maxndec = 7L,
                          maxnumsubI = 6000L) {
  CheckArgs_dzipois(rho = rho, lambda = lambda)
  process <- function(x) dzipois(x = x, rho = rho, lambda = lambda)
  CheckArgs_getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                   ds = ds, dl = dl, di = di, process = process,
                   maxndec = maxndec, maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getAve = FALSE)
  on.exit(options(op.old), add = TRUE)
  getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl, ds = ds,
         dl = dl, di = di, process = process, maxndec = maxndec,
         maxnumsubI = maxnumsubI)
}

#' @name getContl_Poisson
#' @title Compute Control Limits for FSI Poisson CUSUM Control Chart
#' @description Computation of suitable control limits
#'    for (zero-inflated) FSI Poisson CUSUM control chart based on
#'    specified reference value and
#'    in-control ANSS/ARL.
#' @details \code{\link{getContl_pois}} and \code{\link{getContl_zipois}} are
#'   wrapper functions of \code{\link{getContl}} for
#'   (zero-inflated) Poisson distribution.
#' @inheritParams getAve_Poisson
#' @inheritParams getContl
#' @return A list from \code{\link{getContl}}.
#'   See \code{\link{getContl}} for more information.
#' @references White et al. (1997).
#'    POISSON CUSUM VERSUS c CHART FOR DEFECT DATA,
#'    \emph{Quality Engineering}, \strong{9}:4, 673-679.
#' @seealso \code{\link{getContl}}, \link{getContl_binomial}.
#' @examples
#' # Example 1: Poisson distribution
#' getContl_pois(lambda = 4, anss.target = 200, refv = 5, c.zero = 0)
#' @usage NULL
getContl_Poisson <- function(rho, lambda, anss.target, refv, c.zero, maxndec,
                             maxnumsubI) {
  NULL
}

#' @rdname getContl_Poisson
#' @export
getContl_pois <- function(lambda, anss.target = 370.4, refv, c.zero = 0,
                          maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dpois(lambda = lambda)
  process <- function(x) dpois(x = x, lambda = lambda)
  CheckArgs_getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                     process = process, maxndec = maxndec,
                     maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getContl = FALSE)
  on.exit(options(op.old), add = TRUE)
  getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
           process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
}

#' @rdname getContl_Poisson
#' @export
getContl_zipois <- function(rho, lambda, anss.target = 370.4, refv, c.zero = 0,
                            maxndec = 7L, maxnumsubI = 6000L) {
  CheckArgs_dzipois(rho = rho, lambda = lambda)
  process <- function(x) dzipois(x = x, rho = rho, lambda = lambda)
  CheckArgs_getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                     process = process, maxndec = maxndec,
                     maxnumsubI = maxnumsubI)
  op.old <- options(attrCUSUM.Need.CheckArgs_getContl = FALSE)
  on.exit(options(op.old), add = TRUE)
  getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
           process = process, maxndec = maxndec, maxnumsubI = maxnumsubI)
}

