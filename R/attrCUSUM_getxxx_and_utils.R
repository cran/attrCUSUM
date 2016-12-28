#' @title Compute ATSs and Other Informations on Attribute VSI CUSUM Chart
#' @description Computation of ANSSs, ATSs and other related informations on
#'   attribute VSI CUSUM control chart for mean increase
#'   based on the Markov chain approach.
#' @details For CUSUM control chart for detecting mean increase,
#'    the CUSUM statistic \eqn{(C_{t})} are defined as following:
#'    \deqn{C_{t}=max(C_{t-1}, 0) + X_{t} - k,    t=1,2,...,}
#'    wherein \eqn{X_{t}} is a count process of interest,
#'    \eqn{k} is the reference value and \eqn{C_{0}} is the initial state.
#'
#'    The argument \code{process} is an object of function such that
#'    for given integer-valued vector \code{x},
#'    \code{process(x)} returns \eqn{P(X=x)}
#'    where \eqn{X} is a random variable of interest.
#'
#'    If the number of sub-intervals
#'    for transient states of Markov chain is greater than \code{maxnsubI},
#'    it will be set to be a suitable value
#'    being less than or equal \code{maxnsubI}.
#'
#'    For ANSS,
#'    it is the same as that \code{psi.s + psi.l + 1}.
#'
#'    For ATS,
#'    it is the same as that \code{(psi.s * ds) + (psi.l * dl) + di}.
#'
#'    Invalid arguments rise an error with a helpful message.
#'
#'    This function currently provides only the case for monitoring of mean increase.
#'    We leave other issues as our future work.
#'
#' @param refv A reference value of CUSUM statistic.
#' @param contl A control limit in CUSUM control scheme.
#' @param c.zero An initial state of CUSUM statistic (default is 0).
#' @param warnl A warning limit in VSI control scheme (default is 0).
#' @param ds The shorter sampling interval in VSI CUSUM control scheme (default is 1).
#' @param dl The longer sampling interval in VSI CUSUM control scheme.
#' @param di The sampling interval at initial state in VSI CUSUM control scheme.
#' @param process An object of function
#'    standing for pmf of count process of interest. See 'Details'.
#' @param maxndec The maximum number of decimal places
#'     of \code{refv} (\eqn{\le} 7L).
#' @param maxnumsubI The maximum number of sub-intervals in [100,6000] to be used in
#'    applying the Markov chain approach.
#' @return A list including followings:
#'    \item{endpoints}{Endpoints used for
#'      implementation of a matrix of transition probabilities.}
#'    \item{numsubI}{Number of sub-intervals used for
#'      implementation of a matrix of transition probabilities.}
#'    \item{lensubI}{Length of sub-intervals used for
#'      implementation of a matrix of transition probabilities.}
#'    \item{Q}{A matrix of transition probabilities for Markov chain approach.}
#'    \item{I_minus_Q}{Matrix \code{(I - Q)}
#'      where \code{I} is an identity matrix of the same size as the \code{Q}.}
#'    \item{I_minus_Q_inv}{Inverse matrix of \code{(I - Q)}.}
#'    \item{I_minus_Q_inv_1}{Row sums of \code{I_minus_Q_inv}.}
#'    \item{initpr}{A matrix of initial probabilities
#'      for Markov chain approach.}
#'    \item{ds}{The shorter sampling interval in VSI CUSUM control scheme.}
#'    \item{dl}{The longer sampling interval in VSI CUSUM control scheme.}
#'    \item{di}{The sampling interval at initial state
#'      in VSI CUSUM control scheme.}
#'    \item{psi.s}{Long-run proportion of shorter sampling intervals
#'      except initial state. See details}
#'    \item{psi.l}{Long-run proportion of longer sampling intervals
#'      except initial state. See details}
#'    \item{refv.act}{A reference value of CUSUM statistic used actually for
#'      computation.}
#'    \item{contl.act}{A control limit in CUSUM control scheme used actually for
#'      computation.}
#'    \item{warnl.act}{A warning limit in VSI control scheme used actually for
#'      computation.}
#'    \item{c.zero.act}{An initial state of CUSUM statistic used actually for
#'      computation.}
#'    \item{ANSS}{The computed average number of samples to signal for
#'      FSI CUSUM chart.}
#'    \item{ATS}{The computed average time to signal for
#'      FSI CUSUM chart.}
#' @references Reynolds et al. (1990).
#'    CUSUM Charts with Variable Sampling Intervals,
#'    \emph{Technometrics}, 32(4), 371-384.
#' @seealso \code{\link{getContl}}, \link{getAve_Poisson},
#'   \link{getAve_binomial}.
#' @examples
#' # Example 1: Poisson distribution
#' getAve(refv = 5, contl = 8, c.zero = 0, warnl = 1, ds = 0.1,
#'        dl = NULL, di = NULL, process = function(x) dpois(x, lambda = 4),
#'        maxndec = 7L, maxnumsubI = 500L)
#' @export
getAve <- function(refv, contl, c.zero = 0, warnl = 0, ds = 1, dl = NULL,
                   di = NULL, process, maxndec = 7L, maxnumsubI = 6000L) {
  op <- options(warn = -1L)
  on.exit(options(op), add = TRUE)
  CheckArgs_getAve(refv = refv, contl = contl, c.zero = c.zero, warnl = warnl,
                     ds = ds, dl = dl, di = di, process = process,
                     maxndec = maxndec, maxnumsubI = maxnumsubI)
  attrCUSUM_inc_getAve(refv = refv, contl = contl, c.zero = c.zero,
                       warnl = warnl, ds = ds, dl = dl, di = di,
                       process = process, maxndec = maxndec,
                       maxnumsubI = maxnumsubI)
}
CheckArgs_getxxx <-
  function(anss.target = 100, refv = 5, contl = 10, c.zero = 0, warnl = 0,
           ds = 1, dl = 1, di = 1,
           process = function(x) dpois(x, lambda = 1),
           maxndec = 7L, maxnumsubI = 1000L,
           msg.tryCatch.anss.target =
             paste("Argument 'anss.target' is missing",
                   "or corresponding object cannot be found"),
           msg.stop.anss.target = paste("Argument 'anss.target' ",
                                        "must be a positive numeric value in ",
                                        "(1,",
                                        getOption("attrCUSUM.limit.anss.target"),
                                        "]", sep = ""),
           msg.tryCatch.refv = paste("Argument 'refv' is missing",
                                     "or corresponding object cannot be found"),
           msg.stop.refv = paste("Argument 'refv'",
                                 "must be a positive numeric value"),
           msg.tryCatch.contl = paste("Argument 'contl' is missing",
                                      "or corresponding object cannot be found"),
           msg.stop.contl = paste("Argument 'contl'",
                                  "must be a positive numeric value"),
           msg.tryCatch.c.zero = paste("Argument 'c.zero' is missing",
                                       "or corresponding object cannot be found"),
           msg.stop.c.zero = paste("Argument 'c.zero'",
                                   "must be a numeric value",
                                   "being greater than or equal 'refv'"),
           msg.tryCatch.warnl = paste("Argument 'warnl' is missing",
                                      "or corresponding object cannot be found"),
           msg.stop.warnl = paste("Argument 'warnl'",
                                  "must be a numeric value",
                                  "in [-'refv', 'contl']"),
           msg.tryCatch.ds = paste("Argument 'ds' is missing",
                                   "or corresponding object cannot be found"),
           msg.stop.ds = paste("Argument 'ds'",
                               "must be a numeric value",
                               "in (0, 1]"),
           msg.tryCatch.dl = paste("Argument 'dl' is missing",
                                   "or corresponding object cannot be found"),
           msg.stop.dl = paste("Argument 'dl'",
                               "must be NULL or a numeric value",
                               "being greater than or equal 'ds'"),
           msg.tryCatch.di = paste("Argument 'di' is missing",
                                   "or corresponding object cannot be found"),
           msg.stop.di = paste("Argument 'di'",
                               "must be NULL or a positive numeric value"),
           msg.tryCatch.process =
             paste("Argument 'process' is missing",
                   "or corresponding object cannot be found"),
           msg.stop.process = paste("Argument 'process'",
                                    "is not an object of function",
                                    "representing pmf,",
                                    "or memory overflow error occurred"),
           msg.tryCatch.maxndec =
             paste("Argument 'maxndec' is missing",
                   "or corresponding object cannot be found"),
           msg.stop.maxndec = paste("Argument 'maxndec'",
                                    "must be a non-negative integer value",
                                    "being less than or equal",
                                    getOption("attrCUSUM.limit.maxndec")),
           msg.tryCatch.maxnumsubI =
             paste("Argument 'maxnumsubI' is missing",
                   "or corresponding object cannot be found"),
           msg.stop.maxnumsubI = paste("Argument 'maxnumsubI' ",
                                       "must be a integer value in ",
                                       "[",
                                       getOption("attrCUSUM.limit.minnumsubI"),
                                       ",",
                                       getOption("attrCUSUM.limit.maxnumsubI"),
                                       "]", sep = ""),
           eval.stop = FALSE,
           Check.err = NULL) {
    # Declaration
    if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()

    # Check : anss.target
    Check.anss.target <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = anss.target, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.anss.target,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.anss.target)) {
      Check.anss.target <- ifelse(all(anss.target > 1,
                               anss.target <=
                                 getOption("attrCUSUM.limit.anss.target")),
                           TRUE, FALSE)
    }
    if (isFALSE(Check.anss.target)) {
      CheckArgs_AddError(Msg = msg.stop.anss.target,
                              CheckArgs.Envir = Check.err)
    }

    # Check : refv
    Check.refv <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = refv, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.refv,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.refv)) {
      Check.refv <- ifelse(all(refv > 0), TRUE, FALSE)
    }
    if (isFALSE(Check.refv)) {
      CheckArgs_AddError(Msg = msg.stop.refv,
                              CheckArgs.Envir = Check.err)
    }

    # Check : contl
    Check.contl <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = contl, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.contl,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.contl)) {
      Check.contl <- ifelse(all(contl > 0), TRUE, FALSE)
    }
    if (isFALSE(Check.contl)) {
      CheckArgs_AddError(Msg = msg.stop.contl,
                              CheckArgs.Envir = Check.err)
    }

    # Check : c.zero
    Check.c.zero <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = c.zero, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.c.zero,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.refv), isTRUE(Check.contl), isTRUE(Check.c.zero))) {
      Check.c.zero <- ifelse(all(c.zero >= -1 * refv),
                             TRUE, FALSE)
    }
    if (isFALSE(Check.c.zero)) {
      CheckArgs_AddError(Msg = msg.stop.c.zero,
                              CheckArgs.Envir = Check.err)
    }

    # Check : warnl
    Check.warnl <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = warnl, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.warnl,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.refv), isTRUE(Check.contl), isTRUE(Check.warnl))) {
      Check.warnl <- ifelse(all(warnl >= -1 * refv, warnl <= contl),
                            TRUE, FALSE)
    }
    if (isFALSE(Check.warnl)) {
      CheckArgs_AddError(Msg = msg.stop.warnl,
                              CheckArgs.Envir = Check.err)
    }

    # Check : ds
    Check.ds <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = ds, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.ds,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.ds)) {
      Check.ds <- ifelse(all(ds > 0, ds <= 1), TRUE, FALSE)

    }
    if (isFALSE(Check.ds)) {
      CheckArgs_AddError(Msg = msg.stop.ds,
                              CheckArgs.Envir = Check.err)
    }

    # Check : dl
    Check.dl <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = dl, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.dl,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.ds), isTRUE(Check.dl))) {
      Check.dl <- ifelse(all(dl >= ds), TRUE, FALSE)
    }
    if (isFALSE(Check.dl)) {
      if (is.null(dl)) {
        Check.dl <- TRUE
      } else {
        CheckArgs_AddError(Msg = msg.stop.dl,
                                CheckArgs.Envir = Check.err)
      }
    }

    # Check : di
    Check.di <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = di, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.di,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.di))) {
      Check.di <- ifelse(all(di > 0), TRUE, FALSE)
    }
    if (isFALSE(Check.di)) {
      if (is.null(di)) {
        Check.di <- TRUE
      } else {
        CheckArgs_AddError(Msg = msg.stop.di,
                                CheckArgs.Envir = Check.err)
      }
    }

    # Check : process
    Check.process <-
      tryCatch(is.function(x = process),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.process,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.refv), isTRUE(Check.contl), isTRUE(Check.process))) {
      max.tmp <- round(ceiling(refv + contl) + 1)
      Check.process <-
        tryCatch(isTRUE(all(round(process(0:max.tmp), 15L) >= 0,
                            round(process(0:max.tmp), 15L) <= 1,
                            sum(process(0:max.tmp) > 0) >= 2L)),
                 error = function(e) FALSE)
      if (isTRUE(Check.process)) {
        Check.process <-
          tryCatch(isTRUE(round(sum(process(0:max.tmp)), 15L) <= 1),
                   error = function(e) FALSE)
      }
    }
    if (isFALSE(Check.process)) {
      CheckArgs_AddError(Msg = msg.stop.process,
                              CheckArgs.Envir = Check.err)

    }

    # Check : maxndec
    Check.maxndec <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = maxndec, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.maxndec,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.maxndec)) {
      Check.maxndec <- ifelse(all(maxndec >= 0L,
                                  maxndec <=
                                    getOption("attrCUSUM.limit.maxndec")),
                              TRUE, FALSE)
    }
    if (isFALSE(Check.maxndec)) {
      CheckArgs_AddError(Msg = msg.stop.maxndec,
                              CheckArgs.Envir = Check.err)
    }

    # Check : maxnumsubI
    Check.maxnumsubI <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = maxnumsubI, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.maxnumsubI,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (all(isTRUE(Check.refv), isTRUE(Check.contl), isTRUE(Check.maxnumsubI))) {
      Check.maxnumsubI <- ifelse(all(maxnumsubI >=
                                       getOption("attrCUSUM.limit.minnumsubI"),
                                     maxnumsubI <=
                                       getOption("attrCUSUM.limit.maxnumsubI")),
                                 TRUE, FALSE)
    }
    if (isFALSE(Check.maxnumsubI)) {
      CheckArgs_AddError(Msg = msg.stop.maxnumsubI,
                              CheckArgs.Envir = Check.err)
    }

    # Finish
    if (eval.stop) CheckArgs_ShowMsg(Check.err)
    list(Check.err = Check.err,
         Check.xxx = c(Check.anss.target = Check.anss.target,
                       Check.refv = Check.refv,
                       Check.contl = Check.contl,
                       Check.c.zero = Check.c.zero,
                       Check.warnl = Check.warnl,
                       Check.ds = Check.ds,
                       Check.dl = Check.dl,
                       Check.di = Check.di,
                       Check.process = Check.process,
                       Check.maxndec = Check.maxndec,
                       Check.maxnumsubI = Check.maxnumsubI))
  }
CheckArgs_getAve <- function(refv = 5, contl = 10, c.zero = 0, warnl = 0,
                             ds = 1, dl = 1, di = 1,
                             process = function(x) stats::dpois(x, lambda = 1),
                             maxndec = 7L, maxnumsubI = 1000L,
                             eval.stop = TRUE, Check.err = NULL) {
  if (isFALSE(getOption("attrCUSUM.Need.CheckArgs_getAve"))) {
    return(invisible())
  }
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_getxxx(refv = refv, contl = contl, c.zero = c.zero,
                                warnl = warnl, ds = ds, dl = dl, di = di,
                                process = process,
                                maxndec = maxndec, maxnumsubI = maxnumsubI,
                                Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.refv",
                                               "Check.contl", "Check.c.zero",
                                               "Check.warnl",
                                               "Check.ds", "Check.dl",
                                               "Check.di", "Check.process",
                                               "Check.maxndec",
                                               "Check.maxnumsubI")]
  CheckArgs
}

#' @title Compute Control Limits for Attribute FSI CUSUM Chart
#' @description Computation of suitable control limits
#'    for attribute FSI-CUSUM control chart for mean increase
#'    based on specified reference value and in-control ANSS/ARL.
#' @details For CUSUM control chart for detecting mean increase,
#'    the CUSUM statistic \eqn{(C_{t})} are defined as following:
#'    \deqn{C_{t}=max(C_{t-1}, 0) + X_{t} - k,    t=1,2,...,}
#'    wherein \eqn{X_{t}} is a count process of interest,
#'    \eqn{k} is the reference value and \eqn{C_{0}} is the initial state.
#'
#'    The argument \code{process} is an object of function such that
#'    for given integer-valued vector \code{x},
#'    \code{process(x)} returns \eqn{P(X=x)}
#'    where \eqn{X} is a random variable of interest.
#'
#'    If the number of sub-intervals
#'    for transient states of Markov chain is greater than \code{maxnsubI},
#'    it will be set to be a suitable value
#'    being less than or equal \code{maxnsubI}.
#'
#'    Invalid arguments rise an error with a helpful message.
#'
#'    This function currently provides only the case for monitoring of mean increase.
#'    We leave other issues as our future work.
#'
#' @param anss.target A predetermined in-control ANSS/ARL (\eqn{\le}50000L),
#'     default is 370.4.
#' @inheritParams getAve
#' @return A list including followings:
#'    \item{refv.act}{A reference value of CUSUM statistic used actually for
#'      computation.}
#'    \item{c.zero.act}{An initial state of CUSUM statistic used actually for
#'      computation.}
#'    \item{sol1,sol2}{Vector of control limits and corresponding ANSS/ARLs}
#' @references Reynolds et al. (1990).
#'    CUSUM Charts with Variable Sampling Intervals,
#'    \emph{Technometrics}, 32(4), 371-384.
#' @seealso \code{\link{getAve}}.
#' @examples
#' # Example 1: Poisson distribution
#' getContl(anss.target = 200, refv = 5, c.zero = 0,
#'          process = function(x) dpois(x, lambda = 4))
#' @export
getContl <- function(anss.target = 370.4, refv, c.zero = 0, process,
                     maxndec = 7L, maxnumsubI = 6000L) {
  op <- options(warn = -1L)
  on.exit(options(op), add = TRUE)
  CheckArgs_getContl(anss.target = anss.target, refv = refv, c.zero = c.zero,
                       process = process, maxndec = maxndec,
                     maxnumsubI = maxnumsubI)
  attrCUSUM_inc_getContl(anss.target = anss.target, refv = refv,
                         c.zero = c.zero, process = process, maxndec = maxndec,
                         maxnumsubI = maxnumsubI)
}

CheckArgs_getContl <- function(anss.target = 1L, refv = 5, c.zero = 0,
                               process = function(x) stats::dpois(x, lambda = 1),
                               maxndec = 7L, maxnumsubI = 1000L,
                               eval.stop = TRUE, Check.err = NULL) {
  if (isFALSE(getOption("attrCUSUM.Need.CheckArgs_getContl"))) {
    return(invisible())
  }
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()
  CheckArgs <- CheckArgs_getxxx(anss.target = anss.target, refv = refv,
                                c.zero = c.zero, process = process,
                                maxndec = maxndec, maxnumsubI = maxnumsubI,
                                Check.err = Check.err)
  if (eval.stop) CheckArgs_ShowMsg(CheckArgs$Check.err)
  CheckArgs$Check.xxx <- CheckArgs$Check.xxx[c("Check.anss.target",
                                               "Check.refv",
                                               "Check.c.zero",
                                               "Check.process",
                                               "Check.maxndec",
                                               "Check.maxnumsubI")]
  CheckArgs
}

attrCUSUM_getTol <- function(refv, ndec.refv = NULL, minv, contl, maxndec,
                             maxnumsubI) {
  if (is.null(ndec.refv)) {
    ndec.refv <- ndecimal(refv, maxndec)  # ndecimal : returns #(decimal places)
  }
  tol <- as.numeric(paste("1e-", ndec.refv, sep = ""))
  numsubI.tmp <- (contl - minv) / tol
  if (numsubI.tmp > maxnumsubI) {
    tol.tmp <- (contl - minv) / maxnumsubI
    ndec.tmp <- which(round(tol.tmp, seq(maxndec)) > 0)[1]
    tol <- round(tol.tmp, ndec.tmp)
  }
  inc <- tol
  while (((contl - minv) / tol) > maxnumsubI) {
    tol <- tol + inc
  }
  tol
}

attrCUSUM_getATS <- function(ANSS, elements, warnl, c.zero, initpr,
                             I_minus_Q_inv, ds, dl, di) {
  I.psi.s <- findInterval(x = elements,
                          vec = warnl,
                          rightmost.closed = FALSE)
  I.psi.l <- 1 - I.psi.s
  psi.l <- as.numeric(initpr %*% I_minus_Q_inv %*% I.psi.l)
  psi.s <- as.numeric(initpr %*% I_minus_Q_inv %*% I.psi.s)
  if (ds == 1) {
    dl <- ds
  }
  if (all(is.null(dl), is.null(di))) {
    if (psi.l > 0) {
      if (c.zero >= warnl) {
        dl <- (ANSS - (1 + psi.s) * ds) / psi.l
        di <- ds
      } else {
        di <- dl <- (ANSS - psi.s * ds) / (psi.l + 1)
      }
    } else {
      dl <- 0
      di <- ds
    }
  }
  if (all(is.null(dl), !is.null(di))) {
    if (psi.l > 0) {
      dl <- (ANSS - (psi.s * ds) - di) / psi.l
    } else {
      dl <- 0
    }
  }
  if (all(!is.null(dl), is.null(di))) {
    di <- ifelse(c.zero >= warnl, ds, dl)
  }
  ATS <- di + (dl * psi.l) + (ds * psi.s)
  res <- list(ds.act = ds, dl.act = dl, di.act = di,
              psi.s = psi.s, psi.l = psi.l, ATS = ATS)
  res
}

#' @title Convert from Data to FSI CUSUM Statistic
#' @description Conversion of data to FSI CUSUM statistic
#'    for monitoring of mean increase.
#' @param Xt A non-negative integer-valued vector of count process of interest.
#' @inheritParams getAve
#' @details For CUSUM control chart for detecting mean increase,
#'    the CUSUM statistic \eqn{(C_{t})} are defined as following:
#'    \deqn{C_{t}=max(C_{t-1}, 0) + X_{t} - refv,  t=1,2,...,}
#'    wherein \eqn{X_{t}} is a count process of interest.
#' @return A time-series object of
#'    CUSUM statistic for monitoring of mean increase.
#' @examples
#' # Example 1: Poisson distribution
#' Ct <- XtToCt(Xt = rpois(200L, 4), refv = 5, c.zero = 0)
#' plot(Ct, type = "o", pch = 16, main = "CUSUM statistic",
#'      ylab = expression(C[t]),
#'      sub = expression(paste(C[t],"=", "max(",C[t - 1],",",0,")",
#'                             "+ refv -",X[t])))
#' @export
XtToCt <- function(Xt, refv, c.zero = 0, maxndec = 7L) {
  CheckArgs_XtToCt(Xt = Xt, refv = refv, c.zero = c.zero, maxndec = maxndec)
  attrCUSUM_inc_XtToCt(Xt = Xt, refv = refv, c.zero = c.zero, maxndec = maxndec)
}
CheckArgs_Xt <- function(Xt = seq(10),
                         msg.tryCatch.Xt =
                           paste("Argument 'Xt' is missing",
                                 "or corresponding object cannot be found"),
                         msg.stop.Xt = paste("Argument 'Xt'",
                                             "must be a non-negative vector",
                                             "of count process of interest."),
                         eval.stop = TRUE, Check.err = NULL) {
  # Declaration
  if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()

  # Check : Xt
  Check.Xt <-
    tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                     list(x = Xt)),
             error = function(e) {
               CheckArgs_AddError(Msg = msg.tryCatch.Xt,
                                  CheckArgs.Envir = Check.err)
               NA})

  if (isTRUE(Check.Xt)) {
    Check.Xt <- ifelse(all(Xt >= 0L), TRUE, FALSE)
  }
  if (isFALSE(Check.Xt)) {
    CheckArgs_AddError(Msg = msg.stop.Xt,
                       CheckArgs.Envir = Check.err)
  }
  # Finish
  if (eval.stop) CheckArgs_ShowMsg(Check.err)
  list(Check.err = Check.err,
       Check.xxx = c(Check.Xt = Check.Xt))
}
CheckArgs_XtToCt <- function(Xt, refv, c.zero, maxndec, eval.stop = TRUE) {
  # Declaration
  Check.err <- CheckArgs_NewEnvir()

  CheckArgs.1 <- CheckArgs_Xt(Xt = Xt, eval.stop = FALSE, Check.err = Check.err)
  CheckArgs.2 <-
    CheckArgs_getxxx(refv = refv, c.zero = c.zero, maxndec = maxndec,
                     msg.stop.refv = paste("Argument 'refv'",
                                           "must be a positive numeric value"),
                     eval.stop = FALSE, Check.err = Check.err)

  if (eval.stop) CheckArgs_ShowMsg(Check.err)
  list(Check.err = Check.err,
       Check.xxx = c(CheckArgs.1$Check.xxx,
                     CheckArgs.2$Check.xxx[c("Check.refv",
                                             "Check.c.zero",
                                             "Check.maxndec")]))
}

