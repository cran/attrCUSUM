#' @name attrCUSUM-package
#' @title  Tools for Attribute CUSUM Control Chart
#' @description An implementation of tools for design of attribute VSI CUSUM chart.
#'   It currently provides information for monitoring of mean inrcease such as
#'   ANSS, ATS, a matrix of transient probabilities,
#'   suitable control limits when the data are (zero inflated) Poisson/binomial
#'   distribution.
#'   Functions in the tools such as \code{\link{getAve}} and \code{\link{getContl}}
#'   can be easily applied to other count processes.
#'
#'   Also, tools might be extended to more complicated CUSUM control chart.
#'   We leave these issues as our perpetual work.
#'
#' @docType package
NULL

#' @useDynLib attrCUSUM
#' @importFrom Rcpp evalCpp
#' @import stats
#' @import utils
NULL
