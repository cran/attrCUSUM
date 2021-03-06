% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attrCUSUM_getxxx_pois.R
\name{getAve_Poisson}
\alias{getAve_Poisson}
\alias{getAve_pois}
\alias{getAve_zipois}
\title{Compute ATSs and Other Informations on (Zero-Inflated) Poisson CUSUM Chart}
\usage{
getAve_pois(lambda, refv, contl, c.zero = 0, warnl = 0, ds = 1,
  dl = NULL, di = NULL, maxndec = 7L, maxnumsubI = 6000L)

getAve_zipois(rho, lambda, refv, contl, c.zero = 0, warnl = 0, ds = 1,
  dl = NULL, di = NULL, maxndec = 7L, maxnumsubI = 6000L)
}
\arguments{
\item{rho}{A length-one numeric vector of zero-inflated parameter in [0,1).}

\item{lambda}{A length-one positive numeric vector of mean.}

\item{refv}{A reference value of CUSUM statistic.}

\item{contl}{A control limit in CUSUM control scheme.}

\item{c.zero}{An initial state of CUSUM statistic (default is 0).}

\item{warnl}{A warning limit in VSI control scheme (default is 0).}

\item{ds}{The shorter sampling interval in VSI CUSUM control scheme (default is 1).}

\item{dl}{The longer sampling interval in VSI CUSUM control scheme.}

\item{di}{The sampling interval at initial state in VSI CUSUM control scheme.}

\item{maxndec}{The maximum number of decimal places
of \code{refv} (\eqn{\le} 7L).}

\item{maxnumsubI}{The maximum number of sub-intervals in [100,6000] to be used in
applying the Markov chain approach.}
}
\value{
A list from \code{\link{getAve}}.
  See \code{\link{getAve}} for more information.
}
\description{
Computation of ANSSs, ATSs and other related informations on
  (zero-inflated) Poisson CUSUM chart.
}
\details{
\code{\link{getAve_pois}} and \code{\link{getAve_zipois}} are
  wrapper functions of \code{\link{getAve}}
  for (zero-inflated) Poisson CUSUM chart.
}
\examples{
# Example 1: Reproduction of results from White et al. (1997)
tbl <- list()
arglist <- list(list(mu.a = 1, k = 2, h = 2),
                list(mu.a = 4, k = 5, h = 8),
                list(mu.a = 8, k = 10, h = 10),
                list(mu.a = 12, k = 15, h = 11))
deltas <- seq(0, 2.5, 0.5)
for(i in seq_along(arglist)) {
  argument <- arglist[[i]]
  arl <- numeric(length(deltas))
  for(j in seq_along(deltas)) {
    std.a <- sqrt(argument$mu.a)
    arl[j] <- getAve_pois(lambda = argument$mu.a + (std.a * deltas[j]),
                          refv = argument$k,
                          contl = argument$h)$ANSS
  }
  tbl[[i]] <- round(arl, 2)
}
tbl <- data.frame(tbl)
colnames(tbl) <- c("CUSUM(2,2)", "CUSUM(5,8)", "CUSUM(10,10)", "CUSUM(15,11)")
rownames(tbl) <- as.character(deltas)
cat("colnames stand for CUSUM(k,h)\\n",
    "rownames stand for delta\\n", sep = "")
tbl

# Example 2: ANSS profiles (h in seq(20L))
mu.a <-  4
k  <-  5
h <- seq(20)
ANSSs <- numeric(20)
for(i in seq(ANSSs)) {
  ANSSs[i] <- getAve_pois(lambda = mu.a, refv = k, contl = h[i], c.zero = 0,
                          warnl = 1, ds = 1)$ANSS
}
ANSSs

}
\references{
White et al. (1997).
   POISSON CUSUM VERSUS c CHART FOR DEFECT DATA,
   \emph{Quality Engineering}, \strong{9}:4, 673-679.
}
\seealso{
\code{\link{getAve}}, \link{getAve_binomial}.
}

