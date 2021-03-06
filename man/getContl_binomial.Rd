% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attrCUSUM_getxxx_binom.R
\name{getContl_binomial}
\alias{getContl_binom}
\alias{getContl_binomial}
\alias{getContl_zibinom}
\title{Compute Control Limits for FSI binomial CUSUM Control Chart}
\usage{
getContl_binom(size, prob, anss.target = 370.4, refv, c.zero = 0,
  maxndec = 7L, maxnumsubI = 6000L)

getContl_zibinom(rho, size, prob, anss.target = 370.4, refv, c.zero = 0,
  maxndec = 7L, maxnumsubI = 6000L)
}
\arguments{
\item{rho}{A length-one numeric vector of zero-inflated parameter in [0,1).}

\item{size}{A length-one non-negative integer-valued vector
of number of trials.}

\item{prob}{A length-one numeric vector of probability of success in [0,1].}

\item{anss.target}{A predetermined in-control ANSS/ARL (\eqn{\le}50000L),
default is 370.4.}

\item{refv}{A reference value of CUSUM statistic.}

\item{c.zero}{An initial state of CUSUM statistic (default is 0).}

\item{maxndec}{The maximum number of decimal places
of \code{refv} (\eqn{\le} 7L).}

\item{maxnumsubI}{The maximum number of sub-intervals in [100,6000] to be used in
applying the Markov chain approach.}
}
\value{
A list from \code{\link{getContl}}.
  See \code{\link{getContl}} for more information.
}
\description{
Computation of suitable control limits
   for (zero-inflated) FSI binomial CUSUM control chart based on
   specified reference value and
   in-control ANSS/ARL.
}
\details{
\code{\link{getContl_binom}} and \code{\link{getContl_zibinom}} are
  wrapper functions of \code{\link{getContl}}
  (zero-inflated) for binomial distribution.
}
\examples{
# Example 1: zero-inflated binomial distribution
getContl_zibinom(rho = 0.9, size = 100, prob = 0.01,
                 anss.target = 370.4, refv = 0.26, c.zero = 0)
}
\references{
Rakitzis et al. (2016).
   CUSUM Control Charts for the Monitoring of Zero-inflated Binomial Processes,
   \emph{Quality and Reliability Engineering International},
   \strong{32}, 465-483.
}
\seealso{
\code{\link{getContl}}, \link{getContl_Poisson}.
}

