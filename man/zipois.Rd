% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xzipois.R
\name{zipois}
\alias{dzipois}
\alias{pzipois}
\alias{qzipois}
\alias{rzipois}
\alias{zipois}
\title{The Zero Inflated Poisson Distribution}
\usage{
dzipois(x, rho, lambda, log = FALSE)

pzipois(q, rho, lambda, lower.tail = TRUE, log.p = FALSE)

qzipois(p, rho, lambda, lower.tail = TRUE, log.p = FALSE)

rzipois(n, rho, lambda)
}
\arguments{
\item{x}{A non-negative integer-valued vector of quantiles.}

\item{q}{A numeric vector of quantiles.}

\item{p}{A vector of probabilities.}

\item{n}{Number of random values to return,
a length-one positive integer-valued vector.}

\item{rho}{A length-one vector of zero inflation parameter on [0,1].}

\item{lambda}{A length-one vector of positive means.}

\item{log, log.p}{A length-one logical vector;
if TRUE, probabilities p are given as log(p).}

\item{lower.tail}{A length-one logical vector;
if TRUE (the default), probabilities are \eqn{P(X \le x)},
otherwise, \eqn{P(X > x).}}
}
\value{
\code{dzipois} gives the (log) density,
   \code{pzipois} gives the (log) distribution function,
   \code{qzipois} gives the quantile function,
   and \code{rzipois} generates random deviates.

   Invalid arguments rise an error with a helpful message.
}
\description{
Density, distribution function, quantile function and
   random generation for the zero inflated Poisson distribution
   with parameters (rho, lambda).
}
\details{
The probability mass function of X is given by
   \deqn{P(X=x) = rho I(x = 0) + (1 - rho) P(Y=x), x=0,1,2,...,}
   where Y is distributed Poisson(lambda).
}
\examples{
# Example 1: dzipois
dzipois(x = 0:10, rho = 0.1, lambda = 5)

# Example 2: pzipois
pzipois(q = 2, rho = 0.1, lambda = 5)

# Example 3: qzipois
qzipois(p = pzipois(2, 0.1, 5), rho = 0.1, lambda = 5)

# Example 4: rzipois
n <- 1e+5
rho <- 0.2
lambda <- 5
mean(rzipois(n, rho, lambda)) # Sample mean
lambda * (1 - rho) # Theoretical mean
}
\references{
Lambert, D. (1992).
   Zero-Inflated Poisson Regression,
   with an Application to Defects in Manufacturing,
   \emph{Technometrics}, \strong{34}(1), 1-14.
}
\seealso{
\link{Poisson} for the Poisson distribution.
}

