% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xzibinom.R
\name{zibinom}
\alias{dzibinom}
\alias{pzibinom}
\alias{qzibinom}
\alias{rzibinom}
\alias{zibinom}
\title{The Zero Inflated Binomial Distribution}
\usage{
dzibinom(x, rho, size, prob, log = FALSE)

pzibinom(q, rho, size, prob, lower.tail = TRUE, log.p = FALSE)

qzibinom(p, rho, size, prob, lower.tail = TRUE, log.p = FALSE)

rzibinom(n, rho, size, prob)
}
\arguments{
\item{x}{A non-negative integer-valued vector of quantiles.}

\item{q}{A numeric vector of quantiles.}

\item{p}{A vector of probabilities.}

\item{n}{Number of random values to return,
a length-one positive integer-valued vector.}

\item{rho}{A length-one vector of zero inflation parameter on [0,1].}

\item{size}{A length-one vector of number of trials (zero or more).}

\item{prob}{A length-one vector of probability of success on each trial.}

\item{log, log.p}{A length-one logical vector;
if TRUE, probabilities p are given as log(p).}

\item{lower.tail}{A length-one logical vector;
if TRUE (the default), probabilities are \eqn{P(X \le x)},
otherwise, \eqn{P(X > x).}}
}
\value{
\code{dzibinom} gives the (log) density,
   \code{pzibinom} gives the (log) distribution function,
   \code{qzibinom} gives the quantile function,
   and \code{rzibinom} generates random deviates.

   Invalid arguments rise an error.
}
\description{
Density, distribution function, quantile function and
   random generation for the zero inflated binomial distribution
   with parameters (rho, size, prob).
}
\details{
The probability mass function of X is given by
   \deqn{P(X=x) = rho I(x = 0) + (1 - rho) P(Y=x), x=0,1,2,...,}
   where Y is distributed binomial(size, prob).
}
\examples{
# Example 1: dzibinom
dzibinom(x = 0:10, rho = 0.1, size = 5, prob = 0.5)

# Example 2: pzibinom
pzibinom(q = 2, rho = 0.1, size = 5, prob = 0.5)

# Example 3: qzibinom
qzibinom(p = pzibinom(2, 0.1, 5, 0.5), rho = 0.1, size = 5, prob = 0.5)

# Example 4: rzibinom
n <- 1e+5
rho <- 0.2
size <- 5
prob <- 0.5
mean(rzibinom(n, rho, size, prob)) # Sample mean
(size * prob) * (1 - rho) # Theoretical mean
}
\seealso{
\link{Binomial} for the binomial distribution.
}

