\name{CH.test}
 \alias{CH.test}
  \title{Canova-Hansen Test}

  \description{
    This function computes the Canova-Hansen statistic for testing the null hypothesis of stationary seasonal
    cycles against the alternative of seasonal unit roots.}

  \usage{
    CH.test (wts, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{frec}{a vector indicating the cycles to analyse. By default, all seasonal cycles are tested.}
    \item{f0}{a 0-1 (No-Yes) vector of length one indicating wether a first lag of the dependent variable is
      included or not in the auxiliar regression. See details.}
    \item{DetTr}{a logical argument. If TRUE a linear trend is included in the auxiliar regression.}
    \item{ltrunc}{lag truncation parameter for computing the residuals covariance matrix. By default,
      \eqn{round(s*(N/100)^0.25)}, where eqn{s} is the periodicity of the data and \eqn{N} the number of
      observations.}
  }

  \details{
    Elements of \code{frec} must be set equal to 0 if the season assigned to this element is not considered
    and equals to 1 for the frequencies to analyse. The position of each frequency in the vector is as
    follows: c(pi/2, pi) for quarterly series and c(pi/6, pi/3, pi/2, 2pi/3, 5pi/6, pi) for monthly series.
  }

  \value{An object of class \code{\link{chstat-class}}.}

  \seealso{\code{\link{CH.rectest}}.}

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \references{
    F. Canova and B.E. Hansen (1995), Are seasonal patterns constant over time? A test for seasonal
    stability. \emph{Journal of Business and Economic Statistics}, \bold{13}, 237-252.
  }

  \examples{
    ## CH test
    data(AirPassengers)
    ## Test for stationary cycles at all seasonal frequencies,
    ## including a first order lag and but not a linear trend.
    ch.out1 <- CH.test(wts=AirPassengers, frec=c(1,1,1,1,1,1), f0=1, DetTr=FALSE)
    ch.out1
    ## Test for stationary seasonal cycles at frequencies +i and -i,
    ## including a first order lag and but not a linear trend.
    ch.out2 <- CH.test(wts=AirPassengers, frec=c(0,0,0,0,0,1), f0=1, DetTr=FALSE)
    ch.out2
  }

\keyword{ts}
