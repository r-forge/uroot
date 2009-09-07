\name{CH.rectest}
 \alias{CH.rectest}
  \title{Canova-Hansen Recursive Test}

  \description{
    This function computes the Canova-Hansen statistic recursively along subsamples of the original
    data.}

  \usage{
    CH.rectest (wts, type="moving", nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL, trace=list(remain=1,
      plot=0, elaps=1))
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{type}{a character string indicating how subsamples are selected. See details.}
    \item{nsub}{the number of observations in each subsample.}    
    \item{frec}{a vector indicating the frequencies to analyse.}
    \item{f0}{a 0-1 (No-Yes) vector of length one indicating wether a first lag of the dependent variable is
      included or not in the auxiliar regression. See details.}
    \item{DetTr}{a logical argument. If TRUE a linear trend is included in the auxiliar regression.}
    \item{ltrunc}{lag truncation parameter for computing the residuals covariance matrix. By default,
      \eqn{round(s*(N/100)^0.25)}, where eqn{s} is the periodicity of the data and \eqn{N} the number of
      observations.}
    \item{trace}{a list object indicating if a trace of the iteration progress should be printed. Three
      levels of information can be printed: \code{remain}, the percentage of the whole procedure that has
      been completed; \code{plot}, a plot of the computed statistics; and \code{elaps}, how much time the
      whole procedure has consumed.}
  }

  \details{
    Elements of \code{frec} must be set equal to 0 if the season assigned to this element is not considered
    and equals to 1 for the frequencies to analyse. The position of each frequency in the vector is as
    follows: c(pi/2, pi) for quarterly series and c(pi/6, pi/3, pi/2, 2pi/3, 5pi/6, pi) for monthly series.

    Rejection of the null hypothesis implies that the analysed cycles are non-stationary.

    Three types of subsamples are considered: "backw", the statistic is computed for the last \code{nsub}
    observations and then one year backwards is added until the beginning of the sample; "forw", the
    statistic is computed for the first \code{nsub} observations and then one year forwards is added until
    the end of the sample; "moving", the statistic is computed over moving subsamples of length \code{nsub}.
  }

  \value{An object of class \code{\link{chrecst-class}}.}

  \seealso{\code{\link{CH.test}}.}

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
    ch.out1 <- CH.rectest(wts=AirPassengers, type="backw", nsub=48,
                          frec=c(1,1,1,1,1,1), f0=1, DetTr=FALSE)
    show(ch.out1)
    plot(ch.out1)
  }

\keyword{ts}
