\name{KPSS.rectest}
 \alias{KPSS.rectest}
  \title{Kwiatkowski-Phillips-Schmidt-Shin Recursive Test}

  \description{
    This function computes the Kwiatkowski-Phillips-Schmidt-Shin test statistic recursively along subsamples
    of the original data.
  }

   \usage{
     KPSS.rectest (wts, type="moving", nsub=48, ltrunc, trace=list(remain=1, plot=0, elaps=1))
   }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{type}{a character string indicating how subsamples are selected. See details.}
    \item{nsub}{the number of observations in each subsample.}    
    \item{ltrunc}{lag truncation parameter. By default, \eqn{3*sqrt(length(wts))/13}}
    \item{trace}{a list object indicating if a trace of the iteration progress should be printed. Three
      levels of information can be printed: \code{remain}, the percentage of the whole procedure that has
      been completed; \code{plot}, a plot of the computed statistics; and \code{elaps}, how much time the
      whole procedure has consumed.}
  }

  \details{
    Lag truncation parameter indicates the number of autocovariances considered different from zero for
    estimating the variance of the residuals. According to the source paper cited below, the lag truncation
    parameter may be chosen either as \eqn{integer[4(T/100)^{1/4}]} or \eqn{integer[12(T/100)^{1/4}]}, as
    well as \code{l} =0.

    Rejection of the null hypothesis implies that the long term frequency contains a unit root.

    Three types of subsamples are considered: "backw", the statistic is computed for the last \code{nsub}
    observations and then one year backwards is added until the beginning of the sample; "forw", the
    statistic is computed for the first \code{nsub} observations and then one year forwards is added until
    the end of the sample; "moving", the statistic is computed over moving subsamples of length \code{nsub}.  
  }

  \value{An object of class \code{\link{kpssrecst-class}}.}

  \seealso{\code{\link{KPSS.test}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \references{
    D. Kwiatkowski, P.C.B. Phillips, P. Schmidt and Y. Shin (1992), Testing the null hypothesis of
    stationarity against the alternative of a unit root: How sure are we that economic time series have a
    unit root? \emph{Journal of Econometrics}, \bold{54}, 159-178.
  }

  \examples{
    ## KPSS recursive test
    data(AirPassengers)
    kpss.out <- KPSS.rectest(wts=AirPassengers, type="backw", nsub=48,
      ltrunc=2, trace=list(remain=1, plot=0, elaps=1))
    show(kpss.out)
    plot(kpss.out)
  }

\keyword{ts}
