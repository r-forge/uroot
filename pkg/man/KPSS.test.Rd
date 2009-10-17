\name{KPSS.test}
 \alias{KPSS.test}
  \title{Kwiatkowski-Phillips-Schmidt-Shin Test}

  \description{
    This function computes the Kwiatkowski-Phillips-Schmidt-Shin test statistic for examining the null
    hypothesis that a given series is level-stationary, or stationary around a deterministic trend against the
    alternative that the series is first difference stationary.
   }

   \usage{
     KPSS.test (wts, ltrunc)
   }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{ltrunc}{lag truncation parameter. By default, \eqn{3*sqrt(length(wts))/13}}
  }

  \details{
    Lag truncation parameter indicates the number of autocovariances considered different from zero for
    estimating the variance of the residuals. According to the source paper cited below, the lag truncation
    parameter may be chosen either as \eqn{integer[4(T/100)^{1/4}]} or \eqn{integer[12(T/100)^{1/4}]}, as
    well as \code{l} =0.

    Rejection of the null hypothesis implies that the long term frequency contains a unit root.
  }

  \value{An object of class \code{\link{kpssstat-class}}.}

  \seealso{\code{\link{KPSS.rectest}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \references{
    D. Kwiatkowski, P.C.B. Phillips, P. Schmidt and Y. Shin (1992), Testing the null hypothesis of
    stationarity against the alternative of a unit root: How sure are we that economic time series have a
    unit root? \emph{Journal of Econometrics}, \bold{54}, 159-178.
  }

  \examples{
    ## KPSS test
    data(AirPassengers)
    kpss.out1 <- KPSS.test(wts=AirPassengers, ltrunc=1)
    kpss.out1
    kpss.out2 <- KPSS.test(wts=AirPassengers, ltrunc=2)
    kpss.out2
    kpss.out3 <- KPSS.test(wts=AirPassengers, ltrunc=4)
    kpss.out3
  }

\keyword{ts}
