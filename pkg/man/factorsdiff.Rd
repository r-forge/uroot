\name{factorsdiff}
 \alias{factorsdiff}
  \title{Factor Seasonal Differencing Filter}

  \description{
    Factor seasonal differencing filter in quarterly and monthly time series.
  }

  \usage{
    factorsdiff (wts, factors)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{factors}{the factors of the seasonal differencing filter to apply to the data.}
  }

  \details{
    \code{fators} is a 0-1 vector of length \eqn{S/2+1}, where \eqn{S} is the periodicity of the time series.
    The frequencies are ordered in the vector as follows: c(zero, pi, pi/2) for quarterly series and c(zero,
    pi, pi/2, 2pi/3, pi/3, 5pi/6, pi/6) for monthly series.

    A zero in the element \code{i} of the vector indicates that the cycle associated to the corresponding
    frequency will not be filtered. The cycles related to frequencies that are set equal to 1 are filtered.
  }

  \value{A list containing: Data of the filtered series, coefficients and lags of the polynomial that
    configure the filter.}

  \seealso{\code{\link{plotcycles}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \examples{
    ## Filter seasonal frequencies.
    data(AirPassengers)
    factorsdiff(wts=AirPassengers, factors=c(0,1,1,1,1,1,1))
  }

\keyword{misc}
