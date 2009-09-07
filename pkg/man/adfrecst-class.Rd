\name{adfrecst-class}
 \docType{class}
  \alias{adfrecst-class}
  \title{"adfrecst" Class}

  \description{This class contains information from the Augmented Dickey-Fuller unit root test computed
    recursively along subsamples of the original data.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{type}:}{Object of class \code{"character"}: how the subsamples are defined.}
      \item{\code{nsub}:}{Object of class \code{"numeric"}: the number of observations in each subsample.}
      \item{\code{itsd}:}{Object of class \code{"numeric"}: Deterministic regressors included in the
        auxiliar regression, namely intercept, trend, and/or seasonal dummies.}
      \item{\code{regvar}:}{Object of class \code{"numeric"}: Regressor variables. Not considered in this
       procedure.}
      \item{\code{selectlags}:}{Object of class \code{"list"}: Method for selecting lags and the maximum
        order considered.}
      \item{\code{recstats}:}{Object of class \code{"matrix"}: statistics in each subsample.}
      \item{\code{elaps}:}{Object of class \code{"list"}: elapsed time during computation.}
    }
  }

  \section{Methods}{
    \describe{
      \item{plot.}{\code{signature(x = "adfrecst", y = "missing").} Plot the ADF statistics along the
        subsamples.}
      \item{show.}{Show the ADF statistics in each subsample.}
    }
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \references{
    D.A. Dickey and W.A. Fuller (1981), Likelihood ratio statistics for autoregressive time series with a
    unit root. \emph{Econometrica}, \bold{49}, 1057-1071.

    W.A. Fuller (1976), Introduction to Statistical Time Series. Jonh Wiley, New York.
  }

  \seealso{
    \code{\link{ADF.rectest}}.
  }

\keyword{classes}
