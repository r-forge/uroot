\name{adfstat-class}
 \docType{class}
  \alias{adfstat-class}
  \title{"adfstat" Class}

  \description{This class contains information from the Augmented Dickey-Fuller unit root test.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{itsd}:}{Object of class \code{"numeric"}: Deterministic regressors included in the
        auxiliar regression, namely intercept, trend, and/or seasonal dummies.}
      \item{\code{regvar}:}{Object of class \code{"maybeRegvar"}: Regressor variables included in auxiliar
        regression.}
      \item{\code{selectlags}:}{Object of class \code{"list"}: Method for selecting lags and the maximum
        order considered.}
      \item{\code{regvarcoefs}:}{Object of class \code{"maybeRegvar"}: Regressor variables estimates.}
      \item{\code{lagsorder}:}{Object of class \code{"maybeLags"}: Selected lags order.}
      \item{\code{lagcoefs}:}{Object of class \code{"maybeLags"}: Lags estimates.}
      \item{\code{res}:}{Object of class \code{"numeric"}: Residuals from the auxiliar regression.}
      \item{\code{lmadf}:}{Object of class \code{"lm"}: Auxiliar regression fitted.}
      \item{\code{stat}:}{Object of class \code{"matrix"}: ADF statistic.}
    }
  }

  \section{Methods}{
    \describe{
      \item{show.}{Show the relevant information from the ADF test.}
      \item{summary.}{Summarize ADF test regression.}
      \item{urt.xtable.}{Summarize the main information from the test and convert it in an xtable object,
        which can be printed as a LaTeX or HTML table.}
      \item{save.xtable.}{Save the information in the xtable object to a LaTeX or HTML file.}
    }
  }

  \references{
    D.A. Dickey and W.A. Fuller (1981), Likelihood ratio statistics for autoregressive time series with a
    unit root. \emph{Econometrica}, \bold{49}, 1057-1071.

    W.A. Fuller (1976), Introduction to Statistical Time Series. Jonh Wiley, New York.
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}}

  \seealso{
    \code{\link{ADF.test}}.
  }

\keyword{classes}
