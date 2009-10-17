\name{hegystat-class}
  \docType{class}
   \alias{hegystat-class}
   \title{"hegystat" Class}

  \description{This class contains information from the Hylleberg-Engle-Granger-Yoo test.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{itsd}:}{Object of class \code{"numeric"}: Deterministic regressors included in the
        auxiliar regression, namely intercept, trend, and/or seasonal dummies.}
      \item{\code{regvar}:}{Object of class \code{"maybeRegvar"}: Regressor variables included in auxiliar
        regression.}
      \item{\code{hegyreg}:}{Object of class \code{"matrix"}: HEGY regressor variable, i.e. first order lag of
        the original data.}
      \item{\code{selectlags}:}{Object of class \code{"list"}: Method for selecting lags and the maximum
        order considered.}
      \item{\code{regvarcoefs}:}{Object of class \code{"maybeRegvar"}: Regressor variables estimates.}
      \item{\code{hegycoefs}:}{Object of class \code{"maybeRegvar"}: Hegy regressors estimates.}
      \item{\code{lagsorder}:}{Object of class \code{"maybeLags"}: Selected lags order.}
      \item{\code{lagcoefs}:}{Object of class \code{"maybeLags"}: Lags estimates.}
      \item{\code{res}:}{Object of class \code{"numeric"}: Residuals from the auxiliar regression.}
      \item{\code{lmhegy}:}{Object of class \code{"lm"}: Auxiliar regression fitted.}
      \item{\code{stats}:}{Object of class \code{"matrix"}: HEGY statistics.}
    }
  }

  \section{Methods}{
    \describe{
      \item{show}{\code{signature(object = "hegystat")}. Show the relevant information from the
        HEGY test.}
      \item{summary.}{Summarize HEGY test regression.}
      \item{urt.xtable.}{Summarize the main information from the test and convert it in an xtable object,
        which can be printed as a LaTeX or HTML table.}
      \item{save.xtable.}{Save the information in the xtable object to a LaTeX or HTML file.}
    }
  }

  \references{
    S. Hylleberg, R. Engle, C. Granger and B. Yoo (1990), Seasonal integration and cointegration.
    \emph{Journal of Econometrics}, \bold{44}, 215-238.

    J. Beaulieu and J. Miron (1993), Seasonal unit roots in aggregate U.S. data.
    \emph{Journal of Econometrics}, \bold{54}, 305-328.

    P.H. Franses (1990), Testing for seasonal unit roots in monthly data, Technical Report 9032, Econometric
    Institute.
  }

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \seealso{
    \code{\link{HEGY.test}}.
  }

\keyword{classes}
