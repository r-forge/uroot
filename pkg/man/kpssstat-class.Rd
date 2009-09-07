\name{kpssstat-class}
 \docType{class}
  \alias{kpssstat-class}
  \title{"kpssstat" Class}

  \description{This class contains information from the Kwiatkowski-Phillips-Schmidt-Shin test.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: A univariate time series. The input.}
      \item{\code{lmkpss}:}{Object of class \code{"lm"}: Auxiliar regression.}
      \item{\code{ltrunc}:}{Object of class \code{"numeric"}: Lag truncation parameter.}
      \item{\code{levelst}:}{Object of class \code{"numeric"}: Statistic for level-stationarity.}
      \item{\code{trendst}:}{Object of class \code{"numeric"}: Statistic for stationarity around a
        deterministic trend}
    }
  }

  \section{Methods}{
    \describe{
      \item{show}{\code{signature(object = "kpssstat")}: This method shows the relevant information from the
        KPSS test.}
      \item{urt.xtable}{Summarize the main information from the test and convert it in an xtable object, which
        can be printed as a LaTeX or HTML table. A range of lag truncation parameters are considered, three
        lags of order lower than that in the \code{kpssstat} object and three higher than that.}
      \item{save.xtable}{Save the information in the xtable object to a LaTeX or HTML file.}
    }
  }

  \references{
    D. Kwiatkowski, P.C.B. Phillips, P. Schmidt and Y. Shin (1992), Testing the null hypothesis of
    stationarity against the alternative of a unit root: How sure are we that economic time series have a
    unit root? \emph{Journal of Econometrics}, \bold{54}, 159-178.
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}}

  \seealso{
    \code{\link{KPSS.test}}.
  }

\keyword{classes}
