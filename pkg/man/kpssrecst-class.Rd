\name{kpssrecst-class}
 \docType{class}
  \alias{kpssrecst-class}
  \title{"kpssrecst" Class}

  \description{This class contains information from the Kwiatkowski-Phillips-Schmidt-Shin unit root test
    computed recursively along subsamples of the original data.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{type}:}{Object of class \code{"character"}: how the subsamples are defined.}
      \item{\code{nsub}:}{Object of class \code{"numeric"}: the number of observations in each subsample.}
      \item{ltrunc}{lag truncation parameter. By default, \eqn{3*sqrt(length(wts))/13}}
      \item{\code{recstats}:}{Object of class \code{"matrix"}: statistics in each subsample.}
      \item{\code{elaps}:}{Object of class \code{"list"}: elapsed time during computation.}
    }
  }

  \section{Methods}{
    \describe{
      \item{plot.}{\code{signature(x = "kpssrecst", y = "missing").}: Plot the KPSS statistics along the
        subsamples.}
      \item{show}{\code{signature(object = "kpssrecst")}: Show the KPSS statistics in each subsample.}
    }
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \references{
    D. Kwiatkowski, P.C.B. Phillips, P. Schmidt and Y. Shin (1992), Testing the null hypothesis of
    stationarity against the alternative of a unit root: How sure are we that economic time series have a
    unit root? \emph{Journal of Econometrics}, \bold{54}, 159-178.
  }

  \seealso{
    \code{\link{KPSS.rectest}}.
  }

\keyword{classes}
