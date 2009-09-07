\name{chrecst-class}
 \docType{class}
  \alias{chrecst-class}
  \title{"chrecst" Class}

  \description{This class contains information from the Canova-Hansen unit root test computed recursively
    along subsamples of the original data.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{type}:}{Object of class \code{"character"}: how the subsamples are defined.}
      \item{\code{nsub}:}{Object of class \code{"numeric"}: the number of observations in each subsample.}
      \item{frec}{a vector indicating the frequencies to analyse.}
      \item{f0}{a 0-1 (No-Yes) vector of length one indicating wether a first lag of the dependent variable is
        included or not in the auxiliar regression. See details.}
      \item{DetTr}{a logical argument. If TRUE a linear trend is included in the auxiliar regression.}
      \item{ltrunc}{lag truncation parameter. By default, \eqn{3*sqrt(length(wts))/13}}
      \item{\code{recstats}:}{Object of class \code{"matrix"}: statistics in each subsample.}
      \item{\code{elaps}:}{Object of class \code{"list"}: elapsed time during computation.}
    }
  }

  \section{Methods}{
    \describe{
      \item{plot.}{\code{signature(x = "chrecst", y = "missing")}: Plot the CH statistics along the
        subsamples.}
      \item{show.}{Show the CH statistics in each subsample.}
    }
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \references{
    F. Canova and B.E. Hansen (1995), Are seasonal patterns constant over time? A test for seasonal
    stability. \emph{Journal of Business and Economic Statistics}, \bold{13}, 237-252.
  }

  \seealso{
    \code{\link{CH.rectest}}.
  }

\keyword{classes}
