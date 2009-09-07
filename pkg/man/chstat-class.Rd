\name{chstat-class}
 \docType{class}
  \alias{chstat-class}
  \title{"chstat" Class}

  \description{This class contains information from the Canova-Hansen test.}

  \section{Slots}{
    \describe{
      \item{\code{wts}:}{Object of class \code{"ts"}: Original time series.}
      \item{\code{frec}:}{Object of class \code{"numeric"}: Analyzed frecuencies.}
      \item{\code{DetTr}:}{Object of class \code{"logical"}: Has a linear trend benn included.}
      \item{\code{ltrunc}:}{Object of class \code{"numeric"}: Lag truncation parameter}
      \item{\code{Fhat}:}{Object of class \code{"matrix"}: Object for internal use.}
      \item{\code{Omfhat}:}{Object of class \code{"matrix"}: Object for internal use.}
      \item{\code{MA}:}{Object of class \code{"matrix"}: Object for internal use.}
      \item{\code{stat}:}{Object of class \code{"numeric"}: CH statistic.}
    }
  }

  \section{Methods}{
    \describe{
      \item{show.}{\code{signature(object = "chstat")}: This method shows the relevant information from the
        CH test.}
      \item{urt.xtable.}{Summarize the main information from the test and convert it in an xtable object,
        which can be printed as a LaTeX or HTML table. }
      \item{save.xtable.}{Save the information in the xtable object to a LaTeX or HTML file.}
    }
  }

  \references{
    F. Canova and B.E. Hansen (1995), Are seasonal patterns constant over time? A test for seasonal
    stability. \emph{Journal of Business and Economic Statistics}, \bold{13}, 237-252.
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}}

  \seealso{
    \code{\link{CH.test}}.
  }

\keyword{classes}
