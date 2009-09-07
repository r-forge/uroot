\name{vdate-class}
 \docType{class}
  \alias{vdate-class}
  \title{"vdate" Class}

  \description{This class contains the times at which a time series is observed and relates those dates to
    their location in the sample, i.e. the number of observation they are related to.}

  \section{Slots}{
    \describe{
      \item{\code{input}:}{Object of class \code{"numeric"}: either the year and season of an observation
        provided as input or the location in the sample (observation) of a date provided as the input.}
      \item{\code{output}:}{Object of class \code{"matrix"}: either the location in the sample of a date
        specified in \code{input} or the date related to the observation specified in \code{input}.
        the year and the season.}
      \item{\code{wts}:}{Object of class \code{"ts"}: the univariate time series.}
      \item{\code{ys}:}{Object of class \code{"matrix"}: the year and season of an observation.}
      \item{\code{obs}:}{Object of class \code{"matrix"}: the location in the sample of a date specified by
        the year and the season.}
      \item{\code{Myso}:}{Object of class \code{"matrix"}: a matrix containing the dates and their locations
        in the sample at which the time series is observed.}
    }
  }

  \section{Methods}{
    \describe{
      \item{as.numeric}{\code{signature(x = "vdate")}: Coerce an object of class `vdate' to a `numeric'.}
     \item{show}{\code{signature(object = "vdate")}: Report the date to which a given observation is related
       to and vice versa.}
     \item{stepdate}{\code{signature(object = "vdate")}: Returns the date `x' times ahead in the sample. If
       that date is out of sample a message is generated.}
    }
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \seealso{\code{\link{Mdates}}.}

\keyword{classes}
