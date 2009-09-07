\name{Mdates}
 \alias{Mdates}
  \title{Determine the Location in the Sample of a Date and vice versa}

  \description{
    This function determines the year and season to which a given observation in the sample is related to,
    and the location in the sample of a given time specified by the year and the season.
  }

  \usage{
    Mdates (wts, yso)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{yso}{either a vector of length two indicating the year and season or a vector on length one
      indicating the location in the sample of an observation.}
  }

  \value{An object of class \code{\link{vdate-class}}.}

  \seealso{\code{\link{stepdate-methods}}.}

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}}

  \examples{
    ## Mdates
    data(AirPassengers)
    ## Which date is related to the 12th observation?
    out1 <- Mdates(AirPassengers, 12)
    out1
    out1@Myso
    ## Where is located the observation in 1959.2
    out2 <- Mdates(AirPassengers, c(1959,2))
    out2
    ## Beyond the sample.
    Mdates(AirPassengers, 150)
    Mdates(AirPassengers, c(1970,2))

    ## Which is the next date after the 12th observation?
    stepdate(as.vdate(AirPassengers, yso=12), step=1)
  }

\keyword{ts}


