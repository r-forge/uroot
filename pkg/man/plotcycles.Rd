\name{plotcycles}
 \alias{plotcycles}
  \title{Plot Long-run and Seasonal Cycles}

  \description{
    Plot long-run and seasonal cycles in a quarterly or monthly time series based on the seasonal
    differencing filter decomposition.
  }

  \usage{
    plotcycles (wts)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
  }

  \seealso{\code{\link{factorsdiff}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}.}

  \examples{
    ## Plot cycles in a minthly time series.
    data(AirPassengers)
    lairp <- log(AirPassengers)
    plotcycles(lairp)
  }

\keyword{misc}


