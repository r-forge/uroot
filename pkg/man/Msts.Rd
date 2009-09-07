\name{Msts}
 \alias{Msts}
  \title{Matrix with Seasonal Data}

  \description{
    Returns a matrix with the seaonal data by columns. For internal use.
  }

  \usage{
    Msts (wts)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
  }

  \author{Javier L�pez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio D�az-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## Buys-Ballot contour plot.
    data(AirPassengers)
    Msts(wts=log(AirPassengers))
  }

\keyword{misc}