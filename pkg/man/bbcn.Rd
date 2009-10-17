\name{bbcn}
 \alias{bbcn}
  \title{Buys-Ballot Contour Plot}

  \description{
    This function creates a grid of rectangles with colors corresponding to seasonal data and adds contour
    lines to the existing plot.
  }

  \usage{
    bbcn (wts, color=TRUE)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{color}{logical. If TRUE a coloured graphic is showed. Otherwise, grey scale.}
  }

  \seealso{
    \code{\link{bbaplot}}, \code{\link{bbaplot}}, \code{\link{bbcn}},
    \code{\link{image}}, and \code{\link{persp}}.
  }

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## Buys-Ballot contour plot.
    data(AirPassengers)
    bbcn(wts=log(AirPassengers))
  }

\keyword{misc}
