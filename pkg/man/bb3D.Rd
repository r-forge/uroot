\name{bb3D}
 \alias{bb3D}
  \title{Buys-Ballot 3D Plot}

  \description{
    Draw perspective plot of surface over the season-year plane.
  }

  \usage{
    bb3D (wts, color=TRUE, x=30, y=30)
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{color}{logical. If TRUE a coloured graphic is showed. Otherwise, grey scale.}
    \item{x, y}{angles defining the viewing direction. \code{x} gives the azimuthal direction and \code{y}
      the colatitude.}
  }

  \details{
    When the graphic is showed a window is launched as well. Coordinates of the mouse inside this window
    represent angles defining the viewing direction. The graphic rotates clicking on it with the right button
    of the mouse. For it, the package "tcltk" is used.
  }

  \seealso{
    \code{\link{bbplot}}, \code{\link{bbaplot}}, \code{\link{bbcn}}, \code{\link{image}}, and
    \code{\link{persp}}.
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## Buys-Ballot 3D plot.
    data(AirPassengers)
    bb3D(wts=log(AirPassengers))
    bb3D(wts=log(AirPassengers), x=40, y=60)
  }

\keyword{misc}
