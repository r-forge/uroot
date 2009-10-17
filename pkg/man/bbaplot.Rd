\name{bbaplot}
 \alias{bbaplot}
  \title{Buys-Ballot Anual Path Plot}

  \description{
    Plot anual paths in quarterly and monthly time series.
  }

  \usage{
    bbaplot (wts, years=NULL, colour=c("SlateBlue","SeaGreen","red","magenta"))
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{years}{a vector indicating the years to plot. By default the first, last, and half the sample years
      are selected.}
    \item{colour}{colours for lines.}      
  }

  \seealso{
    \code{\link{bbplot}}, \code{\link{bb3D}}, and \code{\link{bbcn}}.
  }

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## Plot anual paths.
    data(AirPassengers)
    bbaplot(wts=AirPassengers)
    bbaplot(wts=log(AirPassengers), years=c(1949, 1951, 1958, 1960))
  }

\keyword{misc}
