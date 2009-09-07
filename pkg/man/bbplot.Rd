\name{bbplot}
 \alias{bbplot}
  \title{Buys-Ballot Plot for Seasonal Time Series}

  \description{
    Plot seasonal paths in quarterly and monthly time series.
  }

  \usage{
    bbplot (wts, colour=c("SlateBlue","SeaGreen","red","magenta"))
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{colour}{colours for lines.}
  }

  \seealso{
    \code{\link{bbaplot}}, \code{\link{bb3D}}, and \code{\link{bbcn}}.
  }

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## Plot seasonal paths in the original data and the logarithms of the first differences.
    data(AirPassengers)
    bbplot(wts=AirPassengers)
    bbplot(wts=diff(log(AirPassengers), lag=1))
  }

\keyword{misc}
