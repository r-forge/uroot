\name{rechegy.save.xtable}
 \alias{rechegy.save.xtable}
  \title{Save a Summary on the HEGY Recursive Test to LaTeX or HTML File}

  \description{
    Save a summary on the HEGY recursive test to LaTeX or HTML file. The statistic in backwards and forwards
    subsamples is reported. The arguments are the union of the arguments in \code{\link[xtable]{xtable}},
    \code{\link[xtable]{print.xtable}}, and \code{\link{HEGY.rectest}} functions.
  }

  \arguments{
    rechegy.save.xtable <- function(wts, nsub=72, itsd, refstat, selectlags=list(mode="signf", Pmax=NULL),
      trace=list(remain=1, plot=0, elaps=1), caption="HEGY recursive test", label="Thegyrec",
      align="lcrlcrl", vsep=NULL, digits=NULL, display=NULL, type="latex", file=NULL, append=FALSE,
      floating=TRUE, table.placement="ht", caption.placement="top", latex.environments=c("center"),
      size=NULL, hline.after=NULL,...)

    \item{wts}{a \code{ts} object.}
    \item{refstat}{a character string indicating the statistic to be reported. In quarterly series:
      c("tpi1","tpi2","Fpi3:4") and in monthly time series also"Fpi5:6","Fpi7:8","Fpi9:10","Fpi11:12")}
    \item{...}{Additional arguments used in \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}},
      and \code{\link{HEGY.rectest}}.}
  }

  \value{A LaTeX or HTML file.}

  \seealso{\code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}, and \code{\link{HEGY.rectest}}.}

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## HEGY test.
    #data(AirPassengers)
    #lairp <- log(AirPassengers)
    #rechegy.save.xtable(wts=lairp, itsd=c(1,1,0), refstat="tpi1")
  }

\keyword{misc}


