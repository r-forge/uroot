\name{reckpss.save.xtable}
 \alias{reckpss.save.xtable}
  \title{Save a Summary on the KPSS Recursive Test to LaTeX or HTML File}

  \description{
    Save a summary on the KPSS recursive test to LaTeX or HTML file. The statistic in backwards and forwards
    subsamples is reported. The arguments are the union of the arguments in \code{\link[xtable]{xtable}},
    \code{\link[xtable]{print.xtable}}, and \code{\link{KPSS.rectest}} functions.
  }

  \arguments{
    reckpss.save.xtable <- function(wts, nsub=48, testtype="level", ltrunc=NULL,
      trace=list(remain=1, plot=0, elaps=1), caption="KPSS recursive test", label="Tkpssrec",
      align="lcrlcrl", vsep=NULL, digits=NULL, display=NULL, type="latex", file=NULL, append=FALSE,
      floating=TRUE, table.placement="ht", caption.placement="top", latex.environments=c("center"),
      size=NULL, hline.after=NULL,...)

    \item{wts}{a \code{ts} object.}
    \item{...}{Additional arguments used in \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}},
      and \code{\link{KPSS.rectest}}.}
  }

  \value{A LaTeX or HTML file.}

  \seealso{\code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}, and \code{\link{KPSS.rectest}}.}

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## KPSS test.
    #data(AirPassengers)
    #lairp <- log(AirPassengers)
    #reckpss.save.xtable(wts=lairp)
  }

\keyword{misc}


