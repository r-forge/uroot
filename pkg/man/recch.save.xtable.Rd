\name{recch.save.xtable}
 \alias{recch.save.xtable}
  \title{Save a Summary on the CH Recursive Test to LaTeX or HTML File}

  \description{
    Save a summary on the CH recursive test to LaTeX or HTML file. The statistic in backwards and forwards
    subsamples is reported. The arguments are the union of the arguments in \code{\link[xtable]{xtable}},
    \code{\link[xtable]{print.xtable}}, and \code{\link{CH.rectest}} functions.
  }

  \arguments{
    recch.save.xtable <- function(wts, nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL,
      trace=list(remain=1, plot=0, elaps=1), caption="CH recursive test", label="Tchrec", align="lcrlcrl",
      vsep=NULL, digits=NULL, display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE,
      table.placement="ht", caption.placement="top", latex.environments=c("center"), size=NULL,
      hline.after=NULL,...)

    \item{wts}{a \code{ts} object.}
    \item{...}{Additional arguments used in \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}},
      and \code{\link{CH.rectest}}.}
  }

  \value{A LaTeX or HTML file.}

  \seealso{\code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}, and \code{\link{CH.rectest}}.}

  \author{Javier López-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Díaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## CH test.
    #data(AirPassengers)
    #lairp <- log(AirPassengers)
    #recch.save.xtable(wts=lairp)
  }

\keyword{misc}


