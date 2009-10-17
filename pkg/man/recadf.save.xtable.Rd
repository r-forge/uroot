\name{recadf.save.xtable}
 \alias{recadf.save.xtable}
  \title{Save a Summary on the ADF Recursive Test to LaTeX or HTML File}

  \description{
    Save a summary on the ADF recursive test to LaTeX or HTML file. The statistic in backwards and forwards
    subsamples is reported. The arguments are the union of the arguments in \code{\link[xtable]{xtable}},
    \code{\link[xtable]{print.xtable}}, and \code{\link{ADF.rectest}} functions.
  }

  \arguments{
    recadf.save.xtable <- function(wts, nsub=72, itsd, selectlags=list(mode="signf", Pmax=NULL),
      trace=list(remain=1, plot=0, elaps=1), caption="ADF recursive test", label="Tadfrec", align="lcrlcrl",
      vsep=NULL, digits=NULL, display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE,
      table.placement="ht", caption.placement="top", latex.environments=c("center"), size=NULL,
      hline.after=NULL,...)

    \item{wts}{a \code{ts} object.}
    \item{...}{Additional arguments used in \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}},
      and \code{\link{ADF.rectest}}.}
  }

  \value{A LaTeX or HTML file.}

  \seealso{\code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}, and \code{\link{ADF.rectest}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \examples{
    ## ADF test.
    #data(AirPassengers)
    #lairp <- log(AirPassengers)
    #recadf.save.xtable(wts=lairp, itsd=c(1,1,0))
  }

\keyword{misc}


