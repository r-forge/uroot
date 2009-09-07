\name{plot-methods}
 \docType{methods}
  \alias{plot-methods}
  \alias{plot,adfrecst,missing-method}
  \alias{plot,kpssrecst,missing-method}
  \alias{plot,hegyrecst,missing-method}
  \alias{plot,chrecst,missing-method}  
  \title{Methods for Function plot in Package `uroot'}

  \description{
    Methods for Function plot in Package \pkg{uroot}.
  }

  \section{Methods}{
    \describe{
      \item{x = "adfrecst", y = "missing".}{Plot ADF recursive statistics computed along subsamples of the
        original data.}
      \item{x = "kpssrecst", y = "missing".}{Plot KPSS recursive statistics computed along subsamples of the
        original data.}
      \item{x = "hegyrecst", y = "missing".}{Plot HEGY recursive statistics computed along subsamples of the
        original data.}
      \item{x = "chrecst", y = "missing".}{Plot CH recursive statistics computed along subsamples of the
        original data.}
    }
  }

  \seealso{\code{\link{ADF.rectest}}, \code{\link{KPSS.rectest}}, \code{\link{HEGY.rectest}}, and
    \code{\link{CH.rectest}}.}

\keyword{methods}
