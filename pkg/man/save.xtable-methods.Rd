\name{save.xtable-methods}
 \docType{methods}
  \alias{save.xtable-methods}
  \alias{save.xtable,adfstat-method}
  \alias{save.xtable,hegystat-method}
  \alias{save.xtable,kpssstat-method}
  \alias{save.xtable,chstat-method}
  \title{ Methods for Function save.xtable in Package `uroot'}

  \description{
    Methods for function \code{save.xtable} in package \pkg{uroot}
  }

  \section{Methods}{
    \describe{
      \item{x = "adfstat".}{Save the output of the \code{ADF.test} to a LaTeX or HTML file.}
      \item{x = "kpssstat".}{Save the output of the \code{KPSS.test} to a LaTeX or HTML file. A range of lag
        truncation parameters are considered, three lags of
        order lower than that in the \code{kpssstat} object and three higher than that.}
      \item{x = "hegystat".}{Save the output of the \code{HEGY.test} to a LaTeX or HTML file.}
      \item{x = "chstat".}{Save the output of the \code{CH.test} to a LaTeX or HTML file.}
      }
  }

  \seealso{\code{\link[xtable]{xtable}},
    \code{\link{ADF.test}}, \code{\link{KPSS.test}}, \code{\link{HEGY.test}}, and
    \code{\link{CH.test}}.}

\keyword{methods}
