\name{urt.xtable-methods}
 \docType{methods}
  \alias{urt.xtable-methods}
  \alias{urt.xtable,adfstat-method}
  \alias{urt.xtable,hegystat-method}
  \alias{urt.xtable,kpssstat-method}
  \alias{urt.xtable,chstat-method}
  \title{ Methods for Function urt.xtable in Package `uroot'}

  \description{
    Methods for function \code{urt.xtable} in package \pkg{uroot}
  }

  \section{Methods}{
    \describe{
      \item{x = "adfstat".}{Convert the output of the \code{ADF.test} into an xtable object, which can be
        printed as a LaTeX or HTML table.}
      \item{x = "kpssstat".}{Convert the output of the \code{KPSS.test} into an xtable object, which can be
        printed as a LaTeX or HTML table. A range of lag truncation parameters are considered, three lags of
        order lower than that in the \code{kpssstat} object and three higher than that.}
      \item{x = "hegystat".}{Convert the output of the \code{HEGY.test} into an xtable object, which can be
        printed as a LaTeX or HTML table.}
      \item{x = "chstat".}{Convert the output of the \code{CH.test} into an xtable object, which can be
        printed as a LaTeX or HTML table.}
      }
  }

  \seealso{\code{\link[xtable]{xtable}},
    \code{\link{ADF.test}}, \code{\link{KPSS.test}}, \code{\link{HEGY.test}}, and
    \code{\link{CH.test}}.}

\keyword{methods}
