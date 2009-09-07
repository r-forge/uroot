\name{stepdate-methods}
 \docType{methods}
  \alias{stepdate-methods}
  \alias{stepdate,ANY-method}
  \alias{stepdate,vdate-method}
  \title{Methods for Function stepdate in Package `uroot'}

  \section{usage}{
    stepdate (object, step)
  }

  \description{
    This method returns the date `x' times ahead in the sample. If that date is out of sample a message is generated.
  }

  \section{arguments}{
  \describe{
      \item{object}{an object of class \code{\link{vdate-class}}}
      \item{step}{the number of dates ahead to move in the sample times}
    }
  }

  \value{An object of class \code{\link{vdate-class}}.}

  \section{Methods}{
  \describe{
    \item{ANY}{ None default method.}
    \item{vdate}{ Returns the date `x' times ahead in the sample. If that date is out of sample a message is generated.}
    }
  }

  \seealso{\code{\link{Mdates}}.}

\keyword{methods}
