\name{ADF.test}
 \alias{ADF.test}
  \title{Augmented Dickey-Fuller Test}

  \description{
    This function computes the augmented Dickey-Fuller statistic for testing the null hypothesis that the
    long run unit root 1 exists.
  }

  \usage{
    ADF.test (wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
  }

  \arguments{
    \item{wts}{a univariate time series object.}
    \item{itsd}{deterministic components to include in the model. Three types of regressors can be
      included: regular deterministic components, seasonal deterministic components, and any regressor
      variable previously defined by the user.

      This argument must be a vector object with the following elements: \code{c(0,0,c(0))},
      if the first and/or second elements are set equal to 1, it indicates that an intercept, and/or linear
      trend, respectively, are included. The third element is a vector indicating which seasonal
      dummies should be included. If no seasonal dummies are desired it must be set equal to zero. For
      example, \code{regular=c(1,0,c(1,2,3))} would include an intercept, no trend, and the first three
      seasonal dummies.}
    \item{regvar}{regressor variables. If none regressor variables are considered, this object must be set
      equal to zero, otherwise, the names of a matrix object previously defined should be indicated.}
    \item{selectlags}{lag selection method. A list object indicating the method to select lags, \code{mode},
      and the maximum lag considered. Available methods are \code{"aic"}, \code{"bic"}, and \code{"signf"}.
      See details. \code{Pmax} is a numeric object indicating the maximum lag order. By default, the maximum
      number of lags considered is \eqn{round(10*log10(n))}, where \eqn{n} is the number of observations.}
  }

  \details{
    The auxiliar regression is defined as,

    \eqn{\delta y_t = \rho y_{t-1} + \epsilon_t,}

    where \eqn{\delta} is the first order operator. Hence, under the null hypothesis \eqn{\rho=0} and the
    long run unit root 1 exists.

    Available methods are the following. \code{"aic"} and \code{"bic"} follows a top-down strategy based on
    the Akaike's and Schwarz's information criteria, and \code{"signf"} removes the non-significant lags at
    the 10\% level of significance until all the selected lags are significant. By default, the maximum
    number of lags considered is \eqn{round(10*log10(n))}, where \eqn{n} is the number of observations.

    It is also possible to set the argument \code{selectlags} equals to a vector, \code{mode=c(1,3,4)}, then
    those lags are directly included in the auxiliar regression and \code{Pmax} is ignored.
  }

  \value{An object of class \code{\link{adfstat-class}}.}

  \seealso{\code{\link{ADF.rectest}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \references{
    D.A. Dickey and W.A. Fuller (1981), Likelihood ratio statistics for autoregressive time series with a
    unit root. \emph{Econometrica}, \bold{49}, 1057-1071.

    W.A. Fuller (1976), Introduction to Statistical Time Series. Jonh Wiley, New York.
  }

  \examples{
    ## ADF test with constant, trend and seasonal dummies.
    data(AirPassengers)
    lairp <- log(AirPassengers)
    adf.out1 <- ADF.test(wts=lairp, itsd=c(1,1,c(1:11)),
                  regvar=0, selectlags=list(mode="bic", Pmax=12))
    adf.out1
    adf.out2 <- ADF.test(wts=lairp, itsd=c(1,1,c(1:11)),
                  regvar=0, selectlags=list(mode="signf", Pmax=NULL))
    adf.out2
  }

\keyword{ts}


