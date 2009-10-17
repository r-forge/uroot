\name{HEGY.test}
 \alias{HEGY.test}
  \title{Hylleberg-Engle-Granger-Yoo Test}

  \description{
    This function computes the Hylleberg-Engle-Granger-Yoo statistics for testing the null hypothesis that
    long run and/or seasonal unit roots exists.
  }

  \usage{
    HEGY.test (wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
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
    Available methods are the following. \code{"aic"} and \code{"bic"} follows a top-down strategy based on
    the Akaike's and Schwarz's information criteria, and \code{"signf"} removes the non-significant lags at
    the 10\% level of significance until all the selected lags are significant. By default, the maximum
    number of lags considered is \eqn{round(10*log10(n))}, where \eqn{n} is the number of observations.

    It is also possible to set the argument \code{selectlags} equals to a vector, \code{mode=c(1,3,4)}, then
    those lags are directly included in the auxiliar regression and \code{Pmax} is ignored.

    The statistics \eqn{t_1}, and \eqn{t_2}, test for a unit root at cycles of frequencies zero, and
    \eqn{\pi}, respectively; \eqn{t_3}, and \eqn{t_4} are related to cycles of frequency \eqn{\pi/2};
    \eqn{t_5} and \eqn{t_6} to cycles of frequency \eqn{2\pi/3}, \eqn{t_7} and \eqn{t_8} to cycles of
    frequency \eqn{\pi/3}; \eqn{t_9} and \eqn{t_10} to cycles of frequency \eqn{5\pi/6}; \eqn{t_11} and
    \eqn{t_12} to cycles of frequency \eqn{\pi/6}, and the corresponding alias frequencies in each case.
    Similar notation is used with the \eqn{F-}statistics, in this way, \eqn{F_3:4} tests for a unit root at
    cycles of frequenciency \eqn{\pi/2}, and so on.
  }

  \value{An object of class \code{\link{hegystat-class}}.}

  \seealso{\code{\link{HEGY.rectest}}.}

  \author{Javier Lopez-de-Lacalle \email{javlacalle@yahoo.es} and Ignacio Diaz-Emparanza
    \email{Ignacio.Diaz-Emparanza@ehu.es}
  }

  \references{
    S. Hylleberg, R. Engle, C. Granger and B. Yoo (1990), Seasonal integration and cointegration.
    \emph{Journal of Econometrics}, \bold{44}, 215-238.

    J. Beaulieu and J. Miron (1993), Seasonal unit roots in aggregate U.S. data.
    \emph{Journal of Econometrics}, \bold{54}, 305-328.

    P.H. Franses (1990), Testing for seasonal unit roots in monthly data, Technical Report 9032, Econometric
    Institute.
  }

  \examples{
    ## HEGY test with constant, trend and seasonal dummies.
    data(AirPassengers)
    lairp <- log(AirPassengers)
    hegy.out1 <- HEGY.test(wts=lairp, itsd=c(1,1,c(1:11)),
                   regvar=0, selectlags=list(mode="bic", Pmax=12))
    hegy.out1
    hegy.out2 <- HEGY.test(wts=lairp, itsd=c(1,1,c(1:11)),
                   regvar=0, selectlags=list(mode="signf", Pmax=NULL))
    hegy.out2
  }

\keyword{ts}


