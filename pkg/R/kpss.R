
setClass("kpssstat", representation(wts="ts", lmkpss="lm", ltrunc="numeric", levelst="numeric",
  trendst="numeric"))

KPSS.test <- function(wts, ltrunc=NULL)
{
  tiempo <- c(1:length(wts))
  ML <- ret(wts, 2)

  ##~ ver
  if(class(ltrunc)=="NULL")
    ltrunc <- as.integer(3*sqrt(length(wts))/13)
  #ltrunc <- as.integer(3*sqrt(length(wts))/13)  # ver PP.test, artículo KPSS
  #ltrunc <- as.integer(10*sqrt(length(wts))/14)
  #ltrunc <- as.integer(4*(length(wts)/100)^(1/4))
  #ltrunc <- as.integer(12*(length(wts)/100)^(1/4))

  lmkpss <- lm(ML[,1] ~ tiempo)
  ehat <- residuals(lmkpss)
  Sa  <- cumsum(ehat)
  N <- length(ehat)

  # estimador consistente de la varianza de los residuos:
  if(ltrunc == 0)
    s.2a <- 1/N*sum(ehat^2)
  if(ltrunc > 0){
    auxa <- c(1:ltrunc)
    for(i in 1:ltrunc)
      auxa[i] <- (1-i/(ltrunc+1))*sum(ehat[(i+1):N]*ehat[1:(N-i)])
    s.2a <- (1/N)*sum(ehat^2)+(2/N)*sum(auxa)
  }

  Trend <- N^(-2)*sum(Sa^2/s.2a)

  # para la hipótesis nula de estacionariedad en tendencia los
  # residuos son ee=var-mean(var),
  # se analiza la serie extrayendo la media y la tendencia.

  wts2 <- wts - mean(na.omit(wts))
  Sb    <- cumsum(wts2)

  # estimador consistente de la varianza de los residuos:
  if(ltrunc==0)
    s.2b <- 1/N*sum(wts2[1:N]^2)
  if(ltrunc>0){
    auxb <- c(1:ltrunc)
    for(i in 1:ltrunc)
      auxb[i] <- (1-i/(ltrunc+1))*sum(wts2[(i+1):N]*wts2[1:(N-i)])
    s.2b <- (1/N)*sum(wts2[1:N]^2)+(2/N)*sum(auxb)
  }

  Level <- N^(-2)*sum(Sb[1:N]^2/s.2b)

  new("kpssstat", wts=wts, lmkpss=lmkpss, ltrunc=ltrunc, levelst=Level, trendst=Trend)
}

setMethod("show", "kpssstat",
  function(object){
    cvl <- data.frame(rbind(c(0.347, 0.463, 0.574, 0.739)))
    dimnames(cvl) <- list("", c("0.10", "0.05", "0.025", "0.01"))
    cvt <- data.frame(rbind(c(0.119, 0.146, 0.176, 0.216)))
    dimnames(cvt) <- list("", c("0.10", "0.05", "0.025", "0.01"))

    cat("\n  ---- ----")
    cat("\n  KPSS test")
    cat("\n  ---- ----\n\n")

    cat("  Null hypotheses: Level stationarity and stationarity around a linear trend.\n")
    cat("  Alternative hypothesis: Unit root.\n")

    cat(c("----\n  Statistic for the null hypothesis of \n   level stationarity:",
        round(object@levelst, 3), "\n"))
    cat("\n    Critical values:\n\n")
    print(cvl)
    cat(c("----\n  Statistic for the null hypothesis of \n   trend stationarity:",
        round(object@trendst, 3), "\n"))
    cat("\n    Critical values:\n\n")
    print(cvt)
    cat(c("----\n  Lag truncation parameter:", object@ltrunc, "\n\n"))
  }
)
