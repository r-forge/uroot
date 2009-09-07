
##~ significance codes (*,**) en regvarcoefs...

setClassUnion("maybeRegvar", c("NULL", "numeric", "matrix", "data.frame")) # NULL y numeric para ltrunc en KPPS Y CH rect. 
setClassUnion("maybeLags", c("NULL", "list", "character", "numeric", "matrix")) ##~ "list"
setClass("adfstat", representation(wts="ts", itsd="numeric", regvar="maybeRegvar", selectlags="list",
  regvarcoefs="maybeRegvar", lagsorder="maybeLags", lagcoefs="maybeLags", res="numeric", lmadf="lm",
  stat="matrix"))

ADF.test <- function(wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
{
  s  <- frequency(wts); t0 <- start(wts); N  <- length(wts)
  if(s==1 && itsd[3] != 0){
     detreg$detcomp <- c(detreg$detcomp[1:2], 0)
     cat("  Seasonal dummies are not considered for annual data.\n")
  }

  # Dependent variable.
  Deltay <- matrix(c(NA, diff(wts, lag=1)), ncol=1)

  # Regressor variables.
  Intercept <- matrix(rep(1, N), ncol=1)[,itsd[1]]
  Trend     <- matrix(c(1:N), ncol=1)[,itsd[2]]
  SDummy <- data.frame(SeasDummy=SeasDummy(wts, "alg"))[,itsd[-c(1,2)]]

  if(!identical(regvar, 0) && length(names(regvar)) == 0)
    regvar <- data.frame(Regvar=regvar)

  if(identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- numeric(0)
  if(!identical(itsd, c(0,0,0)) && identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy))
  if(!identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(Intercept, Trend, SDummy, regvar))
  if(identical(itsd, c(0,0,0)) && !identical(regvar, 0))
    Mdetreg <- as.matrix(data.frame(regvar))
  regvarnames <- dimnames(Mdetreg)[[2]]

  # DF regression without lags.
  Madfreg <- as.matrix(data.frame(adf.reg=c(NA, wts[1:(N-1)])))
  ifelse(length(Mdetreg) == 0,
    lmdf <- lm(Deltay[,1] ~ 0+Madfreg),
    lmdf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg))

  # Lags selection.
  if(class(selectlags[[1]]) == "numeric"){
    selP <- selectlags[[1]]
  } else
      switch(selectlags[[1]],
        aic   = selP <- selPabic(lmdet=lmdf, type="aic", Pmax=selectlags[[2]]),
        bic   = selP <- selPabic(lmdet=lmdf, type="bic", Pmax=selectlags[[2]]),
        signf = selP <- selPsignf(lmdet=lmdf, cvref=NULL, Pmax=selectlags[[2]]),)

  # ADF regression.
  if(identical(selP, 0) || length(selP)==0){
    ifelse(length(Mdetreg)==0, lmadf <- lm(Deltay[,1] ~ 0+Madfreg),
                               lmadf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg))
  } else{
      Mlags <- ret(Deltay, max(selP)+2)[,-1]; aux <- dimnames(Mlags)[[2]]
      Mlags <- data.frame(Mlags[,selP]); lagnames <- aux[selP]
      Mlags <- as.matrix(Mlags)
      ifelse(length(Mdetreg)==0, lmadf <- lm(Deltay[,1] ~ 0+Madfreg + Mlags),
                                 lmadf <- lm(Deltay[,1] ~ 0+Mdetreg + Madfreg + Mlags))
    }

  # lmadf estimates.
  coefs <- coef(summary(lmadf)); Ncoef <- length(coef(lmadf))
  colnames <- dimnames(coefs)[[2]]
  ifelse(Ncoef==1, ref<-1, ref <- which(dimnames(coefs)[[1]] == "Madfreg"))

  # determiistic components estimates.
  if(ref > 1){
    regvarcoefs <- coefs[1:(ref-1),1:4]
    dim(regvarcoefs) <- c((ref-1), 4)
    dimnames(regvarcoefs) <- list(regvarnames, colnames)
  } else
      regvarcoefs <- NULL

  # ADF statistic and p-value.
  adfreg <- t(as.data.frame(coefs[ref,1:4]))
  code <- paste("DF", paste(itsd[1:2], collapse=""), "0", sep="")
  adfreg[,4] <- interpolpval(code=code, stat=adfreg[,3], N=N)$pval
  dimnames(adfreg) <- list("adf.reg", colnames[1:4])

  # Lags estimates
  if(ref < Ncoef){
    lagcoefs <- coefs[(ref+1):Ncoef,1:4]
    dim(lagcoefs) <- c(length((ref+1):Ncoef), 4); lagcoefs <- data.frame(lagcoefs)
    dimnames(lagcoefs) <- list(lagnames, colnames); lagcoefs <- as.matrix(lagcoefs)
  } else
      lagcoefs <- NULL

  new("adfstat", wts=wts, itsd=itsd, regvar=regvar, selectlags=selectlags, regvarcoefs=regvarcoefs,
    lagsorder=selP, lagcoefs=lagcoefs, res=residuals(lmadf), lmadf=lmadf, stat=adfreg)
}

setMethod("show", "adfstat",
  function(object)
  {
    lmadf <- object@lmadf
    coefs <- coef(lmadf); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="Madfreg"))

    cat("  --------- ------ - ------ ----\n")
    cat("  Augmented Dickey & Fuller test\n")
    cat("  --------- ------ - ------ ----\n\n")

    cat("  Null hypothesis: Unit root.\n")
    cat("  Alternative hypothesis: Stationarity.\n")

    cat("\n----\n  ADF statistic:\n\n")
    print(round(object@stat, 3))
    cat("\n  Lag orders:", object@lagsorder)
    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setMethod("summary", "adfstat",
  function(object)
  {
    lmadf <- object@lmadf
    coefs <- coef(lmadf); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="Madfreg"))

    ##~ Mostrando p-values.
    cat("  --------- ------ - ------ ----\n")
    cat("  Augmented Dickey & Fuller test\n")
    cat("  --------- ------ - ------ ----\n\n")

    cat("----\n  Deterministic regressors estimates:\n\n")
    if(ref > 1){
      object@regvarcoefs[,1:4] <- round(object@regvarcoefs[,1:4], 3)
      print(object@regvarcoefs)
    } else
        cat("    None selected.\n")

    cat("\n----\n  ADF regressor estimate:\n\n")
    print(round(object@stat, 3))

    cat("\n----\n  Selected lags estimates:\n\n")
    if(ref < length(coefnames)){
      object@lagcoefs[,1:4] <- round(object@lagcoefs[,1:4], 3)
      print(object@lagcoefs)
    } else
        cat("    None selected.\n")

    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)
