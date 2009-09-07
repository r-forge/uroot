
##~ F-st para VFE seleccionadas. y p.value para F.2:s F1:s, buscar tablas.
##~ significance codes (*,**) en regvarcoefs...

setClass("hegystat", representation(wts="ts", itsd="numeric", regvar="maybeRegvar", hegyreg="matrix",
  selectlags="list", regvarcoefs="maybeRegvar", hegycoefs="matrix", lagsorder="maybeLags",
  lagcoefs="maybeLags", res="numeric", lmhegy="lm", stats="matrix"))

HEGY.test <- function(wts, itsd, regvar=0, selectlags=list(mode="signf", Pmax=NULL))
{
  s  <- frequency(wts); t0 <- start(wts); N <- length(wts)

  # Dependent variable.
  Deltay <- matrix(c(rep(NA, s), diff(wts, lag=s)), ncol=1)

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

  # HEGY regression without lags.
  Mhegyreg <- hegy.reg(wts)
  ifelse(length(Mdetreg) == 0,
    lmdf <- lmhegyp <- lm(Deltay[,1] ~ 0+Mhegyreg),
    lmdf <- lmhegyp <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg))

  # Lags selection.
  if(class(selectlags[[1]]) == "numeric"){
    selP <- selectlags[[1]]
  } else
      switch(selectlags[[1]],
        aic   = selP <- selPabic(lmdet=lmhegyp, type="aic", Pmax=selectlags[[2]]),
        bic   = selP <- selPabic(lmdet=lmhegyp, type="bic", Pmax=selectlags[[2]]),
        signf = selP <- selPsignf(lmdet=lmhegyp, cvref=NULL, Pmax=selectlags[[2]]),)

  # HEGY regression.
  # lmdetlag: regression with deterministic components and lags (without the hegy regressors).
  # lmhegy: lmdetlag including the hegy regressors.
  if(identical(selP, 0) || length(selP)==0){
    if(length(Mdetreg)==0){
      lmdetlag <- lm(Deltay[,1] ~ 0)
      lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mhegyreg)
    } else{
        lmdetlag <- lm(Deltay[,1] ~ 0+Mdetreg)
        lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg)
      }
  } else{
      Mlags <- ret(Deltay, max(selP)+2)[,-1]; aux <- dimnames(Mlags)[[2]]
      Mlags <- data.frame(Mlags[,selP]); lagnames <- aux[selP]
      Mlags <- as.matrix(Mlags)
      if(length(Mdetreg)==0){
        lmdetlag <- lm(Deltay[,1] ~ 0+ Mlags)
        lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mhegyreg + Mlags)
      } else{
          lmdetlag <- lm(Deltay[,1] ~ 0+Mdetreg + Mlags)
          lmhegy <- lmhegyout <- lm(Deltay[,1] ~ 0+Mdetreg + Mhegyreg + Mlags)
      }
    }

  # lmhegy estimates.
  coefs <- coef(summary(lmhegy)); Ncoef <- length(coef(lmhegy))
  colnames <- dimnames(coefs)[[2]]
  ifelse(Ncoef==s, ref<-1, ref <- which(dimnames(coefs)[[1]] == "MhegyregYpi1"))

  if(ref > 1){
    regvarcoefs <- coefs[1:(ref-1),1:4]
    dim(regvarcoefs) <- c((ref-1), 4)
    dimnames(regvarcoefs) <- list(regvarnames, colnames)
  } else
      regvarcoefs <- NULL

  # hegyreg
  ##~ Con bootshegy puede ocurrir que Mhegyreg tiene columnas casi 0 y estimación es NA, entonces coefs[ref:(ref+s-1),1:4] eliman esos NAs y ref ya no sirve como referencia.
  hegycoefs <- coefs[ref:(ref+s-1),1:4]  ##~ Añadir F_1:s, F_2:s
  dimnames(hegycoefs)[[1]] <- dimnames(Mhegyreg)[[2]]

  if((ref+s-1) < Ncoef){
    lagcoefs <- coefs[(ref+s):Ncoef,1:4]
    dim(lagcoefs) <- c(length((ref+s):Ncoef), 4); lagcoefs <- data.frame(lagcoefs)
    dimnames(lagcoefs) <- list(lagnames, colnames); lagcoefs <- as.matrix(lagcoefs)
  } else
      lagcoefs <- NULL

  # HEGY Statistics and p-values.
  # tpi
  if(s==4) c1 <- "HEGY";
  if(s==12) c1 <- "BM"
  c2 <- paste(itsd[1:2], sep="", collapse="")
  ifelse(itsd[3] != 0, c3 <-1, c3 <-0)

  for(i in 1:s){
    code <- paste(c(c1, c2, c3, "tpi", i), sep="", collapse="")
    hegycoefs[i,4] <- interpolpval(code=code, stat=hegycoefs[i,3], N=N)$pval
  }

  # tpi1-tpi2, Fpi_
  EtFst <- matrix(nrow=(s/2+3), ncol=2)
  EtFst[1:2,] <- hegycoefs[1:2,3:4]

  code <- paste(c(c1, c2, c3, "Foddeven"), sep="", collapse="")
  for(i in 1:(s/2-1)){
    lmpart <- update(lmdetlag, . ~ . + Mhegyreg[,c(1:s)[-c(2*i+1, 2*i+2)]])
    lmhegy <- update(lmpart, . ~ . + Mhegyreg[,c(2*i+1, 2*i+2)])
    EtFst[i+2,1] <- anova(lmpart, lmhegy, test="F")$F[2]
    EtFst[i+2,2] <- interpolpval(code=code, stat=EtFst[i+2,1], N=N)$pval
  }

  lmpart <- update(lmdetlag, . ~ . + Mhegyreg[,1])
  lmhegy <- update(lmpart, . ~ . + Mhegyreg[,2:s])
  EtFst[(s/2+2),1] <- anova(lmpart, lmhegy, test="F")$F[2]

  lmhegy <- update(lmdetlag, . ~ . + Mhegyreg[,1:s])
  EtFst[(s/2+3),1] <- anova(lmdetlag, lmhegy, test="F")$F[2]

  EtFsnames <- c("tpi_1", "tpi_2",
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))
  #dim(EtFst) <- c((s/2+3), 2);
  dimnames(EtFst) <- list(EtFsnames, c("Stat.", "p-value"))

  new("hegystat", wts=wts, itsd=itsd, regvar=regvar, hegyreg=Mhegyreg, selectlags=selectlags,
    regvarcoefs=regvarcoefs, hegycoefs=hegycoefs, lagsorder=selP, lagcoefs=lagcoefs,
    res=residuals(lmhegy), lmhegy=lmhegyout, stats=EtFst)
}

setMethod("show", "hegystat",
  function(object)
  {
    s <- frequency(object@wts); lmhegy <- object@lmhegy
    coefs <- coef(lmhegy); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="MhegyregYpi1"))

    ##~ Mostrando p-values.
    cat("  ---- ----\n")
    cat("  HEGY test\n")
    cat("  ---- ----\n\n")

    cat("  Null hypothesis: Unit root.\n")
    cat("  Alternative hypothesis: Stationarity.\n")

    cat("\n----\n  HEGY statistics:\n\n")
    print(round(object@stats, 3))
    cat("\n  Lag orders:", object@lagsorder)
    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)

setMethod("summary", "hegystat",
  function(object)
  {
    s <- frequency(object@wts); lmhegy <- object@lmhegy
    coefs <- coef(lmhegy); coefnames <- names(coefs)
    ifelse(length(coefnames)==1, ref <- 1, ref <- which(coefnames=="MhegyregYpi1"))

    ##~ Mostrando p-values.
    cat("  ---- ----\n")
    cat("  HEGY test\n")
    cat("  ---- ----\n\n")

    cat("----\n  Deterministic regressors estimates:\n\n")
    if(ref > 1){
      print(round(object@regvarcoefs, 3))
    } else
        cat("    None selected.\n")

    cat("\n----\n  HEGY regressors estimates:\n\n")
    print(round(object@hegycoefs, 3))

    cat("\n----\n  Selected lags estimates:\n\n")
    if((ref+s-1) < length(coefnames)){
      print(round(object@lagcoefs, 3))
    } else
        cat("    None selected.\n")

    cat("\n  Number of available observations:", length(object@res),"\n")
  }
)
