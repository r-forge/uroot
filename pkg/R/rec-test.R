
##~ Poner en help: regvar es cero, no se ha implementado este argumento aquí.

# ADF recursive test.

setClass("adfrecst", representation(wts="ts", type="character", nsub="numeric", itsd="numeric",
  regvar="numeric", selectlags="list", recstats="matrix", elaps="list"))

ADF.rectest <- function(wts, type="moving", nsub=48, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=2)
  code <- paste("DF", paste(itsd[1:2], collapse=""), "0", sep="")

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    Mst[i+1,1] <- ADF.test(wts=wtsss, itsd=itsd, regvar=0, selectlags=selectlags)@stat[,3]
    Mst[i+1,2] <- interpolpval(code=code, stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1)], xlim=c(1,iter+1), ylab="ADF statistic", xlab="Iteration", main="ADF statistics")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    Mst[nrow(Mst),1] <- ADF.test(wts=wts, itsd=itsd, regvar=0, selectlags=selectlags)@stat[,3]
    Mst[nrow(Mst),2] <- interpolpval(code=code, stat=Mst[nrow(Mst),1], N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=2, dimnames=list(subsdates, c("adf.stat", " ")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("adfrecst", wts=wts, type=type, nsub=nsub, itsd=itsd, regvar=0, selectlags=selectlags,
    recstats=Mst, elaps=elaps)
}

setMethod("show", "adfrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    sts <- Tround(matrix(as.numeric(object@recstats[,1])), column=1, digits=2)
    sts <- format(sts, justify="right")
    pvls <- format(object@recstats[,2], justify="left")

    smpls <- dimnames(object@recstats)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  --- --------- ----")
    cat("\n  ADF recursive test")
    cat("\n  --- --------- ----\n\n")
    cat("  ADF statistics in each subsample:\n\n")

    for(i in 1:iter)
      cat(" ", smpls[i], " ", sts[i], pvls[i], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="adfrecst", y="missing"),
  function(x,...)
  {
    plot(as.numeric(x@recstats[,1]), xlim=c(1,nrow(x@recstats)), ylab="ADF statistic",
      xlab="Iteration", main="ADF statistics", las=1)
  }
)

# KPSS recursive test.

setClass("kpssrecst", representation(wts="ts", type="character", nsub="numeric", ltrunc="maybeRegvar",
  recstats="matrix", elaps="list"))

KPSS.rectest <- function(wts, type="moving", nsub=48, ltrunc=NULL, trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=4)

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    out <- KPSS.test(wts=wtsss, ltrunc=ltrunc)
    Mst[i+1,1] <- out@levelst
    Mst[i+1,2] <- interpolpval(code="KPSSlevel", stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    Mst[i+1,3] <- out@trendst
    Mst[i+1,4] <- interpolpval(code="KPSStrend", stat=Mst[(i+1),3], N=length(wtsss), swarn=FALSE)$pvlab

    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(3,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1),1], xlim=c(1,iter+1),
        ylab="KPSS statistic", xlab="Iteration", main="KPSS Level.stat")
      plot(Mst[1:(i+1),2], xlim=c(1,iter+1),
        ylab="KPSS statistic", xlab="Iteration", main="KPSS Trend.stat")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    out <- KPSS.test(wts=wts, ltrunc=ltrunc)
    Mst[nrow(Mst),1] <- out@levelst
    Mst[nrow(Mst),2] <- interpolpval(code="KPSSlevel", stat=Mst[nrow(Mst),1],
                                     N=length(wts), swarn=FALSE)$pvlab
    Mst[nrow(Mst),3] <- out@trendst
    Mst[nrow(Mst),4] <- interpolpval(code="KPSStrend", stat=Mst[nrow(Mst),3],
                                     N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=4,
           dimnames=list(subsdates, c("level.stat", "pvl", "trend.stat", "pvl")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("kpssrecst", wts=wts, type=type, nsub=nsub, ltrunc=ltrunc, recstats=Mst, elaps=elaps)
}

setMethod("show", "kpssrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    stsl <- Tround(matrix(as.numeric(object@recstats[,1])), column=1, digits=2)
    stsl <- format(stsl, justify="right")
    pvll <- format(object@recstats[,2], justify="left")
    stst <- Tround(matrix(as.numeric(object@recstats[,3])), column=1, digits=2)
    stst <- format(stst, justify="right")
    pvlt <- format(object@recstats[,4], justify="left")

    smpls <- dimnames(object@recstats)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  ---- --------- ----")
    cat("\n  KPSS recursive test")
    cat("\n  ---- --------- ----\n\n")
    cat("  KPSS statistics in each subsample:\n\n")

    cat(rep(" ", nchar(smpls[1])/2), "    Level", "      Trend", "\n")
    for(i in 1:iter)
      cat(" ", smpls[i], " ", stsl[i], pvll[i], "  ", stst[i], pvlt[i], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="kpssrecst", y="missing"),
  function(x,...)
  {
    Mst <- x@recstats
    opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
    plot(as.numeric(Mst[,1]), xlim=c(1,nrow(Mst)),
      ylab="KPSS statistic", xlab="Iteration", main="KPSS Level.stat")
    plot(as.numeric(Mst[,3]), xlim=c(1,nrow(Mst)),
      ylab="KPSS statistic", xlab="Iteration", main="KPSS Trend.stat")
    par(opar)
  }
)

# HEGY recursive test.

setClass("hegyrecst", representation(wts="ts", type="character", nsub="numeric", itsd="numeric",
  regvar="numeric", selectlags="list", recstats="matrix", elaps="list"))

HEGY.rectest <- function(wts, type="moving", nsub=72, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)
  if(s==4)  pdim <- c(3,2)
  if(s==12) pdim <- c(5,2)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  if(s==4)  c1 <- "HEGY"
  if(s==12) c1 <- "BM"
  c2 <- paste(itsd[1:2], sep="", collapse="")
  ifelse(itsd[3] != 0, c3 <-1, c3 <-0)
  codeaux <- paste(c1, c2, c3, sep="")

  etfsnames <- c(paste("tpi_", 1:2, sep=""),
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

  out <- matrix(nrow=iter+lss, ncol=((s/2+3)*2))  #ncol=(s+s/2+1))  ##~ no poner tpi_>2
  subsdates <- rep(NA, nrow(out))

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    hegy.out <- HEGY.test(wts=wtsss, itsd=itsd, regvar=0, selectlags=selectlags)
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    out[i+1,c(1,3)] <- hegy.out@hegycoefs[1:2,3]  # hegy.out@hegycoefs[,3]
    out[i+1,2] <- interpolpval(code=paste(codeaux, "tpi1", sep=""), stat=out[i+1,1],
                               N=length(wtsss), swarn=FALSE)$pvlab
    out[i+1,4] <- interpolpval(code=paste(codeaux, "tpi2", sep=""), stat=out[i+1,3],
                               N=length(wtsss), swarn=FALSE)$pvlab

    out[i+1,seq(5,ncol(out),2)] <- hegy.out@stats[3:(s/2+3),1]
    codeF <- paste(codeaux, "Foddeven", sep="")
    for(j in seq(6,5+(s/2-1)*2,2))
      out[i+1,j] <- interpolpval(code=codeF, stat=out[i+1,j-1], N=length(wtsss), swarn=FALSE)$pvlab

    if(trace[2] == 1){
      opar <- par(mfrow=pdim, mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      k<-1
      for(j in seq(1,ncol(out),2)){
        plot(out[1:(i+1),j], xlim=c(1,iter+1), ylab="", xlab="Iteration",
          main=paste(etfsnames[k], "statistics", sep=" "))
        k<-k+1
      }
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    hegy.out <- HEGY.test(wts=wts, itsd=itsd, regvar=0, selectlags=selectlags)
    subsdates[iter+lss] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")

    out[nrow(out),c(1,3)] <- hegy.out@hegycoefs[1:2,3]  # hegy.out@hegycoefs[,3]
    out[nrow(out),2] <- interpolpval(code=paste(codeaux, "tpi1", sep=""),
                          stat=out[nrow(out),1], N=length(wts), swarn=FALSE)$pvlab
    out[nrow(out),4] <- interpolpval(code=paste(codeaux, "tpi2", sep=""),
                          stat=out[nrow(out),3], N=length(wts), swarn=FALSE)$pvlab

    out[nrow(out),seq(5,ncol(out),2)] <- hegy.out@stats[3:(s/2+3),1]
    codeF <- paste(codeaux, "Foddeven", sep="")
    for(j in seq(6,5+(s/2-1)*2,2))
      out[nrow(out),j] <- interpolpval(code=codeF, stat=out[nrow(out),j-1],
                                       N=length(wts), swarn=FALSE)$pvlab
  }

  etfsn <- rep(NA, ncol(out))
  etfsn[seq(1,ncol(out),2)] <- etfsnames
  etfsn[seq(2,ncol(out),2)] <- " "
  out <- matrix(out, nrow=nrow(out), ncol=ncol(out), dimnames=list(subsdates, etfsn))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("hegyrecst", wts=wts, type=type, nsub=nsub, itsd=itsd, regvar=0, selectlags=selectlags,
    recstats=out, elaps=elaps)
}

setMethod("show", "hegyrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps; s <- frequency(object@wts)
    out <- object@recstats[,1:((s/2+1)*2)]      # Sin tomar F_2:s, F_1:s
    aux1 <- seq(1,ncol(out),2); aux2 <- aux1+1  # aux2 <- seq(2,ncol(out),2)
    auxout <- matrix(as.numeric(out[,aux1]), ncol=ncol(out)/2)
    for(i in 1:ncol(auxout))
      auxout <- Tround(auxout, column=i, digits=2)
    out[,aux1] <- format(auxout, justify="right")
    out[,aux2] <- format(out[,aux2], justify="left")

    smpls <- dimnames(out)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    out2 <- matrix(ncol=ncol(out), nrow=nrow(out)+1)
    out2[1,aux1] <- dimnames(out)[[2]][aux1]
    out2[1,aux2] <- "   "
    out2[2:nrow(out2),] <- out
    out2[,aux1] <- format(out2[,aux1], justify="right")

    cat("\n  ---- --------- ----")
    cat("\n  HEGY recursive test")
    cat("\n  ---- --------- ----\n\n")
    cat("  HEGY statistics in each subsample:\n\n")

    cat(rep(" ", nchar(smpls[1])/2+1), out2[1,1:4], "\n")
    for(i in 2:(iter+1))
      cat(" ", smpls[i-1], " ", out2[i,1:4], "\n")
    if(ncol(out2) > 4){
      cat("\n", rep(" ", nchar(smpls[1])/2+1), out2[1,5:ncol(out2)], "\n")
      for(i in 2:(iter+1))
        cat(" ", smpls[i-1], " ", out2[i,5:ncol(out2)], "\n")
    }

    cat("\n\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="hegyrecst", y="missing"),
  function(x,...)
  {
    out <- x@recstats; s <- frequency(x@wts)
    if(frequency(x@wts)==4)  pdim <- c(3,2)
    if(frequency(x@wts)==12) pdim <- c(5,2)
    etfsnames <- c(paste("tpi_", 1:2, sep=""),
                 paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                 paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

    opar <- par(mfrow=pdim, mar=c(3,4.2,2.5,2), las=1)
    k<-1
    for(j in seq(1,ncol(x@recstats),2)){
      plot(out[,j], xlim=c(1,nrow(out)), ylab="", xlab="Iteration",
        main=paste(etfsnames[k], "statistics", sep=" "))
      k<-k+1
    }
    par(opar)
  }
)


setClass("chrecst", representation(wts="ts", type="character", nsub="numeric", frec="numeric", f0="numeric",
  DetTr="logical", ltrunc="maybeRegvar", recstats="matrix", elaps="list"))

CH.rectest <- function(wts, type="moving", nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1))
{
  s <- frequency(wts); N <- length(wts); t0 <- start(wts); tN <- end(wts)
  if(class(frec)=="NULL") frec <- rep(1, s/2)

  if(type == "moving"){
    iter <- length(wts)-nsub; lss <- 1 }
  if(type == "backw" || type == "forw"){
    iter <- as.integer((N-nsub)/s); lss <- 2 }

  code <- paste("CHp", sum(frec*c(rep(2, s/2-1),1)), sep="")
  subsdates <- rep(NA, iter+lss)
  Mst <- matrix(NA, nrow=iter+lss, ncol=2)

  refiter <- seq(0,100,5); ptm1 <- proc.time()
  for(i in 0:iter)
  {
    switch(type,
      "moving" = { t0ss <- stepdate(as.vdate(wts), i)@ys
                   tNss <- stepdate(as.vdate(wts), i+nsub-1)@ys },
      "forw"   = { t0ss <- as.vdate(wts)@ys
                   tNss <- stepdate(as.vdate(wts), nsub+i*s-1)@ys },
      "backw"  = { t0ss <- stepdate(as.vdate(wts, N), -(nsub+i*s-1))@ys
                   tNss <- stepdate(as.vdate(wts), N-1)@ys },)

    t0ss <- as.numeric(t0ss); tNss <- as.numeric(tNss)
    wtsss <- window(wts, start=t0ss, end=tNss)

    Mst[i+1,1] <- CH.test(wts=wtsss, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc)@stat
    Mst[i+1,2] <- interpolpval(code=code, stat=Mst[(i+1),1], N=length(wtsss), swarn=FALSE)$pvlab
    subsdates[i+1] <- paste(t0ss[1], ":", t0ss[2], "-", tNss[1], ":", tNss[2], sep="")

    if(trace[2] == 1){
      opar <- par(mfrow=c(2,1), mar=c(3,4.2,2.5,2), las=1)
      plot(wts, xlab="", ylab="", main="Subsamples")
      abline(v=(t0ss[1] + t0ss[2]/s-1/s), lty=2, col="blue")
      abline(v=(tNss[1] + tNss[2]/s-1/s), lty=2, col="blue")
      plot(Mst[1:(i+1),1], xlim=c(1,iter+1), ylab="CH statistic", xlab="Iteration", main="CH statistics")
      par(opar)
    }

    if(trace[1] == 1){
      refaux <- which(refiter == as.integer((i/iter)*100))
      if(length(refiter[refaux]) == 1){
        refiter[refaux] <- NA
        cat(paste("  ", as.integer((i/iter)*100), "% complete.\n", sep=""))
      }
    }
  }

  if(type == "backw" || type == "forw"){
    Mst[nrow(Mst),1] <- CH.test(wts=wts, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc)@stat
    Mst[nrow(Mst),2] <- interpolpval(code=code, stat=Mst[nrow(Mst),1], N=length(wts), swarn=FALSE)$pvlab
    subsdates[nrow(Mst)] <- paste(t0[1], ":", t0[2], "-", tN[1], ":", tN[2], sep="")
  }
  Mst <- matrix(Mst, nrow=iter+lss, ncol=2, dimnames=list(subsdates, c("ch.stat", " ")))

  ptm2 <- proc.time(); elaps <- elapsedtime(ptm1, ptm2)
  if(trace[3] == 1)
    cat("\n  Elapsed time:", elaps$elaps, elaps$units, ".\n")

  new("chrecst", wts=wts, type=type, nsub=nsub, frec=frec, f0=f0, DetTr=DetTr, ltrunc=ltrunc,
    recstats=Mst, elaps=elaps)
}

setMethod("show", "chrecst",
  function(object)
  {
    iter <- nrow(object@recstats); elaps <- object@elaps
    out <- matrix(c(as.numeric(object@recstats[,1]), object@recstats[,2]),
             ncol=2, dimnames=dimnames(object@recstats))
    out <- Tround(out, column=1, digits=2)
    out[,1] <- format(out[,1], justify="right")
    out[,2] <- format(out[,2], justify="left")

    smpls <- dimnames(out)[[1]]
    aux <- max(nchar(smpls))
    for(i in 1:length(smpls)){
      refnst <- rep(" ", aux-nchar(smpls[i]))
      if(length(refnst) > 0)
        smpls[i] <- paste(smpls[i], refnst, sep="")[1]
    }

    cat("\n  -- --------- ----")
    cat("\n  CH recursive test")
    cat("\n  -- --------- ----\n\n")
    cat("  CH statistics in each subsample:\n\n")

    for(i in 1:iter)
      cat(" ", smpls[i], " ", out[i,], "\n")

    cat("\n Signif. codes:  0 '***' 0.01 '**' 0.025 '*' 0.05 '.' 0.1 ' ' 1 \n")
    switch(object@type,
      "moving" = cat("\n Based on", iter, "subsamples of length", object@nsub, ".\n"),
      "backw"  = cat("\n Based on", iter, "backwards subsamples.\n"),
      "forw"   = cat("\n Based on", iter, "forwards subsamples.\n"),)
    cat(" Computation time:", elaps$elaps, elaps$units, ".\n\n")
  }
)

setMethod("plot", signature(x="chrecst", y="missing"),
  function(x,...)
  {
    out <- x@recstats
    plot(out[,1], xlim=c(1,nrow(out)),
      ylab="CH statistic", xlab="Iteration", main="CH statistics", las=1)
  }
)
