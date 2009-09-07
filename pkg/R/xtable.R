
urt.xtable <- function(x, caption=NULL, label=NULL, align=NULL, vsep=NULL, digits=NULL, display=NULL,...){
}
save.xtable <- function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...){
}

## ADF

setMethod("urt.xtable", "adfstat",
  function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(x@stat, ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(x@stat))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@stat), t(x@lagcoefs))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(x@stat), t(x@lagcoefs))), ncol=4, byrow=TRUE)

    regnames <- c(dimnames(x@regvarcoefs)[[1]], "adf.stat", dimnames(x@lagcoefs)[[1]])
    cnames <- c("Estimate" , "Std.Error", "t-stat", "p-value")
    Mout <- matrix(Mout, ncol=4, dimnames=list(regnames, cnames))

    xtable(x=Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "adfstat",
  function(x, caption="ADF test", label="Tadf", align="lrrrr", vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tadf.tex"
    }

    regnames <- c(dimnames(x@regvarcoefs)[[1]], "adf.stat", dimnames(x@lagcoefs)[[1]])
    Tout <- urt.xtable(x, caption=caption, label=label, align=align,
                       vsep=vsep, digits=digits, display=display)

    hline.after <- hlref0 <- which(regnames=="adf.stat")
    if(hlref0 > 1)
      hline.after <- c(hline.after-1, hline.after)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

## HEGY

setMethod("urt.xtable", "hegystat",
  function(x, caption="HEGY test", label="Thegy", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    s <- frequency(x@wts); refrv <- length(x@regvarcoefs[,1])
    hegyout <- matrix(nrow=(s+s/2+1), ncol=4)
    hegyout[1:s,] <- x@hegycoefs
    hegyout[(s+1):(s+s/2+1),3:4] <- x@stats[3:(s/2+3),]

    etfsnames <- c(paste("tpi_", 1:s, sep=""),
                   paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                   paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))

    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "NULL")
      Mout <- hegyout
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "NULL")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(hegyout))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "NULL" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(hegyout), t(x@lagcoefs))), ncol=4, byrow=TRUE)
    if(class(x@regvarcoefs) == "matrix" && class(x@lagcoefs) == "matrix")
      Mout <- matrix(t(c(t(x@regvarcoefs), t(hegyout), t(x@lagcoefs))), ncol=4, byrow=TRUE)

    regnames <- c(dimnames(x@regvarcoefs)[[1]], etfsnames, dimnames(x@lagcoefs)[[1]])
    cnames <- c("Estimate" , "Std.Error", "Statistics", "p-value")
    Mout <- matrix(Mout, ncol=4, dimnames=list(regnames, cnames))

    #xtable(x=Mout[1:(s+refrv),],
    #  caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
    xtable(x=Mout[,3:4], caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "hegystat",
  function(x, caption="HEGY test", label="Thegy", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Thegy.tex"
    }

    etfsnames <- c(paste("tpi_", 1:s, sep=""),
                   paste("Fpi", paste(seq(3,s,2),seq(4,s,2), sep=":"), sep="_"),
                   paste("Fpi_2:", s, sep=""), paste("Fpi_1:", s, sep=""))
    regnames <- c(dimnames(x@regvarcoefs)[[1]], etfsnames, dimnames(x@lagcoefs)[[1]])

    Tout <- urt.xtable(x, caption=caption, label=label, align=align, vsep=vsep,
                       digits=digits, display=display)

    hline.after <- hlref0 <- which(regnames=="tpi_1")
    if(hlref0 > 1)
      hline.after <- c(hline.after-1, hline.after)
    hline.after <- c(hline.after, frequency(x@wts)+s+s/2+1)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

# KPSS

setMethod("urt.xtable", "kpssstat",
  function(x, caption="KPSS test", label="Tkpss", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    #Mout <- matrix(NA, nrow=5, ncol=3)
    #Mout[,1] <- c("0","1","2","3","4")

    ifelse(x@ltrunc-3 < 0, lt0<-0, lt0<-x@ltrunc-3)
    aux <- length(lt0:(x@ltrunc+3))
    Mout <- matrix(NA, nrow=aux, ncol=3)
    j <- 1
    for(i in lt0:(x@ltrunc+3)){
      out <- KPSS.test(wts=x@wts, ltrunc=i)
      Mout[j,1] <- i
      Mout[j,2] <- round(out@levelst, 2)
      Mout[j,3] <- round(out@trendst, 2)
      j <- j+1
    }
    Mout <- matrix(Mout, ncol=3, dimnames=list(rep("",aux), c("Trunc", "Level.st", "Trend.st")))
    xtable(Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "kpssstat",
  function(x, caption="KPSS test", label="Tkpss", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tkpss.tex"
    }

    Tout <- urt.xtable(x)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

# CH

setMethod("urt.xtable", "chstat",
  function(x, caption="CH test", label="Tch", align=NULL, vsep=NULL, digits=NULL, display=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    s <- frequency(x@wts)
    if(s==4)
      fnames <- c("$L_{\\pi/2}$", "$L_{\\pi$}", "$L_f$")
    if(s==12)
      fnames <- c("$L_{\\pi/6}$", "$L_{\\pi/3}$", "$L_{\\pi/2}$",
                  "$L_{2\\pi/3}$", "$L_{5\\pi/6}$", "$L_{\\pi}$", "$L_f$")

    Mout <- matrix(nrow=(s/2+1), ncol=2)
    for(i in 1:(s/2)){
      frec <- rep(0, s/2); frec[i] <- 1
      Mout[i,1] <- CH.test(wts=x@wts, frec=frec, f0=0, DetTr=x@DetTr, ltrunc=NULL)@stat
      Mout[i,2] <- CH.test(wts=x@wts, frec=frec, f0=1, DetTr=x@DetTr, ltrunc=NULL)@stat
    }
    Mout[(s/2+1),1] <- CH.test(wts=x@wts, frec=rep(1,s/2), f0=0, DetTr=x@DetTr, ltrunc=NULL)@stat
    Mout[(s/2+1),2] <- CH.test(wts=x@wts, frec=rep(1,s/2), f0=1, DetTr=x@DetTr, ltrunc=NULL)@stat

    Mout <- matrix(Mout, ncol=2, dimnames=list(fnames, c("$y_t$", "$\\Delta\\,y_t$")))

    xtable(Mout, caption=caption, label=label, align=align, vsep=vsep, digits=digits, display=display)
  }
)

setMethod("save.xtable", "chstat",
  function(x, caption="CH test", label="Tch", align=NULL, vsep=NULL, digits=NULL, display=NULL,
    type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
    caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
  {
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      #outfile <- "Tch.tex"
    }

    Tout <- urt.xtable(x)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat("\n  The table has been saved at", outfile, ".\n")
  }
)

## Output in recursive testing.


recadf.save.xtable <- function(wts, nsub=72, itsd, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1),
  caption="ADF recursive test", label="Tadfrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    rec.bw <- ADF.rectest(wts=wts, type="backw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)
    rec.fw <- ADF.rectest(wts=wts, type="forw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,1])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,2], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,1])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,2], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

rechegy.save.xtable <- function(wts, nsub=72, itsd, refstat, selectlags=list(mode="signf", Pmax=NULL),
  trace=list(remain=1, plot=0, elaps=1),
  caption="HEGY recursive test", label="Thegyrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    refstat2 <- which(c("tpi1","","tpi2","","Fpi3:4","","Fpi5:6","",
                        "Fpi7:8","","Fpi9:10","","Fpi11:12") == refstat)
    if(length(refstat2)==0)
      stop(call="Select a correct statistic's name.")
    rec.bw <- HEGY.rectest(wts=wts, type="backw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)
    rec.fw <- HEGY.rectest(wts=wts, type="forw", nsub=nsub, itsd=itsd, selectlags=selectlags, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,refstat2])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,refstat2+1], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,refstat2])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,refstat2+1], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    cat(paste("Statistic ", refstat, "\n"))
    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

reckpss.save.xtable <- function(wts, nsub=48, testtype="level", ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1),
  caption="KPSS recursive test", label="Tkpssrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }

    if(testtype=="level")
      ref <- 1
    if(testtype=="trend")
      ref <- 3
    rec.bw <- KPSS.rectest(wts=wts, type="backw", nsub=nsub,ltrunc=ltrunc, trace=trace)
    rec.fw <- KPSS.rectest(wts=wts, type="forw", nsub=nsub, ltrunc=ltrunc, trace=trace)

    fwsmpls <- dimnames(rec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(rec.fw@recstats[,ref])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(rec.fw@recstats[,ref+1], justify="left")

    bwsmpls <- dimnames(rec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(rec.bw@recstats[,ref])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(rec.bw@recstats[,ref+1], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}

recch.save.xtable <- function(wts, nsub=48, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL,
  trace=list(remain=1, plot=0, elaps=1),
  caption="CH recursive test", label="Tchrec", align="lcrlcrl", vsep=NULL, digits=NULL,
  display=NULL, type="latex", file=NULL, append=FALSE, floating=TRUE, table.placement="ht",
  caption.placement="top", latex.environments=c("center"), size=NULL, hline.after=NULL,...)
{
    require(xtable) || stop("The package xtable is not available")
    outfile <- tclvalue(tkgetSaveFile(filetypes='{"Text files" {".tex"}} {"All Files" {"*"}}'))
    if(!nchar(outfile)){
      tkmessageBox(title="Stop", message="No file was chosen.", icon="error")
      stop(call="No file was chosen.")
    }
    chrec.bw <- CH.rectest(wts=wts, type="backw", nsub=nsub, frec=frec, f0=f0, DetTr=DetTr,
                           ltrunc=ltrunc, trace=trace)
    chrec.fw <- CH.rectest(wts=wts, type="forw", nsub=nsub, frec=frec, f0=f0, DetTr=DetTr,
                           ltrunc=ltrunc, trace=trace)

    fwsmpls <- dimnames(chrec.fw@recstats)[[1]]
    fwsts <- Tround(matrix(as.numeric(chrec.fw@recstats[,1])), column=1, digits=2)
    fwsts <- format(fwsts, justify="right")
    fwpvl <- format(chrec.fw@recstats[,2], justify="left")

    bwsmpls <- dimnames(chrec.bw@recstats)[[1]]
    bwsts <- Tround(matrix(as.numeric(chrec.bw@recstats[,1])), column=1, digits=2)
    bwsts <- format(bwsts, justify="right")
    bwpvl <- format(chrec.bw@recstats[,2], justify="left")

    Mout <- matrix(c(fwsmpls, fwsts, fwpvl,
                     bwsmpls, bwsts, bwpvl), ncol=6,
                   dimnames=list(rep("", length(bwsmpls)), c("Sample","Stats","","Sample","Stats","")))

    Tout <- xtable(x=as.data.frame(Mout), caption=caption, label=label, align=align,
                   vsep=vsep, digits=digits, display=display)

    print.xtable(x=Tout, type=type, file=outfile, append=append, floating=floating,
      table.placement=table.placement, caption.placement=caption.placement,
      latex.environments=latex.environments, size=size, hline.after=hline.after)

    msg <- paste("The table has been saved at", outfile)
    tkmessageBox(title="xtable", message=msg, icon="info")
}
