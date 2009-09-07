# FUCIONES GUI

# MENU ARCHIVO

OpenSourceFile <- function()
{
   sourcefile <- tclvalue(
    tkgetOpenFile(filetypes='{"R Code and text files" {".txt" ".TXT" ".R"}} {"All Files" {"*"}}'))
    if(!nchar(sourcefile))
       tkmessageBox(message="No file was chosen.", icon="error")
    else
       source(sourcefile)
}

SaveAsWorkSpace <- function()
{
  savename <- tclvalue(tkgetSaveFile(filetypes='{"R files" {".RData"}} {"All Files" {"*"}}'))
  if(!nchar(savename))
    tkmessageBox(message="No file was chosen.", icon="error")
  else
    save(file=savename, list=ls(all=TRUE))
}

LoadWorkSpace <- function()
{
   wsfile <- tclvalue(tkgetOpenFile(filetypes='{"R files" {".RData"}} {"All Files" {"*"}}'))
   if(!nchar(wsfile))
      tkmessageBox(message="No file was chosen.", icon="error")
   else
      load(wsfile)
}

closerusea <- function()
{
    response <- tclvalue(tkmessageBox(message="Quit uroot?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    else{
      tkdestroy(.tt)
      rm(.tt, .treeWidget, envir=.GlobalEnv)
        }
}

# MENU DATOS

dataIKERBIDE <- function()
{
  ttbd <- tktoplevel()
  tkwm.title(ttbd, "CAPV data bank")

  airp    <- tkradiobutton(ttbd)
  epaact  <- tkradiobutton(ttbd)
  iai     <- tkradiobutton(ttbd)
  ipc     <- tkradiobutton(ttbd)
  ipi     <- tkradiobutton(ttbd)
  ipri    <- tkradiobutton(ttbd)
  licof   <- tkradiobutton(ttbd)
  mtur    <- tkradiobutton(ttbd)
  mvic    <- tkradiobutton(ttbd)
  paroreg <- tkradiobutton(ttbd)
  pcemen  <- tkradiobutton(ttbd)
  pernhot <- tkradiobutton(ttbd)
  valorpr <- tkradiobutton(ttbd)
  vpoinic <- tkradiobutton(ttbd)
  vpoterm <- tkradiobutton(ttbd)

  serieValue <- tclVar(" ")
  tkconfigure(airp, variable=serieValue, value="1")
  tkconfigure(epaact, variable=serieValue, value="2")
  tkconfigure(iai, variable=serieValue, value="3")
  tkconfigure(ipc, variable=serieValue, value="4")
  tkconfigure(ipi, variable=serieValue, value="5")
  tkconfigure(ipri, variable=serieValue, value="6")
  tkconfigure(licof, variable=serieValue, value="7")
  tkconfigure(mtur, variable=serieValue, value="8")
  tkconfigure(mvic, variable=serieValue, value="9")
  tkconfigure(paroreg, variable=serieValue, value="10")
  tkconfigure(pcemen, variable=serieValue, value="11")
  tkconfigure(pernhot, variable=serieValue, value="12")
  tkconfigure(valorpr, variable=serieValue, value="13")
  tkconfigure(vpoinic, variable=serieValue, value="14")
  tkconfigure(vpoterm, variable=serieValue, value="15")

  tkgrid(tklabel(ttbd, text="Select a series:", fg="blue"))
  tkgrid(tklabel(ttbd, text="  Airpassengers"), airp, sticky="w")
  tkgrid(tklabel(ttbd, text="  Actives"), epaact, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial activity index"), iai, sticky="w")
  tkgrid(tklabel(ttbd, text="  Consumer price index"), ipc, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial production index"), ipi, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial price index"), ipri, sticky="w")
  tkgrid(tklabel(ttbd, text="  Official bidding"), licof, sticky="w")
  tkgrid(tklabel(ttbd, text="  Private car registration"), mtur, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial cargo vehicles registration"), mvic, sticky="w")
  tkgrid(tklabel(ttbd, text="  Registered unemployment"), paroreg, sticky="w")
  tkgrid(tklabel(ttbd, text="  Cement production"), pcemen, sticky="w")
  tkgrid(tklabel(ttbd, text="  Hotel occupation"), pernhot, sticky="w")
  tkgrid(tklabel(ttbd, text="  Production value"), valorpr, sticky="w")
  tkgrid(tklabel(ttbd, text="  Initiated Council houses"), vpoinic, sticky="w")
  tkgrid(tklabel(ttbd, text="  Finished Council houses"), vpoterm, sticky="w")

  Descrbut <- function()
  {
     #mytkpager(file.path(R.home(), "library/uroot/data/source_capv"),
     mytkpager(system.file("data", "source_capv", package="uroot"),
       title="CAPV Data Bank", header="", delete.file=FALSE, wwidth=70, wheight=20, export=FALSE)
  }
  Descr.but <- tkbutton(ttbd, text="Source", command=Descrbut)
  OnOK <- function()
  {
     data("mccapv")
     wts <- mccapv[[as.numeric(tclvalue(serieValue))]]

     labels <- c("airp", "epaact", "iai", "ipc", "ipi", "ipri", "licof", "mtur", "mvic",
                 "paroreg", "pcemen", "pernhot", "valorpr", "vpoinic", "vpoterm")
     label <- as.character(labels[as.numeric(tclvalue(serieValue))])

     assign(label, wts, env=.GlobalEnv)
     assign(".wts", wts, env=.GlobalEnv)

     tkinsert(.treeWidget,"end","root",label,text=label)

     msg <- paste("Information about the series has been stored in the object ", label, sep="")
     tkmessageBox(title="Series info", message=msg, icon="info")
     tkdestroy(ttbd)
  }
  OK.but <- tkbutton(ttbd, text="OK", command=OnOK)
  tkgrid(Descr.but)
  tkgrid(OK.but)
}

#

dataINE <- function()
{
  ttbd <- tktoplevel()
  tkwm.title(ttbd, "INE data bank")

  costesal <- tkradiobutton(ttbd)
  indcsal  <- tkradiobutton(ttbd)
  ipc      <- tkradiobutton(ttbd)
  ipi      <- tkradiobutton(ttbd)
  ipri     <- tkradiobutton(ttbd)
  mtur     <- tkradiobutton(ttbd)
  mvic     <- tkradiobutton(ttbd)
  paroreg  <- tkradiobutton(ttbd)
  pernhot  <- tkradiobutton(ttbd)
  valojhot <- tkradiobutton(ttbd)
  vpoinic  <- tkradiobutton(ttbd)
  vpoterm  <- tkradiobutton(ttbd)
  epaact   <- tkradiobutton(ttbd)

  serieValue <- tclVar(" ")
  tkconfigure(costesal, variable=serieValue, value="1")
  tkconfigure(indcsal, variable=serieValue, value="2")
  tkconfigure(ipc, variable=serieValue, value="3")
  tkconfigure(ipi, variable=serieValue, value="4")
  tkconfigure(ipri, variable=serieValue, value="5")
  tkconfigure(mtur, variable=serieValue, value="6")
  tkconfigure(mvic, variable=serieValue, value="7")
  tkconfigure(paroreg, variable=serieValue, value="8")
  tkconfigure(pernhot, variable=serieValue, value="9")
  tkconfigure(valojhot, variable=serieValue, value="10")
  tkconfigure(vpoinic, variable=serieValue, value="11")
  tkconfigure(vpoterm, variable=serieValue, value="12")
  tkconfigure(epaact, variable=serieValue, value="13")

  tkgrid(tklabel(ttbd, text="Select a series:", fg="blue"))
  tkgrid(tklabel(ttbd, text="  Wage cost"), costesal, sticky="w")
  tkgrid(tklabel(ttbd, text="  Wage cost index"), indcsal, sticky="w")
  tkgrid(tklabel(ttbd, text="  Consumer price index"), ipc, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial production index"), ipi, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial price index"), ipri, sticky="w")
  tkgrid(tklabel(ttbd, text="  Private car registration"), mtur, sticky="w")
  tkgrid(tklabel(ttbd, text="  Industrial cargo vehicles registration"), mvic, sticky="w")
  tkgrid(tklabel(ttbd, text="  Registered unemployment"), paroreg, sticky="w")
  tkgrid(tklabel(ttbd, text="  Hotel occupation"), pernhot, sticky="w")
  tkgrid(tklabel(ttbd, text="  Travellers lodged in hotels"), valojhot, sticky="w")
  tkgrid(tklabel(ttbd, text="  Initiated council houses"), vpoinic, sticky="w")
  tkgrid(tklabel(ttbd, text="  Finished council houses"), vpoterm, sticky="w")
  tkgrid(tklabel(ttbd, text="  Actives"), epaact, sticky="w")

  Descrbut <- function()
  {
    #mytkpager(file.path(R.home(), "library/uroot/data/source_es"),
    mytkpager(system.file("data", "source_es", package="uroot"),
       title="INE Data Bank", header="", delete.file=FALSE, wwidth=60, wheight=10, export=FALSE)
  }
  Descr.but <- tkbutton(ttbd, text="Source", command=Descrbut)
  OnOK <- function()
  {
     data("mces")
     wts <- mces[[as.numeric(tclvalue(serieValue))]]     

     labels <- c("costesal", "indcsal", "ipc", "ipi", "ipri", "mtur", "mvic", "paroreg",
                  "pernhot", "valojhot", "vpoinic", "vpoterm", "epaact")
     label <- as.character(labels[as.numeric(tclvalue(serieValue))])

     assign(label, wts, env=.GlobalEnv)
     assign(".wts", wts, env=.GlobalEnv)

     tkinsert(.treeWidget,"end","root",label,text=label)

     msg <- paste("Information about the series has been stored in the object ", label, sep="")
     tkmessageBox(title="Series info", message=msg, icon="info")
     tkdestroy(ttbd)
  }
  OK.but <- tkbutton(ttbd, text="OK", command=OnOK)
  tkgrid(Descr.but)
  tkgrid(OK.but)
}

#

dataFranses <- function()
{
  ttbd <- tktoplevel()
  tkwm.title(ttbd, "Data bank")

  usaipi    <- tkradiobutton(ttbd)
  canun     <- tkradiobutton(ttbd)
  gergnp    <- tkradiobutton(ttbd)
  ukinvest  <- tkradiobutton(ttbd)
  usaipisa  <- tkradiobutton(ttbd)
  canunsa   <- tkradiobutton(ttbd)
  gergnpsa  <- tkradiobutton(ttbd)
  ukgdp     <- tkradiobutton(ttbd)
  ukcons    <- tkradiobutton(ttbd)
  ukndcons  <- tkradiobutton(ttbd)
  ukexp     <- tkradiobutton(ttbd)
  ukimp     <- tkradiobutton(ttbd)
  ukpinvest <- tkradiobutton(ttbd)
  ukwf      <- tkradiobutton(ttbd)
  swndcpc   <- tkradiobutton(ttbd)
  swdipc    <- tkradiobutton(ttbd)

  serieValue <- tclVar(" ")
  tkconfigure(usaipi, variable=serieValue, value="1")
  tkconfigure(canun, variable=serieValue, value="2")
  tkconfigure(gergnp, variable=serieValue, value="3")
  tkconfigure(ukinvest, variable=serieValue, value="4")
  tkconfigure(usaipisa, variable=serieValue, value="5")
  tkconfigure(canunsa, variable=serieValue, value="6")
  tkconfigure(gergnpsa, variable=serieValue, value="7")
  tkconfigure(ukgdp, variable=serieValue, value="8")
  tkconfigure(ukcons, variable=serieValue, value="9")
  tkconfigure(ukndcons, variable=serieValue, value="10")
  tkconfigure(ukexp, variable=serieValue, value="11")
  tkconfigure(ukimp, variable=serieValue, value="12")
  tkconfigure(ukpinvest, variable=serieValue, value="13")
  tkconfigure(ukwf, variable=serieValue, value="14")
  tkconfigure(swndcpc, variable=serieValue, value="15")
  tkconfigure(swdipc, variable=serieValue, value="16")

  tkgrid(tklabel(ttbd, text="Select a series:", fg="blue"))
  tkgrid(tklabel(ttbd, text="  Total Industrial Production Index for the United States"), usaipi, sticky="w")
  tkgrid(tklabel(ttbd, text="  Unemployment in Canada"), canun, sticky="w")
  tkgrid(tklabel(ttbd, text="  Real GNP in Germany"), gergnp, sticky="w")
  tkgrid(tklabel(ttbd, text="  Real Total Investment in the United Kindom"), ukinvest, sticky="w")
  tkgrid(tklabel(ttbd, text="  Total Industrial Production Index for the United States (seasonally adjusted)"),
           usaipisa, sticky="w")
  tkgrid(tklabel(ttbd, text="  Unemployment in Canada (seasonally adjusted)"), canunsa, sticky="w")
  tkgrid(tklabel(ttbd, text="  Real GNP in Germany (seasonally adjusted)"), gergnpsa, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kingdom gross domestic product"), ukgdp, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kingdom total consumption"), ukcons, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kindom nondurables consumption"), ukndcons, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kindom exports of goods and services"), ukexp, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kindom imports of goods and services"), ukimp, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kindom public investment"), ukpinvest, sticky="w")
  tkgrid(tklabel(ttbd, text="  United Kindom workforce"), ukwf, sticky="w")
  tkgrid(tklabel(ttbd, text="  Real per capita non-durables consumption in Sweden (measured in logs)"),
           swndcpc, sticky="w")
  tkgrid(tklabel(ttbd, text="  Real per capita disposable income in Sweden (measured in logs)"),
           swdipc, sticky="w")

  Descrbut <- function()
  {
    #mytkpager(file.path(R.home(), "library/uroot/data/source_franses"),
    mytkpager(system.file("data", "source_franses", package="uroot"),
       title="Franses Data Bank", header="", delete.file=FALSE, wwidth=95, wheight=40, export=FALSE)
  }
  Descr.but <- tkbutton(ttbd, text="Source", command=Descrbut)
  OnOK <- function()
  {
     data("mcmisc")
     wts <- mcmisc[[as.numeric(tclvalue(serieValue))]]

     labels <-  c("usaipi", "canun", "gergnp", "ukinvest", "usaipisa", "canunsa", "gergnpsa",
                   "ukgdp", "ukcons", "ukndcons", "ukexp", "ukimp", "ukpinvest", "ukwf",
                   "swndcpc", "swdipc")
     label <- as.character(labels[as.numeric(tclvalue(serieValue))])

     assign(label, wts, env=.GlobalEnv)
     assign(".wts", wts, env=.GlobalEnv)

     tkinsert(.treeWidget,"end","root",label,text=label)

     msg <- paste("Information about the series has been stored in the object ", label, sep="")
     tkmessageBox(title="Series info", message=msg, icon="info")
     tkdestroy(ttbd)
  }
  OK.but <- tkbutton(ttbd, text="OK", command=OnOK)
  tkgrid(Descr.but)
  tkgrid(OK.but)
}

# MENU CONTRATES

MakeADF.test <- function()
{
  ttadf  <- tktoplevel()
  tkwm.title(ttadf, "ADF test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  N <- length(.wts)

  tkgrid(tklabel(ttadf, text="  Select deterministic components:", fg="blue"), sticky="w")
  Cte  <- tkcheckbutton(ttadf)
  Tdl  <- tkcheckbutton(ttadf)
  Vfet <- tkcheckbutton(ttadf)

  CteValue  <- tclVar("0")
  TdlValue  <- tclVar("0")
  VfetValue <- tclVar("0")

  tkconfigure(Cte, variable=CteValue)
    tkgrid(tklabel(ttadf, text="Intercept"), Cte)
  tkconfigure(Tdl, variable=TdlValue)
    tkgrid(tklabel(ttadf, text="Trend"), Tdl)
  tkconfigure(Vfet, variable=VfetValue)
    tkgrid(tklabel(ttadf, text="Seasonal dummys"), Vfet)

  rb1 <- tkradiobutton(ttadf)
  rb2 <- tkradiobutton(ttadf)
  rb3 <- tkradiobutton(ttadf)
  rb4 <- tkradiobutton(ttadf)
  rb5 <- tkradiobutton(ttadf)
  rb6 <- tkradiobutton(ttadf)
  rbValue <- tclVar("BIC")
  yself <- tclVar()
  entry.yself <- tkentry(ttadf, width="3", textvariable=yself)

  tkconfigure(rb3, variable=rbValue, value="AIC")
  tkconfigure(rb4, variable=rbValue, value="BIC")
  tkconfigure(rb5, variable=rbValue, value="Signf")
  tkconfigure(rb6, variable=rbValue, value="Tu mismo")

  tkgrid(tklabel(ttadf, text="  Select the method for choosing lags:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttadf, text="AIC-top-down"), rb3)
  tkgrid(tklabel(ttadf, text="BIC-top-down"), rb4)
  tkgrid(tklabel(ttadf, text="Significant lags"), rb5)
  #tkgrid(tklabel(ttadf, text="By yourself"), rb6, entry.yself)

  #Definir1 <- tkbutton(ttadf, text="Define", command=Makevfic)
  #Definir2 <- tkbutton(ttadf, text="Define", command=MakeVFEp)
  #Mvfic <<- matrix(NA, nrow=N, ncol=6)
  #tkgrid(tklabel(ttadf, text="  Include dummys:", fg="blue"), sticky="w")
  #tkgrid(tklabel(ttadf, text="  Generic dummy           "), Definir1)
  ##VFEp <<- 0; done <<- tclVar(0)
  #tkgrid(tklabel(ttadf, text="Partial seasonal dummy      "), Definir2)

  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
      tkdestroy(ttadf)
      #ifelse(exists("VFEp"), VFEp <- VFEp, VFEp <<- 0)

      mVal <- as.character(tclvalue(CteValue))
      if(mVal =="1")
         Ct <- 1
      if(mVal =="0")
         Ct <- 0
      mVal <- as.character(tclvalue(TdlValue))
      if(mVal =="1")
         TD <- 1
      if(mVal =="0")
         TD <- 0
      mVal <- as.character(tclvalue(VfetValue))
      if(mVal =="1")
         Vfe <- 1:(frequency(.wts)-1)
      if(mVal =="0")
         Vfe <- 0

      # SelecP
      rbVal <- as.character(tclvalue(rbValue))
      if(tclvalue(rbValue) == "AIC")
        selecP <- list(mode="aic", Pmax=NULL)
       if(tclvalue(rbValue) == "BIC")
        selecP <- list(mode="bic", Pmax=NULL)
      if(tclvalue(rbValue) == "Signf")
        selecP <- list(mode="signf", Pmax=NULL)
      if(tclvalue(rbValue) == "Tu mismo")
        selecP <- as.numeric(tclvalue(yself))

      # VFIC
      #aux <- length(which(Mvfic[1,] >= 0))
      #ifelse(aux == 0, Mvfic <<- 0, Mvfic <<- as.matrix(Mvfic[,1:aux]))
      .out <<- ADF.test(.wts, itsd=c(Ct,TD,Vfe), regvar=0, selectlags=selecP)
      show(.out)

      tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttadf,text="OK",command=OnOK)
  tkgrid(OK.but)
}

#

MakeKPSS.test <- function()
{
  ttkpss <- tktoplevel()
  tkwm.title(ttkpss, "KPSS test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  ltrunc <- tclVar(as.integer(3*sqrt(length(.wts))/13))
  tkgrid(tklabel(ttkpss, text="  Introduce the lag truncation parameter: \n (By default 3*sqrt(N)/13)",
       fg="blue"), rowspan=2)
  entry.ltrunc <- tkentry(ttkpss, width="5", textvariable=ltrunc)
  tkgrid(entry.ltrunc)

  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
     tkdestroy(ttkpss)
     .out <<- KPSS.test(.wts, ltrunc=as.numeric(tclvalue(ltrunc)))
     show(.out)
     tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttkpss, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

MakeCH.test <- function()
{
  ttch <- tktoplevel()
  tkwm.title(ttch, "CH test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  trend  <- tkcheckbutton(ttch);  trendValue  <- tclVar("0")
  lag1   <- tkcheckbutton(ttch);  lag1Value  <- tclVar("1")

  tkgrid(tklabel(ttch, text="  Select the elements to include in the auxiliar regression:",
        fg="blue"), sticky="w")
  tkconfigure(trend, variable=trendValue)
    tkgrid(tklabel(ttch, text="Trend"), trend)
  tkconfigure(lag1, variable=lag1Value)
    tkgrid(tklabel(ttch, text="First order lag"), lag1)

  #rb1 <- tkradiobutton(ttch)
  #rb2 <- tkradiobutton(ttch)
  rbValue <- tclVar("nsumm")
  #tkconfigure(rb1, variable = rbValue, value = "nsumm")
  #tkconfigure(rb2, variable = rbValue, value = "summ")

  tkgrid(tklabel(ttch, text = "   ---   ---   --- "))
  #tkgrid(tklabel(ttch, text = "  Show a summary.", fg="blue"), rb2, sticky="w")
  #tkgrid(tklabel(ttch, text = "  Analyse selected frequencies:", fg="blue"), rb1, sticky="w")
  tkgrid(tklabel(ttch, text = "  Analyse selected frequencies:", fg="blue"), sticky="w")

  if(frequency(.wts)==12){
    pi6  <- tkcheckbutton(ttch)
    pi3  <- tkcheckbutton(ttch)
    pi23 <- tkcheckbutton(ttch)
    pi56 <- tkcheckbutton(ttch)
           }
  pi2  <- tkcheckbutton(ttch)
  pi   <- tkcheckbutton(ttch)

  if(frequency(.wts)==12){
    pi6Value  <- tclVar("0")
    pi3Value  <- tclVar("0")
    pi23Value <- tclVar("0")
    pi56Value <- tclVar("0")
           }
  pi2Value  <- tclVar("0")
  piValue   <- tclVar("0")

  if(frequency(.wts)==12){
    tkconfigure(pi6, variable=pi6Value)
    tkgrid(tklabel(ttch, text="pi/6"), pi6)
    tkconfigure(pi3, variable=pi3Value)
    tkgrid(tklabel(ttch, text="pi/3"), pi3)
  }
  tkconfigure(pi2, variable=pi2Value)
  tkgrid(tklabel(ttch, text="pi/2"), pi2)
  if(frequency(.wts)==12){
    tkconfigure(pi23, variable=pi23Value)
    tkgrid(tklabel(ttch, text="2pi/3"), pi23)
    tkconfigure(pi56, variable=pi56Value)
    tkgrid(tklabel(ttch, text="5pi/6"), pi56)
  }
  tkconfigure(pi, variable=piValue)
  tkgrid(tklabel(ttch, text="pi"), pi)

  #
  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
    tkdestroy(ttch)
    trendVal <- as.character(tclvalue(trendValue))
      if(trendVal =="1")
         DetTr <- TRUE
      if(trendVal =="0")
         DetTr <- FALSE
   lag1Val <- as.character(tclvalue(lag1Value))
      if(lag1Val =="1")
         f0 <- 1
      if(lag1Val =="0")
         f0 <- 0

    if (tclvalue(rbValue) == "nsumm")
    {
      frec <- rep(0, frequency(.wts)/2)
      if(frequency(.wts) == 12)
      {
        mVal <- as.character(tclvalue(pi6Value))
        if(mVal =="1")
           frec[1] <- 1
        if(mVal =="0")
           frec[1] <- 0
        mVal <- as.character(tclvalue(pi3Value))
        if(mVal =="1")
          frec[2] <- 1
        if(mVal =="0")
           frec[2] <- 0
        mVal <- as.character(tclvalue(pi23Value))
        if(mVal =="1")
           frec[4] <- 1
        if(mVal =="0")
           frec[4] <- 0
        mVal <- as.character(tclvalue(pi56Value))
        if(mVal =="1")
           frec[5] <- 1
        if(mVal =="0")
           frec[5] <- 0
      }

      if(frequency(.wts) == 12){ aux <- c(3,6) }
      if(frequency(.wts) == 4){ aux <- c(1,2) }
      mVal <- as.character(tclvalue(pi2Value))
      if(mVal =="1")
         frec[aux[1]] <- 1
      if(mVal =="0")
         frec[aux[1]] <- 0
      mVal <- as.character(tclvalue(piValue))
      if(mVal =="1")
         frec[aux[2]] <- 1
      if(mVal =="0")
         frec[aux[2]] <- 0

      .out <<- CH.test(.wts, frec, f0, DetTr=DetTr)
      show(.out)
    }
    if (tclvalue(rbValue) == "summ")
    {
      rdoCH <- c(1:(frequency(.wts)/2+1))
      frecf <- rep(0, frequency(.wts)/2)
      for(i in 1:(frequency(.wts)/2))
      {
         frecf[i] <- 1
         rdoCH[i] <- CH.test(.wts, frecf, f0, DetTr=DetTr)[1]
         frecf <- rep(0, frequency(.wts)/2)
     }
     rdoCH[(frequency(.wts)/2+1)] <-
            CH.test(.wts, rep(1, frequency(.wts)/2), f0, DetTr=DetTr)[1]
     lCH <- CH.test(.wts, rep(1, frequency(.wts)/2), f0, DetTr=FALSE)[2]
     CH  <- round(as.numeric(rdoCH), 2)
     if(frequency(.wts)==12)
         rdoch <- data.frame("f.pi.6"=CH[1], "f.pi.3"=CH[2], "f.pi.2"=CH[3], "f.2pi.3"=CH[4],
                             "f.5pi.6"=CH[5], "f.pi"=CH[6], "Joint-test"=CH[7])
      if(frequency(.wts)==4)
         rdoch <- data.frame("f.pi.2"=CH[5], "f.pi"=CH[6], "Joint-test"=CH[7])
      cat("\n ------ CH test ------ \n\n")
    }
    tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttch,text="OK", command=OnOK)
  tkgrid(OK.but)
}

MakeCHseas.test <- function()
{
  ttch <- tktoplevel()
  tkwm.title(ttch, "CH test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1 <- tkradiobutton(ttch)
  rb2 <- tkradiobutton(ttch)
  rbValue <- tclVar("nsumm")
  tkconfigure(rb1, variable = rbValue, value = "nsumm")
  tkconfigure(rb2, variable = rbValue, value = "summ")

  #tkgrid(tklabel(ttch, text = "  Show a summary.", fg="blue"), rb2, sticky="w")
  tkgrid(tklabel(ttch, text = " "))
  tkgrid(tklabel(ttch, text = "  Analyse an individual season.", fg="blue"), rb1, sticky="w")

  scr <- tkscrollbar(ttch, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(ttch,height=4,selectmode="single", yscrollcommand= function(...)tkset(scr,...),
                background="white")
  tkgrid(tklabel(ttch, text="    Select the season to analyse:"), sticky="w")
  tkgrid(tl, scr)
  tkgrid.configure(scr, rowspan=4, sticky="nsw")
  if(frequency(.wts)==4)
    seas <- c("Quarter 1", "Quarter 2 ", "Quarter 3", "Quarter 4")
  if(frequency(.wts)==12)
    seas <- c("January", "February", "March", "April", "May", "June", "July", "August",
              "September", "October", "November", "December")
  for (i in (1:12))
      tkinsert(tl, "end", seas[i])
  tkselection.set(tl, 1)

  Confirm <- function(){
     seasChoice <<- which(seas==seas[as.numeric(tkcurselection(tl))+1])
     tkmessageBox(title="CH input", message="Season has been selected.", icon="info")
     cat(c("\n Season selected:", seas[seasChoice], " \n\n"))
                       }
  Confirm.but <-tkbutton(ttch, text="Select", command=Confirm)
  tkgrid(Confirm.but)

  OnOK <- function()
  {
    if (tclvalue(rbValue) == "summ")
      rdoCH <- CHseas.test(.wts, 4, c(1:frequency(.wts)), showcat=TRUE)
    if (tclvalue(rbValue) == "nsumm"){
      rdoCH <- CHseas.test(.wts, 4, seasChoice, showcat=TRUE)
      rm(seasChoice)
                                    }
    tkdestroy(ttch)
    tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttch,text="OK",command=OnOK)
  tkgrid(tklabel(ttch, text = " "))
  tkgrid(OK.but)
}

#

MakeHEGY.test <- function()
{                                # anyadir selecP <- "v1"
  tthegybm  <- tktoplevel()
  tkwm.title(tthegybm, "HEGY test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  N <- length(.wts)

  tkgrid(tklabel(tthegybm, text="  Select the deterministic components:", fg="blue"), sticky="w")

  Cte  <- tkcheckbutton(tthegybm)
  Tdl  <- tkcheckbutton(tthegybm)
  Vfet <- tkcheckbutton(tthegybm)

  CteValue  <- tclVar("0")
  TdlValue  <- tclVar("0")
  VfetValue <- tclVar("0")

  tkconfigure(Cte, variable=CteValue)
    tkgrid(tklabel(tthegybm, text="Intercept"), Cte)
  tkconfigure(Tdl, variable=TdlValue)
    tkgrid(tklabel(tthegybm, text="Trend"), Tdl)
  tkconfigure(Vfet, variable=VfetValue)
    tkgrid(tklabel(tthegybm, text="Seasonal dummys"), Vfet)

  rb1 <- tkradiobutton(tthegybm)
  rb2 <- tkradiobutton(tthegybm)
  rb5 <- tkradiobutton(tthegybm)
  rbValue <- tclVar("BIC")
  yself1 <- tclVar()
  yself2 <- tclVar()
  yself3 <- tclVar()
  yself4 <- tclVar()
  entry.yself1 <- tkentry(tthegybm, width="3", textvariable=yself1)
  entry.yself2 <- tkentry(tthegybm, width="3", textvariable=yself2)
  entry.yself3 <- tkentry(tthegybm, width="3", textvariable=yself3)
  entry.yself4 <- tkentry(tthegybm, width="3", textvariable=yself4)
  tkconfigure(rb1, variable=rbValue, value="AIC")
  tkconfigure(rb2, variable=rbValue, value="BIC")
  tkconfigure(rb5, variable=rbValue, value="Signf")
  tkgrid(tklabel(tthegybm, text="  Select the method for choosing lags:", fg="blue"), sticky="w")
  tkgrid(tklabel(tthegybm, text="AIC-top-down"), rb1)
  tkgrid(tklabel(tthegybm, text="BIC-top-down"), rb2)
  tkgrid(tklabel(tthegybm, text="Significant lags"), rb5)

  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
      tkdestroy(tthegybm)
      #ifelse(exists("VFEp"), VFEp <- VFEp, VFEp <<- 0)

      mVal <- as.character(tclvalue(CteValue))
      if(mVal =="1")
         Ct <- 1
      if(mVal =="0")
         Ct <- 0
      mVal <- as.character(tclvalue(TdlValue))
      if(mVal =="1")
         TD <- 1
      if(mVal =="0")
         TD <- 0
      mVal <- as.character(tclvalue(VfetValue))
      if(mVal =="1")
         Vfe <- 1:(frequency(.wts)-1)
      if(mVal =="0")
         Vfe <- 0

      # SelecP
      rbVal <- as.character(tclvalue(rbValue))
      if(tclvalue(rbValue) == "AIC")
        selecP <- list(mode="aic", Pmax=NULL)
       if(tclvalue(rbValue) == "BIC")
        selecP <- list(mode="bic", Pmax=NULL)
      if(tclvalue(rbValue) == "Signf")
        selecP <- list(mode="signf", Pmax=NULL)
      if(tclvalue(rbValue) == "Tu mismo")
        selecP <- c(as.numeric(tclvalue(yself1)), as.numeric(tclvalue(yself2)),
                    as.numeric(tclvalue(yself3)), as.numeric(tclvalue(yself4)))

      # VFIC
      #aux <- length(which(Mvfic[1,] >= 0))
      #ifelse(aux == 0, Mvfic <<- 0, Mvfic <<- as.matrix(Mvfic[,1:aux]))

      .out <<- HEGY.test(.wts, itsd=c(Ct,TD,Vfe), regvar=0, selectlags=selecP)
      show(.out)
      #cleanlist <- list(Mvfic=Mvfic, VFEp=VFEp)
      #clean.auxobjects(cleanlist, type="")
      tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(tthegybm,text="OK",command=OnOK)
  tkgrid(OK.but)
}


# MENU DATOS
# modo menu, "DataInfo" ó ""SDGP"

GetDataInfo <- function()   # Datos-Descripción
{
  ttdinfo  <- tktoplevel()
  tkwm.title(ttdinfo, "Description")

  tkgrid(tklabel(ttdinfo, text="Please, describe the data.", fg="blue"), sticky="w")

  nombre <- tclVar()
  tkgrid(tklabel(ttdinfo, text="  Series label:", fg="blue"), sticky="w")
  entry.nombre <- tkentry(ttdinfo, width="10", textvariable=nombre)
  tkgrid(entry.nombre)

  rb1     <- tkradiobutton(ttdinfo)
  rb2     <- tkradiobutton(ttdinfo)
  rb3     <- tkradiobutton(ttdinfo)
  rbValue <- tclVar("Trimestral")
  tkconfigure(rb1, variable=rbValue, value="Trimestral")
  tkconfigure(rb2, variable=rbValue, value="Mensual")
  tkconfigure(rb3, variable=rbValue, value="Anual")
  tkgrid(tklabel(ttdinfo, text="  Register the periodicity of the series:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttdinfo, text="Quarterly"), rb1)
  tkgrid(tklabel(ttdinfo, text="Monthly"), rb2)
  tkgrid(tklabel(ttdinfo, text="Anual"), rb3)

  s0 <- tclVar()
  a0 <- tclVar()

  entry.a0 <- tkentry(ttdinfo, width="4", textvariable=a0)
  entry.s0 <- tkentry(ttdinfo, width="2", textvariable=s0)
  tkgrid(tklabel(ttdinfo, text="  Introduce the year and season of the", fg="blue"), sticky="w")
  tkgrid(tklabel(ttdinfo, text="     first observation:", fg="blue"), entry.a0, entry.s0, sticky="w")

  rblog1 <- tkradiobutton(ttdinfo)
  rblog2 <- tkradiobutton(ttdinfo)
  rblogValue <- tclVar("Original data")
  tkconfigure(rblog1, variable=rblogValue, value="Original data")
  tkconfigure(rblog2, variable=rblogValue, value="Logarithms")
  tkgrid(tklabel(ttdinfo, text="  Indicate the scale of the data:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttdinfo, text="Original data"), rblog1)
  tkgrid(tklabel(ttdinfo, text="Logarithms"), rblog2)

  OnOK <- function()
  {
     label <- as.character(tclvalue(nombre))

     #periodicidad
     rbVal <- as.character(tclvalue(rbValue))
     tkdestroy(ttdinfo)
     if(tclvalue(rbValue) == "Trimestral")
        s <- 4
     if(tclvalue(rbValue) == "Mensual")
        s <- 12
     if(tclvalue(rbValue) == "Anual")
        s <- 1

     if(tclvalue(rblogValue) == "Original data")
        logvari <- FALSE
     if(tclvalue(rblogValue) == "Logarithms")
        logvari <- TRUE

     t0    <- rep(0, 2)
     t0[1] <- as.numeric(tclvalue(a0))
     t0[2] <- as.numeric(tclvalue(s0))
     wts <- ts(wts, frequency=s, start=c(t0[1],t0[2]))
     N <- length(wts)
     logvari <- FALSE # para gráficos (títulos),

     assign(label, wts, env=.GlobalEnv)
     assign(".wts", wts, env=.GlobalEnv)
     tkinsert(.treeWidget,"end","root",label,text=label)

     tempf <- tempfile()
     cat("rm(datos, wts)", file=tempf)
     eval(source(tempf))
     tkcmd("file", "delete", tempf)

     msg <- paste("Information about the series has been stored in the object ",
                  label, "\n", sep="")
     tkmessageBox(title="Series info", message=msg, icon="info")
     tkdestroy(ttdinfo)
  }
  OK.but <- tkbutton(ttdinfo, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#
MakeDGPsim <- function()
{
  ttdgp  <- tktoplevel()
  tkwm.title(ttdgp, "Simulate DGP")

  rb1     <- tkradiobutton(ttdgp)
  rb2     <- tkradiobutton(ttdgp)
  rb3     <- tkradiobutton(ttdgp)
  rbValue <- tclVar("Trimestral")
  tkconfigure(rb1, variable=rbValue, value="Trimestral")
  tkconfigure(rb2, variable=rbValue, value="Mensual")
  tkconfigure(rb3, variable=rbValue, value="Anual")
  tkgrid(tklabel(ttdgp, text="  Introduce the periodicity of the series:",
      fg="blue"), sticky="w")
  tkgrid(tklabel(ttdgp, text="Quarterly"), rb1)
  tkgrid(tklabel(ttdgp, text="Monthly"), rb2)
  tkgrid(tklabel(ttdgp, text="Anual"), rb3)

  N <- tclVar()
  entry.N <- tkentry(ttdgp, width="5", textvariable=N)
  tkgrid(tklabel(ttdgp, text="  Introduce the number of observations:",
      fg="blue"), sticky="w")
  tkgrid(entry.N)

  phi1Var    <- tclVar(" ")
  phi2Var    <- tclVar(" ")
  phi3Var    <- tclVar(" ")
  phi4Var    <- tclVar(" ")
  retphi1Var <- tclVar(" ")
  retphi2Var <- tclVar(" ")
  retphi3Var <- tclVar(" ")
  retphi4Var <- tclVar(" ")
  rho1Var    <- tclVar(" ")
  rho2Var    <- tclVar(" ")
  rho3Var    <- tclVar(" ")
  rho4Var    <- tclVar(" ")
  retrho1Var <- tclVar(" ")
  retrho2Var <- tclVar(" ")
  retrho3Var <- tclVar(" ")
  retrho4Var <- tclVar(" ")

  phi1Entry    <- tkentry(ttdgp, width="3", textvariable=phi1Var)
  phi2Entry    <- tkentry(ttdgp, width="3", textvariable=phi2Var)
  phi3Entry    <- tkentry(ttdgp, width="3", textvariable=phi3Var)
  phi4Entry    <- tkentry(ttdgp, width="3", textvariable=phi4Var)
  retphi1Entry <- tkentry(ttdgp, width="3", textvariable=retphi1Var)
  retphi2Entry <- tkentry(ttdgp, width="3", textvariable=retphi2Var)
  retphi3Entry <- tkentry(ttdgp, width="3", textvariable=retphi3Var)
  retphi4Entry <- tkentry(ttdgp, width="3", textvariable=retphi4Var)
  rho1Entry    <- tkentry(ttdgp, width="3", textvariable=rho1Var)
  rho2Entry    <- tkentry(ttdgp, width="3", textvariable=rho2Var)
  rho3Entry    <- tkentry(ttdgp, width="3", textvariable=rho3Var)
  rho4Entry    <- tkentry(ttdgp, width="3", textvariable=rho4Var)
  retrho1Entry <- tkentry(ttdgp, width="3", textvariable=retrho1Var)
  retrho2Entry <- tkentry(ttdgp, width="3", textvariable=retrho2Var)
  retrho3Entry <- tkentry(ttdgp, width="3", textvariable=retrho3Var)
  retrho4Entry <- tkentry(ttdgp, width="3", textvariable=retrho4Var)

  tkgrid(tklabel(ttdgp,
  text="  Fill in the following information.", fg="blue"), sticky="w")

  tkgrid(tklabel(ttdgp, text="Autorregresive lags:"))
  tkgrid(tklabel(ttdgp, text="Coefficient:"), phi1Entry, phi2Entry, phi3Entry, phi4Entry)
  tkgrid(tklabel(ttdgp, text="Lag:"), retphi1Entry, retphi2Entry, retphi3Entry, retphi4Entry)
  tkgrid(tklabel(ttdgp, text="Moving average lags:"))
  tkgrid(tklabel(ttdgp, text="Coefficient:"), rho1Entry, rho2Entry, rho3Entry, rho4Entry)
  tkgrid(tklabel(ttdgp, text="Lag:"), retrho1Entry, retrho2Entry, retrho3Entry, retrho4Entry)

  OnOK <- function()
  {
     rbVal <- as.character(tclvalue(rbValue))
     tkdestroy(ttdgp)
     if(tclvalue(rbValue) == "Trimestral")
        s <- 4
     if(tclvalue(rbValue) == "Mensual")
        s <- 12
     if(tclvalue(rbValue) == "Anual")
        s <- 1
     N <- as.numeric(tclvalue(N))
     phi    <- rbind(na.omit(c( as.numeric(tclvalue(phi1Var)), as.numeric(tclvalue(phi2Var)),
                   as.numeric(tclvalue(phi3Var)), as.numeric(tclvalue(phi4Var)) )))
     retphi <- rbind(na.omit(c( as.numeric(tclvalue(retphi1Var)),
                   as.numeric(tclvalue(retphi2Var)), as.numeric(tclvalue(retphi3Var)),
                   as.numeric(tclvalue(retphi4Var)) )))
     rho    <- na.omit(c( as.numeric(tclvalue(rho1Var)), as.numeric(tclvalue(rho2Var)),
                  as.numeric(tclvalue(rho3Var)), as.numeric(tclvalue(rho4Var)) ))
     retrho <- na.omit(c( as.numeric(tclvalue(retrho1Var)), as.numeric(tclvalue(retrho2Var)),
                  as.numeric(tclvalue(retrho3Var)), as.numeric(tclvalue(retrho4Var)) ))
     wts <- DGPsim(s, N, phi, retphi, rho, retrho); t0 <- c(0,1) ;label <- "DGP"

     assign(label, wts, env=.GlobalEnv)
     assign(".wts", wts, env=.GlobalEnv)
     tkinsert(.treeWidget,"end","root",label,text=label)

     msg <- paste("Data generating process information has been stored in the object ",
                  label, sep="")
     tkmessageBox(title="Series info", message=msg, icon="info")
     tkdestroy(ttdgp)
  }
  OK.but <- tkbutton(ttdgp, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

ReadDataCSV <- function()
{
  tttxt <- tktoplevel()
  tkwm.title(tttxt, "Read data from csv file")

  espaciosep <- tkradiobutton(tttxt)
  comasep    <- tkradiobutton(tttxt)
  ptcomasep  <- tkradiobutton(tttxt)
  sepValue   <- tclVar(";")

  tkconfigure(espaciosep, variable=sepValue, value=" ")
  tkconfigure(comasep, variable=sepValue, value=",")
  tkconfigure(ptcomasep, variable=sepValue, value=";")
  tkgrid(tklabel(tttxt, text="  Separator character:", fg="blue"), sticky="w")
  tkgrid(tklabel(tttxt, text="White space"), espaciosep)
  tkgrid(tklabel(tttxt, text="Comma"), comasep)
  tkgrid(tklabel(tttxt, text="Semicolon"), ptcomasep)

  comadec  <- tkradiobutton(tttxt)
  puntodec <- tkradiobutton(tttxt)
  decValue <- tclVar(",")
  tkconfigure(comadec, variable=decValue, value=",")
  tkconfigure(puntodec, variable=decValue, value=".")
  tkgrid(tklabel(tttxt, text="  Character for decimal points:", fg="blue"), sticky="w")
  tkgrid(tklabel(tttxt, text="Comma"), comadec)
  tkgrid(tklabel(tttxt, text="Dot"), puntodec)

  tkgrid(tklabel(tttxt, text="  The data file contain a header with the series names:",
         fg="blue"), sticky="w")
  header      <- tkcheckbutton(tttxt)
  headerValue <- tclVar("1")

  tkconfigure(header, variable=headerValue)
  tkgrid(tklabel(tttxt, text="With header"), header)

  datacolumn <- tclVar()
  tkgrid(tklabel(tttxt, text="  Introduce the column that contains the series to analyse:",
         fg="blue"), sticky="w")
  entry.datacolumn <- tkentry(tttxt, width="5", textvariable=datacolumn)
  tkgrid(entry.datacolumn)

  OnOK <- function(){
    tkdestroy(tttxt)
    datafile <- tclvalue(tkgetOpenFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}'))
    if(!nchar(datafile))
       tkmessageBox(message="No file was chosen.", icon="error")
    else
       tkmessageBox(message=paste("The file chosen is", datafile, "and has been stored in the object datos."), icon="info")
     mVal <- as.character(tclvalue(headerValue))
     if(mVal =="1")
        headerarg <- TRUE
     if(mVal =="0")
        headerarg <- FALSE

     datos <<- read.csv(datafile, header=headerarg, sep=tclvalue(sepValue),
                        dec=tclvalue(decValue), na.strings="NA")

     wtsaux <- datos[,as.numeric(tclvalue(datacolumn))]; # rm(datos)
     wts    <<- as.numeric(as.matrix(wtsaux))

     #msg <- paste("Don't forget describe the series (Menu-Data->Description)", sep="")
     #tkmessageBox(title="Series info", message=msg, icon="info")
     GetDataInfo()
   }
   OK.but <- tkbutton(tttxt, text="OK", command=OnOK)
   tkgrid(OK.but)
}

# Abrir SPSS

ReadSPSS <- function()
{
  name <- tclvalue(tkgetOpenFile(
                   filetypes="{{SPSS Files} {.sav}} {{All files} *}"))
  if (name=="") return;
  zz <- read.spss(name,use.value.label=T,to.data.frame=T)
  assign("myData", zz, envir=.GlobalEnv)
}

#

ysooys <- function(yso, t0, N, s)
{
  index <- matrix(-9999, ncol=3, nrow=N)
  index[,3] <- c(1:N)
  index[,2] <- c(c(t0[2]:s), rep(1:s,N/s))[1:N]
  index[1:length(t0[2]:s),1] <- rep(t0[1], length(t0[2]:s))
  i <- 1
  while(index[N,1] == -9999)
  {
     iaux <- which(index[,1] == -9999)
     reps <- ifelse(length(iaux) >= s, reps <- s, reps <- length(iaux))
     index[iaux[1]:(iaux[1]+(reps-1)),1] <- rep(t0[1]+i, reps)
     i <- i+1
  }

# year and season to observation
  if(length(yso)==2)
  {
    quest1 <- which(c(index[,1] == yso[1]) == TRUE)
    quest2 <- which(c(index[,2] == yso[2]) == TRUE)
     i <- 1; out <- quest1[1]
    while(length(which(quest2 == quest1[i])) != 1){
      i <- i+1
      out <- quest1[i]
    }
  }

# observation to year and season
  if(length(yso)==1)
    out <- index[which(index[,3]==yso),1:2]

  list(out=out, index=index)
}

#

CambiarPeriodo <- function()
{
  ttpmtr <- tktoplevel()
  tkwm.title(ttpmtr, "Change sample period")

  a02 <- tclVar()
  s02 <- tclVar()
  aN2 <- tclVar()
  sN2 <- tclVar()

  tkgrid(tklabel(ttpmtr, text="  New sample period:", fg="blue"), sticky="w")
  entry.a02 <- tkentry(ttpmtr, width="4", textvariable=a02)
  entry.s02 <- tkentry(ttpmtr, width="2", textvariable=s02)
  tkgrid(tklabel(ttpmtr, text="     Year and season of the first observation:"),
         entry.a02, entry.s02)
  entry.aN2 <- tkentry(ttpmtr, width="4", textvariable=aN2)
  entry.sN2 <- tkentry(ttpmtr, width="2", textvariable=sN2)
  tkgrid(tklabel(ttpmtr, text="     Year and season of the last observation:"),
         entry.aN2, entry.sN2)

  OnOK <- function(){
    tkdestroy(ttpmtr)
    string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
    .wts <<- ExeString(c(string, ""))

    #obs1 <- ysooys(c(as.numeric(tclvalue(a02)), as.numeric(tclvalue(s02))), start(.wts),
    #               length(.wts), frequency(.wts))[[1]]
    #obs2 <- ysooys(c(as.numeric(tclvalue(aN2)), as.numeric(tclvalue(sN2))), start(.wts),
    #               length(.wts), frequency(.wts))[[1]]
    t0   <- c(as.numeric(tclvalue(a02)), as.numeric(tclvalue(s02)))
    tN <- c(as.numeric(tclvalue(aN2)), as.numeric(tclvalue(sN2)))
    #wts <- ts(.wts[obs1:obs2], frequency=frequency(.wts), start=t0)
    wts <- window(.wts, start=c(as.numeric(tclvalue(a02)), as.numeric(tclvalue(s02))),
                          end=c(as.numeric(tclvalue(aN2)), as.numeric(tclvalue(sN2))))

    i <- 1; newstring <- "NULL"
    while(newstring == "NULL"){
      ifelse(exists(paste(string, "_subs", i, sep="")), i <- i+1,
             newstring <- paste(string, "_subs", i, sep=""))
    }
    assign(newstring, wts, env=.GlobalEnv)
    showlabel <- paste(newstring, "     ", t0[1], ".", t0[2], "-", tN[1], ".", tN[2], sep="")
    tkinsert(.treeWidget,"end", string, newstring, text=showlabel)

    #change.wts.label(c("t0", "t0cp", "wts", "N"))
    tkmessageBox(message="The sample period has changed.", icon="info")
                    }
  OK.but <- tkbutton(ttpmtr, text="OK", command=OnOK)
  tkgrid(OK.but)
}

# MENU GRÁFICOS
Makeplotwts <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  plot(.wts, xlab="", ylab="", las=1, main=.wts$label)
  print(.wts)
}
Makeplotlog    <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  plot(log(.wts, base=exp(1)), xlab="", ylab="", las=1, main="Logarithm of the series")
  print(log(.wts, base=exp(1)))
}
Makeplotboxcox <- function()
{
  ttplot <- tktoplevel()

  lambda  <- tclVar(0)
  tkgrid(tklabel(ttplot, text=" Introduce Box-Cox algorithm parameter:"), sticky="w")
  entry.lambda <- tkentry(ttplot, width="4", textvariable=lambda)
  tkgrid(tklabel(ttplot, text="lambda"), entry.lambda)

  OnOK <- function()
  {
    tkdestroy(ttplot)
    string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
    .wts <<- ExeString(c(string, ""))
    wtsp  <- ts(BoxCox(.wts, as.numeric(tclvalue(lambda))),
                 frequency=frequency(.wts), start=start(.wts))
    plot(wtsp, xlab="", ylab="", las=1, main="Box-Cox transformation")
    print(wtsp)
  }
  tkgrid(tkbutton(ttplot,text="OK",command=OnOK))
}
Makeboxcox <- function()
{
  ttbc <- tktoplevel()
  lambda  <- tclVar(0)
  tkgrid(tklabel(ttbc, text=" Introduce Box-Cox algorithm parameter:", fg="blue"), sticky="w")
  entry.lambda <- tkentry(ttbc, width="4", textvariable=lambda)
  tkgrid(tklabel(ttbc, text="lambda"), entry.lambda)
  OnOK <- function()
  {
     tkdestroy(ttbc)

     string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
     newstring <- paste("boxcox_", string, sep="")
     wtst     <- ts(BoxCox(.wts, as.numeric(tclvalue(lambda))),
                     frequency=frequency(.wts), start=start(.wts))
     assign(newstring, wtst, env=.GlobalEnv)
     tkinsert(.treeWidget,"end", string, newstring, text=newstring)

     #change.wts.label(c("label", "logvari", "t0", "varit", "vari", "N"))
     tkmessageBox(message="The scale has changed.", icon="info")
  }
  tkgrid(tkbutton(ttbc,text="OK",command=OnOK))
}
#
Makeplotdelta <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  s <- frequency(.wts)
  N <- length(.wts)
  t0 <- start(.wts)

  eje.m  <- rep(seq(0,(1/s)*(s-1),1/s), as.integer(N/s)+1)
  eje.a  <- c(t0[1]:(t0[1]+as.integer(N/s)))
  eje.am <- c(1:length(eje.m))
  aux    <- as.numeric(gl(length(eje.a), s))
  k <- 1
  for(i in 1:length(eje.m)){
    eje.am[k] <- t0[1]-1+aux[i]+eje.m[i]
    k<- k+1
                           }
  eje.am2 <- eje.am[seq(1,length(eje.am),s/2)]
  plot(diff(.wts, lag=1), xlab="", ylab="", las=1, main="First differences of the series")
  abline(v=eje.am2, lty=2, col="blue")
  print(diff(.wts, lag=1))
}

Makeplotdeltas  <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  s <- frequency(.wts)
  N <- length(.wts)
  t0 <- start(.wts)

  eje.m  <- rep(seq(0,(1/s)*(s-1),1/s), as.integer(N/s)+1)
  eje.a  <- c(t0[1]:(t0[1]+as.integer(N/s)))
  eje.am <- c(1:length(eje.m))
  aux    <- as.numeric(gl(length(eje.a), s))
  k <- 1
  for(i in 1:length(eje.m)){
    eje.am[k] <- t0[1]-1+aux[i]+eje.m[i]
    k<- k+1
                           }
  eje.am2 <- eje.am[seq(1,length(eje.am),s/2)]
  plot(diff(.wts, lag=s), xlab="", ylab="",las=1, main="Seasonal differences of the series")
  abline(v=eje.am2, lty=2, col="blue")
  print(diff(.wts, lag=s))
}
Makeplotddeltas <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  plot(diff(diff(.wts, lag=frequency(.wts)), lag=1), xlab="", ylab="", las=1, main="First and sesonal differences of the series")
  #abline(v=eje.am2, lty=2, col="blue")
  print(diff(diff(.wts, lag=frequency(.wts)), lag=1))
}
Makeplotperdiff  <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  difpwts <- perdiff(.wts)
  plot(difpwts, xlab="", ylab="", las=1, main="Periodic differences of the series")
  print(difpwts)
}
Makewtsodet  <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  Transfdet(.wts, frequency(.wts))
  plot(wtsodet, xlab="", ylab="",las=1, main="Series without deterministic components")
  print(wtsodet)
  rm(wtsodet)
}
Makespec <- function(dif1)
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  wts <- .wts
  if(dif1 == FALSE){
     opar <- par(tcl=-0.5, cex.axis=0.8, cex.main=1, las=1)
     spectrum(wts, spans=c(3,5), log="no", ylab="", main="Estimated spectral density")
     par(opar)
                   }
  if(dif1 == TRUE){
     opar <- par(tcl=-0.5, cex.axis=0.8, cex.main=1, las=1)
     spectrum(diff(wts, lag=1), spans=c(3,5), log="no", ylab="",
           main="Estimated spectral density upon the first differences")
     par(opar)
                  }
}
#
Makebbplot <- function(transf)
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  if(transf == "orig")
    wtsbb <- .wts
  if(transf == "fdiff"){
    #ML <- ret(.wts, 2)
    #wtsbb <- ts(ML[,1]-ML[,2], frequency=frequency(.wts), start= start(.wts))
    wtsbb <- diff(.wts, lag=1)
  }
  #if(transf == "pdiff")
  #  wtsbb <- perdiff(.wts)

  #opar <- par(mar=c(8,4.7,5,1.5), ps=18, font=1,tcl=-0.5, cex.axis=0.7, las=1)
  bbplot(wtsbb)
  #par(opar)
}

#

Makebbaplot <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  ttplot  <- tktoplevel()
  tkwm.title(ttplot, "Buys-Ballot plot")

  ap1Var <- tclVar(" ")
  ap2Var <- tclVar(" ")
  ap3Var <- tclVar(" ")
  ap4Var <- tclVar(" ")
  ap5Var <- tclVar(" ")
  ap6Var <- tclVar(" ")

  ap1Entry <- tkentry(ttplot, width="7", textvariable=ap1Var)
  ap2Entry <- tkentry(ttplot, width="7", textvariable=ap2Var)
  ap3Entry <- tkentry(ttplot, width="7", textvariable=ap3Var)
  ap4Entry <- tkentry(ttplot, width="7", textvariable=ap4Var)
  ap5Entry <- tkentry(ttplot, width="7", textvariable=ap5Var)
  ap6Entry <- tkentry(ttplot, width="7", textvariable=ap6Var)

  tkgrid(tklabel(ttplot,
       text="Enter the years to plot:", fg="blue"))
  tkgrid(tklabel(ttplot, text="Year(s):"), ap1Entry, sticky="e")
  tkgrid(tklabel(ttplot, text="       "), ap2Entry, sticky="e")
  tkgrid(tklabel(ttplot, text="       "), ap3Entry, sticky="e")
  tkgrid(tklabel(ttplot, text="       "), ap4Entry, sticky="e")
  tkgrid(tklabel(ttplot, text="       "), ap5Entry, sticky="e")
  tkgrid(tklabel(ttplot, text="       "), ap6Entry, sticky="e")

  onOK <- function()
  {
      # tkdestroy(ttplot)
      yearsp <- c( ap1 <- as.numeric(tclvalue(ap1Var)),
         ap2 <- as.numeric(tclvalue(ap2Var)),
         ap3 <- as.numeric(tclvalue(ap3Var)),
         ap4 <- as.numeric(tclvalue(ap4Var)),
         ap5 <- as.numeric(tclvalue(ap5Var)),
         ap6 <- as.numeric(tclvalue(ap6Var)) )
      anyosp <- yearsp[which(yearsp != " ")]    # <<-
      opar <- par(mar=c(8,4.7,5,1.5), ps=18, font=1, tcl=-0.5)
      bbaplot(.wts, anyosp)
      par(opar)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=onOK)
  tkgrid(OK.but)
}
#
Makebbcn <- function()
{
  ttplot  <- tktoplevel()
  tkwm.title(ttplot, "Buys-Ballot contour")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1     <- tkradiobutton(ttplot)
  rb2     <- tkradiobutton(ttplot)

  rbValue <- tclVar("Color")
  tkconfigure(rb1, variable=rbValue, value="Blanco y Negro")
  tkconfigure(rb2, variable=rbValue, value="Color")
  tkgrid(tklabel(ttplot, text="Select the representation type:", fg="blue"))
  tkgrid(tklabel(ttplot, text="Black and white"), rb1)
  tkgrid(tklabel(ttplot, text="Colour"), rb2)

  OnOK <- function()
  {
     tkdestroy(ttplot)
     rbVal <- as.character(tclvalue(rbValue))
     if(rbVal == "Blanco y Negro"){ selecolor <- FALSE }
     if(rbVal == "Color"){ selecolor <- TRUE }
     bbcn(.wts, color=selecolor)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}
#
Makebb3D <- function(x=30, y=30)
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Buys-Ballot 3D")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1 <- tkradiobutton(ttplot)
  rb2 <- tkradiobutton(ttplot)

  rbValue <- tclVar("Color")
  tkconfigure(rb1, variable=rbValue, value="Blanco y Negro")
  tkconfigure(rb2, variable=rbValue, value="Color")
  tkgrid(tklabel(ttplot, text="Select the representation type:", fg="blue"))
  tkgrid(tklabel(ttplot, text="Black and white"), rb1)
  tkgrid(tklabel(ttplot, text="Colour"), rb2)

  OnOK <- function()
  {
    tkdestroy(ttplot)

    rbVal <- as.character(tclvalue(rbValue))
    if(rbVal == "Blanco y Negro"){ selecolor <- FALSE }
    if(rbVal == "Color"){ selecolor <- TRUE }
    bb3D(.wts, color=selecolor, as.integer(x), as.integer(y))
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

MakeSeasboxplot <- function()
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Seasonal box plot")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1 <- tkradiobutton(ttplot)
  rb2 <- tkradiobutton(ttplot)

  rbValue <- tclVar("Color")
  tkconfigure(rb1, variable=rbValue, value="Blanco y Negro")
  tkconfigure(rb2, variable=rbValue, value="Color")
  tkgrid(tklabel(ttplot, text="Select the representation type:", fg="blue"))
  tkgrid(tklabel(ttplot, text="Black and white"), rb1)
  tkgrid(tklabel(ttplot, text="Colour"), rb2)

  OnOK <- function()
  {
     tkdestroy(ttplot)
     rbVal <- as.character(tclvalue(rbValue))
     if(rbVal == "Blanco y Negro"){ color <- FALSE }
     if(rbVal == "Color"){ color <- TRUE }
     seasboxplot(.wts, color)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#
Makemonthplot <- function(type)
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  if(type == "orig"){
    monthplot(.wts, las=1, ylab="")
  }
  if(type == "fdiff"){
    monthplot(diff(.wts, lag=1), las=1, ylab="")
  }
}

#
Makeplotcycles <- function()
{
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))
  plotcycles(.wts)
}
#
Makefactorsdiff <- function()
{
  ttplot <- tktoplevel()
  tkgrid(tklabel(ttplot, text="Select the frequencies you wish to filter:", fg="blue"))

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  cero <- tkcheckbutton(ttplot)
  pi   <- tkcheckbutton(ttplot)
  pi2  <- tkcheckbutton(ttplot)
  if(frequency(.wts)==12){
    pi23 <- tkcheckbutton(ttplot)
    pi3  <- tkcheckbutton(ttplot)
    pi56 <- tkcheckbutton(ttplot)
    pi6  <- tkcheckbutton(ttplot)
           }
  ceroValue <- tclVar("0")
  piValue   <- tclVar("0")
  pi2Value  <- tclVar("0")
  if(frequency(.wts)==12){
    pi23Value <- tclVar("0")
    pi3Value  <- tclVar("0")
    pi56Value <- tclVar("0")
    pi6Value  <- tclVar("0")
           }
  tkconfigure(cero, variable=ceroValue)
  tkgrid(tklabel(ttplot, text="cero"), cero)
  tkconfigure(pi, variable=piValue)
  tkgrid(tklabel(ttplot, text="pi"), pi)
  tkconfigure(pi2, variable=pi2Value)
  tkgrid(tklabel(ttplot, text="pi/2"), pi2)
  if(frequency(.wts)==12){
    tkconfigure(pi23, variable=pi23Value)
    tkgrid(tklabel(ttplot, text="2pi/3"), pi23)
    tkconfigure(pi3, variable=pi3Value)
    tkgrid(tklabel(ttplot, text="pi/3"), pi3)
    tkconfigure(pi56, variable=pi56Value)
    tkgrid(tklabel(ttplot, text="5pi/6"), pi56)
    tkconfigure(pi6, variable=pi6Value)
    tkgrid(tklabel(ttplot, text="pi/6"), pi6)
           }
  OnOK <- function()
  {
     tkdestroy(ttplot)
     ff1 <- as.numeric(tclvalue(ceroValue))
     ff2 <- as.numeric(tclvalue(piValue))
     ff3 <- as.numeric(tclvalue(pi2Value))
     if(frequency(.wts)==12){
       ff4 <- as.numeric(tclvalue(pi23Value))
       ff5 <- as.numeric(tclvalue(pi3Value))
       ff6 <- as.numeric(tclvalue(pi56Value))
       ff7 <- as.numeric(tclvalue(pi6Value))
       filtra <- c(ff1,ff2,ff3,ff4,ff5,ff6,ff7)
              }
     if(frequency(.wts)==4){ filtra <- c(ff1,ff2,ff3) }
     Fil.vari <- factorsdiff(.wts, filtra)[[1]]
     #tkmessageBox(message="Object Fil.vari has been created. It contains filtered series data.", icon="info")
     string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))

     newstring <- paste("Fil_", string, sep="")

     aux <- filtra[1]
     for(i in 2:(frequency(.wts)/2+1))
       aux <- paste(aux, filtra[i], sep="")
     #showlabel <- paste(newstring, "     ", aux, sep="")
     showlabel <- newstring <- paste(newstring, aux, sep="_")

     vari  <- ts(Fil.vari, frequency=frequency(.wts), start=start(.wts))
     assign(newstring, vari, env=.GlobalEnv)

     tkinsert(.treeWidget,"end", string, newstring, text=newstring)

  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

Makecorrgrm <- function()
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Correlograms")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1     <- tkradiobutton(ttplot)
  rb2     <- tkradiobutton(ttplot)
  rb3     <- tkradiobutton(ttplot)
  rb4     <- tkradiobutton(ttplot)
  rbValue <- tclVar("Serie original")

  tkconfigure(rb1, variable=rbValue, value="original")
  tkconfigure(rb2, variable=rbValue, value="delta")
  tkconfigure(rb3, variable=rbValue, value="deltas")
  tkconfigure(rb4, variable=rbValue, value="deltadeltas")
  tkgrid(tklabel(ttplot, text="Select a transformation:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttplot, text="  Original series"), rb1, sticky="w")
  tkgrid(tklabel(ttplot, text="  First differences"), rb2, sticky="w")
  tkgrid(tklabel(ttplot, text="  Seasonal differences"), rb3, sticky="w")
  tkgrid(tklabel(ttplot, text="  First and sesonal differences"), rb4, sticky="w")

  OnOK <- function()
  {
     # tkdestroy(ttplot)
     rbVal <- as.character(tclvalue(rbValue))
     opar <- par(mfrow=c(2,1)) #, mar=c(4,4.7, 2 ,2), ps=18, font=1, tcl=-0.5,
     #           cex.lab=0.8, cex.axis=0.7)
     corrgrm(.wts, tclvalue(rbValue))
     par(opar)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

Makermp <- function()
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Range-mean plot")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  krmg <- tclVar(round(sqrt(length(.wts))))

  entry.krmg <- tkentry(ttplot, width="3", textvariable=krmg)
  tkgrid(tklabel(ttplot, text="  Introduce the interval width for calculating the points:  \n (By default sqrt(N))", fg="blue"), entry.krmg, rowspan=2)
  tkgrid(tklabel(ttplot))

  rb1     <- tkradiobutton(ttplot)
  rb2     <- tkradiobutton(ttplot)
  lambda  <- tclVar(0)
  rbValue <- tclVar("Serie original")
  tkconfigure(rb1,variable=rbValue, value="Serie original")
  tkconfigure(rb2,variable=rbValue, value="Transformacion Box-Cox")
  tkgrid(tklabel(ttplot, text="  Introduce the value of lambda if you select the transformation:",
       fg="blue"), sticky="w")
  tkgrid(tklabel(ttplot, text="Original series"), rb1)
  tkgrid(tklabel(ttplot, text="Box-Cox transformation"), rb2)

  entry.lambda <- tkentry(ttplot, width="4", textvariable=lambda)
  tkgrid(tklabel(ttplot, text="lambda"), entry.lambda)

  OnOK <- function()
  {
     tkdestroy(ttplot)
     if(tclvalue(rbValue)=="Serie original")
       rmwts <- .wts
     if(tclvalue(rbValue)=="Transformacion Box-Cox")
     {
       if(tclvalue(lambda) == 0)
         rmwts <- log(.wts, base=exp(1))
       if(tclvalue(lambda) != 0){
         rmwts <- BoxCox(.wts, as.numeric(tclvalue(lambda)))
         cat(c("\n Box-Cox parameter: ", tclvalue(lambda), "\n\n"))
#         bcvari <- rmvari <- BoxCox(.wts, as.numeric(tclvalue(lambda)))
#         tkmessageBox(message="Object bcvari has been created. It contains transformed series #data.", icon="info")
                                }
     }
     # opar <- par(mar=c(8,4.7,5,2), ps=18, font=1,tcl=-0.5)
     #opar <- par(mar=c(4,4.7, 4 ,2), ps=18, font=1, tcl=-0.5, cex.lab=0.8, cex.axis=0.7)
     rmg(rmwts, as.numeric(tclvalue(krmg)))
     #par(opar)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

Makevfic <- function()
{
  tt    <- tktoplevel()
  tkwm.title(tt, "Generic dummy")

  y0fic <- tclVar()
  s0fic <- tclVar()
  yNfic <- tclVar()
  sNfic <- tclVar()

  entry.y0fic <- tkentry(tt, width="5", textvariable=y0fic)
  entry.s0fic <- tkentry(tt, width="3", textvariable=s0fic)
  entry.yNfic <- tkentry(tt, width="5", textvariable=yNfic)
  entry.sNfic <- tkentry(tt, width="3", textvariable=sNfic)

  tkgrid(tklabel(tt, text="  Specify a dummy:", fg="blue"), sticky="w")
  tkgrid(tklabel(tt, text="    Start (Year-season):"), entry.y0fic, entry.s0fic)
  tkgrid(tklabel(tt, text="    End   (Year-season):"), entry.yNfic, entry.sNfic)

  OnOK <- function()
  {
     tkdestroy(tt)
     #n <- which.min(c(which(Mvfic[1,] == 0), which(Mvfic[1,] == 1)))
     ifelse(length(n)==0, n<-0, n<-n)
     Mvfic[,(n+1)] <<- vfic(as.numeric(tclvalue(y0fic)),
                    as.numeric(tclvalue(s0fic)),
                    as.numeric(tclvalue(yNfic)),
                    as.numeric(tclvalue(sNfic)))
     tkmessageBox(message="This dummy has been incorporated to the Mvfic object.", icon="info")
  }
  OK.but <- tkbutton(tt, text="OK", command=OnOK)
  tkgrid(OK.but)
}

#

MakeVFEp <- function()
{
  tt <- tktoplevel()
  tkwm.title(tt, "Partial seasonal dummy")

  vfe1 <- tclVar()
  vfe2 <- tclVar()
  vfe3 <- tclVar()
  vfe4 <- tclVar()
  if(frequency(.wts)==12){
  vfe5 <- tclVar()
  vfe6 <- tclVar()
                  }
  entry.vfe1 <- tkentry(tt, width="2", textvariable=vfe1)
  entry.vfe2 <- tkentry(tt, width="2", textvariable=vfe2)
  entry.vfe3 <- tkentry(tt, width="2", textvariable=vfe3)
  entry.vfe4 <- tkentry(tt, width="2", textvariable=vfe4)
  if(frequency(.wts)==12){
    entry.vfe5 <- tkentry(tt, width="2", textvariable=vfe5)
    entry.vfe6 <- tkentry(tt, width="2", textvariable=vfe6)
                  }
  tkgrid(tklabel(tt, text="Introduce the seasons you wish to consider:", fg="blue"), sticky="w")
  if(frequency(.wts)==4)
     tkgrid(tklabel(tt, text="Seasons:"), entry.vfe1, entry.vfe2, entry.vfe3, entry.vfe4)
  if(frequency(.wts)==12)
     tkgrid(tklabel(tt, text="Seasons:"),
            entry.vfe1, entry.vfe2, entry.vfe3, entry.vfe4, entry.vfe5, entry.vfe6)

  OnOK <- function()
  {
    tkdestroy(tt)
    vfe1 <- as.numeric(tclvalue(vfe1))
    vfe2 <- as.numeric(tclvalue(vfe2))
    vfe3 <- as.numeric(tclvalue(vfe3))
    vfe4 <- as.numeric(tclvalue(vfe4))
    if(frequency(.wts)==12){
    vfe5 <- as.numeric(tclvalue(vfe5))
    vfe6 <- as.numeric(tclvalue(vfe6))
             }
    ifelse(frequency(.wts)==12, aux <- c(vfe1, vfe2, vfe3, vfe4, vfe5, vfe6),
                  aux <- c(vfe1, vfe2, vfe3, vfe4))
    VFEp <<- MVFE(.wts, frequency(.wts), start(.wts), "alg")[,na.omit(aux)]
    tkmessageBox(message="The dummy has been saved as the object VFEp.", icon="info")
  }
  OK.but <- tkbutton(tt, text="OK", command=OnOK)
  tkgrid(OK.but)
}
#

MakePanelqmCorrg <- function()
{
   ttplot <- tktoplevel()
   tkwm.title(ttplot, "Panel")
   tklabel(ttplot, text="Panel")

   string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
   .wts <<- ExeString(c(string, ""))

   tkdestroy(ttplot)
   opar <- par(mfrow=c(4,2), mar=c(2,3,3.5,2), # mar=c(3,3,3.5,2),
               tcl=-0.5, cex.axis=0.7, las=1)
   corrgrm(.wts, "original")
   corrgrm(.wts, "delta")
   corrgrm(.wts, "deltas")
   corrgrm(.wts, "deltadeltas")
   par(opar)
}
#
MakePanelmSerie <- function()
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Panel")
  tklabel(ttplot, text="Panel")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1     <- tkradiobutton(ttplot)
  rb2     <- tkradiobutton(ttplot)
  rbValue <- tclVar("log")
  tkconfigure(rb1, variable=rbValue, value="log")
  tkconfigure(rb2, variable=rbValue, value="nlog")

  tkgrid(tklabel(ttplot, text="  Indicate the scale of the series:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttplot, text="Logarithm"), rb1)
  tkgrid(tklabel(ttplot, text="Without logarithm"), rb2)

  OnOK <- function()
  {
    tkdestroy(ttplot)
    if(tclvalue(rbValue) == "log"){
      wtsaux <- log(.wts, base=exp(1))
      main1 <- "First differences of the logarithms"
      main2 <- "Seasonal and regular differences of the logarithms"
      main3 <- "Estimated espectral density upon the logarithms"
      main4 <- "Spectrum of the first differences in logaritmhs"
                                  }
    if(tclvalue(rbValue) == "nlog"){
      wtsaux <- .wts
      main1 <- "First differences"
      main2 <- "Seasonal and regular differences"
      main3 <- "Estimated spectral density"
      main4 <- "Spectrum of the first differences"
                                   }
    opar <- par(mfrow=c(4,2), mar=c(2,3,3.5,2), tcl=-0.5, cex.axis=0.7, cex.main=1, las=1)
    plot(wtsaux, main=string)
    rmg(wtsaux, round(sqrt(length(.wts))))
    plot(log(wtsaux, base=exp(1)), main="Logarithms of the series")
    rmg(log(wtsaux, base=exp(1)), round(sqrt(length(.wts))))
    plot(diff(wtsaux, lag=1), main=main1)
    plot(diff(diff(wtsaux, lag=frequency(.wts)), lag=1), main=main2)
    spectrum(wtsaux, spans=c(3,5), log="no", ylab="", xlab="frequency", main=main3)
    spectrum(diff(wtsaux, lag=1), spans=c(3,5), log="no", ylab="", xlab="frequency", main=main4)
    par(opar)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}
#
MakeQPanel1 <- function()  # cambiar nombre en GUI menu
{
  ttplot <- tktoplevel()
  tkwm.title(ttplot, "Panel")
  tklabel(ttplot, text="Panel")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  rb1     <- tkradiobutton(ttplot)
  rb2     <- tkradiobutton(ttplot)
  rbValue <- tclVar("log")
  tkconfigure(rb1, variable=rbValue, value="log")
  tkconfigure(rb2, variable=rbValue, value="nlog")

  tkgrid(tklabel(ttplot, text="  Indicate the scale of the series:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttplot, text="Logarithm"), rb1)
  tkgrid(tklabel(ttplot, text="Without logarithm"), rb2)

  OnOK <- function()
  {
    tkdestroy(ttplot)
    if(tclvalue(rbValue) == "log"){
      wtsaux <- log(.wts, base=exp(1))
      main1 <- "First differences of the logarithms"
      main2 <- "Seasonal and regular differences of the logarithms"
      main3 <- "Estimated espectral density upon the logarithms"
      main4 <- "Spectrum of the first differences in logaritmhs"
                                  }
    if(tclvalue(rbValue) == "nlog"){
      wtsaux <- .wts
      main1 <- "First differences"
      main2 <- "Seasonal and regular differences"
      main3 <- "Estimated spectral density"
      main4 <- "Spectrum of the first differences"
                                   }
    opar <- par(mfrow=c(4,2), mar=c(2,3,3.5,2), tcl=-0.5, cex.axis=0.8, cex.main=1, las=1)
    plot(wtsaux, main=string, xlab="", ylab="")
    rmg(wtsaux, round(sqrt(length(.wts))))
    plot(log(wtsaux, base=exp(1)), main="Logarithms of the series")
    rmg(log(wtsaux, base=exp(1)), round(sqrt(length(.wts))))
    plot(diff(wtsaux, lag=1), main=main1, ylab="")
    plot(diff(diff(wtsaux, lag=frequency(.wts)), lag=1), main=main2, ylab="")
    spectrum(wtsaux, spans=c(3,5), log="no", ylab="", xlab="frequency", main=main3)
    spectrum(diff(wtsaux, lag=1), spans=c(3,5), log="no", ylab="", xlab="frequency", main=main4)
    par(opar)
  }
  OK.but <- tkbutton(ttplot, text="OK", command=OnOK)
  tkgrid(OK.but)
}

# Mostrar el contenido de un archivo en un ventana redimensionable
mytkpager <- function (file, header, title, delete.file, wwidth, wheight, export)
{
  title <- paste(title, header)
  for (i in seq(along = file))
  {
     zfile <- file[[i]]
     tt <- tktoplevel()
     tkwm.title(tt, if(length(title))
                       title[(i - 1)%%length(title) + 1]
                    else "")
     txt <- tktext(tt, bg = "white", font = "courier", fg="blue")

     scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt,...))
     tkconfigure(txt, yscrollcommand = function(...) tkset(scr,...), width=wwidth, height=wheight)
     tkpack(txt, side = "left", fill = "both", expand = TRUE)
     tkpack(scr, side = "right", fill = "y")

     chn <- tkcmd("open", zfile)
     tkinsert(txt, "end", gsub("_\b", "", tclvalue(tkcmd("read", chn))))
     tkcmd("close", chn)
     tkconfigure(txt, state = "disabled")
     tkmark.set(txt, "insert", "0.0")
     tkfocus(txt)

     if(delete.file)
        tkcmd("file", "delete", zfile)
  }
  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu=topMenu)

  if(export==TRUE)
  {
    ToolsMenu <- tkmenu(topMenu, tearoff=FALSE)
    tkadd(ToolsMenu, "command", label="Export to a LaTeX file", command=function()RecTestLaTeX())
    tkadd(topMenu, "cascade", label="Tools", menu=ToolsMenu)
  }
}

#

MakeFactors <- function()
{
  tt <- tktoplevel()
  tkgrid(tklabel(tt, text="Select the frequencies you wish to filter:", fg="blue"))

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  cero <- tkcheckbutton(tt)
  pi   <- tkcheckbutton(tt)
  pi2  <- tkcheckbutton(tt)
  if(frequency(.wts)==12){
    pi23 <- tkcheckbutton(tt)
    pi3  <- tkcheckbutton(tt)
    pi56 <- tkcheckbutton(tt)
    pi6  <- tkcheckbutton(tt)
           }
  ceroValue <- tclVar("0")
  piValue   <- tclVar("0")
  pi2Value  <- tclVar("0")
  if(frequency(.wts)==12){
    pi23Value <- tclVar("0")
    pi3Value  <- tclVar("0")
    pi56Value <- tclVar("0")
    pi6Value  <- tclVar("0")
           }
  tkconfigure(cero, variable=ceroValue)
  tkgrid(tklabel(tt, text="cero"), cero)
  tkconfigure(pi, variable=piValue)
  tkgrid(tklabel(tt, text="pi"), pi)
  tkconfigure(pi2, variable=pi2Value)
  tkgrid(tklabel(tt, text="pi/2"), pi2)
  if(frequency(.wts)==12){
    tkconfigure(pi23, variable=pi23Value)
    tkgrid(tklabel(tt, text="2pi/3"), pi23)
    tkconfigure(pi3, variable=pi3Value)
    tkgrid(tklabel(tt, text="pi/3"), pi3)
    tkconfigure(pi56, variable=pi56Value)
    tkgrid(tklabel(tt, text="5pi/6"), pi56)
    tkconfigure(pi6, variable=pi6Value)
    tkgrid(tklabel(tt, text="pi/6"), pi6)
           }
  OnOK <- function()
  {
     tkdestroy(tt)
     ff1 <- as.numeric(tclvalue(ceroValue))
     ff2 <- as.numeric(tclvalue(piValue))
     ff3 <- as.numeric(tclvalue(pi2Value))
     if(frequency(.wts)==12){
       ff4 <- as.numeric(tclvalue(pi23Value))
       ff5 <- as.numeric(tclvalue(pi3Value))
       ff6 <- as.numeric(tclvalue(pi56Value))
       ff7 <- as.numeric(tclvalue(pi6Value))
       filtra <- c(ff1,ff2,ff3,ff4,ff5,ff6,ff7)
              }
     if(frequency(.wts)==4){ filtra <- c(ff1,ff2,ff3) }
     Fil.vari <- factorsdiff(.wts, filtra)[[1]]  ## plot(Fil.vari)
     wts <- ts(Fil.vari, frequency=frequency(.wts), start=start(.wts))
     plot(wts)
  }
  OK.but <- tkbutton(tt, text="OK", command=OnOK)
  tkgrid(OK.but)
}

Transfdet <- function(wts)
{
  s <- frequency(wts)
  N <- length(wts)

  VFEm <- function(wts, s)
  {
    N    <- length(wts)
    VFE <- matrix(0, nrow = N, ncol = s-1)
    sq1 <- seq(1, N, s)
    sq2 <- seq(0, N, s)
    k <- 0
    for (j in 1:(s-1)){
       ifelse(sq1[length(sq1)] + k > N, n1 <- length(sq1) -1, n1 <- N)
       ifelse(sq2[length(sq2)] + k > N, n2 <- length(sq2) -1, n2 <- N)
       for (i in 1:n1)
          VFE[sq1[i]+k, j] <- 1
       for (i in 1:n2)
          VFE[sq2[i]+k, j] <- -1
        k <- k + 1
                      }
    VFE
  }
  tiempo  <- matrix(c(1:length(wts)), ncol=1)
  VFE     <- VFEm(wts, s)
  ML      <- ret(wts, 2)
  lmdet   <- lm(ML[,1] ~ tiempo + VFE[,1:(s-1)])  # incluye constante

  VFE2 <- matrix(nrow=length(wts), ncol=s-1)
  for(j in 1:length(wts)){
     for(i in 3:(s+1))
       VFE2[j,(i-2)] <- lmdet$coef[i]*VFE[j,(i-2)]
                          }
   for(i in 1:length(wts))
      VFE2[i,1] <- sum(VFE2[i,])

   wtsodet <- wts-(lmdet$coef[1] + lmdet$coef[2]*tiempo + VFE2[,1])
   print(summary(lmdet))
   wtsodet
}

## Others

# Ejecutar como commando una variable de tipo carácter
ExeString <- function(Char)
{
  tempf <- tempfile()
  string <- paste(Char[1], Char[2], sep="")
  if(length(Char) > 2){
    for(i in 3:length(Char))
      string <- paste(string, Char[i], sep="")
  }
  string <- cat(string, file=tempf)
  # cat(paste(char, arg, sep=""), file=tempf)
  char.out <- eval(source(tempf))[[1]]

  char.out
}

BoxCox <- function(wts, lambda)
{
  if(lambda==0)
    bcwts <- log(wts, base=exp(1))
  if(lambda!=0)
    bcwts <- (wts^lambda - 1)/lambda
  bcwts
}

seasboxplot <- function(wts, color)
{
  s    <- frequency(wts)
  t0   <- start(wts)
  N    <- length(wts)

  if(s==4)
    xnames <- c("Qrtr1", "Qrtr2", "Qrtr3", "Qrtr4")
  if(s==12)
    xnames <- c("January", "February", "March", "April", "May", "June", "July",
               "August", "September", "October", "November", "December")

  if(color==TRUE){color1 <- "lightblue"; color2 <- "SeaGreen2"}
  if(color==FALSE){color1 <- "lightgray"; color2 <- "gray60"}
  opar <- par(las=1) # cex.axis=0.7, cex.main=0.8,
  summ.box <- boxplot(split(wts, cycle(wts)), names=xnames,col=color1)
  #                   main="Gráfico de cajas estacional"
  boxplot(split(wts, cycle(wts)), names=xnames, notch=TRUE, add=TRUE, col=color2)
  par(opar)

  if(s==4){
    stats <- round(data.frame("Qrtr1"=summ.box[[1]][,1], "Qrtr2"=summ.box[[1]][,2],
           "Qrtr3"=summ.box[[1]][,3], "Qrtr4"=summ.box[[1]][,4]), 2)
           }
  if(s==12){
    stats <- round(data.frame("January"=summ.box[[1]][,1], "February"=summ.box[[1]][,2],
           "March"=summ.box[[1]][,3], "April"=summ.box[[1]][,4],
           "May"=summ.box[[1]][,5], "June"=summ.box[[1]][,6],
           "July"=summ.box[[1]][,7], "August"=summ.box[[1]][,8],
           "September"=summ.box[[1]][,9], "October"=summ.box[[1]][,10],
           "November"=summ.box[[1]][,11], "December"=summ.box[[1]][,12]), 2)
           }
  cat("\n ------ Seasonal box-plot ------ \n\n")
  cat("     Statictics (by rows): \n")
  cat("     minimum, quartile1, median, quartile3, maximun. \n \n")
  print(stats)
  cat("\n\n Confidence interval of 90% for each median: \n\n")
  print(round(summ.box$conf),2)
  if(length(summ.box$out)){
  cat("\n\n The oints wich lie beyond the extremes of the whiskers are: de\n\n")
  print(round(summ.box$conf,2))
  cat("\n\n and are refered to the following seasons:")
  print(summ.box$group)
                          }
}

corrgrm <- function(wts, transf)
{
  s  <- frequency(wts)
  t0 <- start(wts)
  N  <- length(wts)

  if(transf == "original"){
#     opar <- par(mfrow=c(2,1), mar=c(4,4.7, 1 ,1.5), ps=18,
#                 font=1, tcl=-0.5, cex.lab=0.8, cex.axis=0.7)
     acf(wts, na.action = na.pass, lag=48, main="ACF", xlab="", ylab="")
     mtext("Original series", side=3, line=0.35, cex=0.7)
     pacf(wts, na.action = na.pass, lag=48, main="PACF", xlab="Lag", ylab="")
     #, xlab="Lag", ylab="PACF")
     mtext("Original series", side=3, line=0.35, cex=0.7)
#     par(opar)
                          }
  if(transf == "delta"){
#     opar <- par(mfrow=c(2,1), mar=c(4,4.7, 1 ,1.5), ps=18,
#                 font=1, tcl=-0.5, cex.lab=0.8, cex.axis=0.7)
     acf(diff(wts, lag=1), na.action = na.pass, lag=48, xlab="", ylab="", main="")
     mtext(expression(Delta(y)), side=3, line=0.35)
     pacf(diff(wts, lag=1), na.action = na.pass, lag=48, xlab="Lag", ylab="", main="")
     mtext(expression(Delta(y)), side=3, line=0.35)
#     par(opar)
                       }
  if(transf == "deltas"){
#     opar <- par(mfrow=c(2,1), mar=c(4,4.7, 1 ,1.5), ps=18,
#                 font=1, tcl=-0.5, cex.lab=0.8, cex.axis=0.7)
     acf(diff(wts, lag=s), na.action = na.pass, lag=48, xlab="", ylab="", main="")
     mtext(expression(Delta^s*(y)), side=3, line=0.35)
     pacf(diff(wts, lag=s), na.action = na.pass, lag=48, xlab="Lag", ylab="", main="")
     mtext(expression(Delta^s*(y)), side=3, line=0.35)
#     par(opar)
                        }
  if(transf == "deltadeltas"){
#     opar <- par(mfrow=c(2,1), mar=c(4,4.7,1,1.5), ps=18,
#                 font=1, tcl=-0.5, cex.lab=0.8, cex.axis=0.7)
     ML      <- ret(wts, s+2)
     ddswts <- ML[,1]-ML[,2]-ML[,s+1]+ML[,s+2]
     acf(ddswts, na.action = na.pass, lag=48, xlab="", ylab="", main="")
     mtext(expression(Delta*Delta^s* (y)), side=3, line=0.35)
     pacf(ddswts, na.action = na.pass, lag=48, xlab="Lag", ylab="", main="")
     mtext(expression(Delta*Delta^s*(y)), side=3, line=0.35)
#     par(opar)
                             }
}

ExportGraph <- function()
{
    exgrfile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".ps" ".eps"}} {"All Files" {"*"}}'))
      if(!nchar(exgrfile))
         tkmessageBox(message="No file was chosen.", icon="error")
      else{
        dev.copy2eps(file=exgrfile, device=X11)
          }
      # tkdestroy(ttexgr)
      # tkdestroy(ttplot)   # ver con esto o sin esto
}

PanelqmCorrg <- function()
{
#   opar <- par(mfrow=c(4,2), mar=c(3,4,3,3.5), # mar=c(3,3,3.5,2),
#               tcl=-0.5, cex.axis=0.7, cex.main=1, las=1)
   corrgrm(.wts, "original")
   corrgrm(.wts, "delta")
   corrgrm(.wts, "deltas")
   corrgrm(.wts, "deltadeltas")
#   par(opar)
}
#

PanelmFreq <- function()
{
#   opar <- par(mfrow=c(4,2), mar=c(3,3,3.5,2),
#               tcl=-0.5, cex.axis=0.7, cex.main=1, las=1)
  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  freqg(.wts)
#   par(opar)
}

#

PanelqRMBBFreq <- function()
{
#  opar <- par(mfrow=c(4,2), mar=c(3,4,3,3),
#              tcl=-0.5, cex.axis=0.7, cex.main=1, las=1)
  rmg(.wts, frequency(.wts))
  freqg(.wts)
   bbap(.wts, frequency(.wts), start(.wts), c(start(.wts)[1], start(.wts)[1]+2,
     start(.wts)[1]+4, start(.wts)[1]+6))
     # Serie de al menos 8 anyos
   quarterg(.wts, start(.wts), plot=TRUE)
#  par(opar)
}

wtsinfo_treeW <- function()
{
  tt  <- tktoplevel()
  tkwm.title(tt, "Series info")
  txt <- tktext(tt, bg="white", height=30, width=70)
  tkgrid(txt)

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  # tclvalue(.label) <- as.character(tclvalue(string))
  #.wvari <<- ExeString(c(tclvalue(.label), ""))
  .wvari <<- ExeString(c(string, ""))

  if(.wvari$s == 4){ per <- "4, quarterly" }
  if(.wvari$s == 12){ per <- "12, monthly" }
  end <- ysooys(.wvari$N, .wvari$t0, .wvari$N, .wvari$s)[[1]]

  tkinsert(txt, "end", paste("\n", .wvari$label, "\n"))
  start <- paste(.wvari$t0[1], .wvari$t0[2], sep=".")
  end  <- paste(end[1], end[2], sep=".")
  tkinsert(txt, "end", paste("\n Period:", start, " - ", end, "\n"))
  tkinsert(txt, "end", paste("\n Periodicity: ", per, "\n"))

  tkconfigure(txt, state="disabled")
  tkfocus(txt)
}

MakeADFHEGY.rectest <- function(type)
{
  ttadf  <- tktoplevel()
  tkwm.title(ttadf, paste(type, "test"))

  #string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))  ##~ ACTIVAR
  #.wts <<- ExeString(c(string, ""))  ##~ ACTIVAR
  #N <- length(.wts)  ##~ ACTIVAR

  tkgrid(tklabel(ttadf, text="  Select deterministic components:", fg="blue"), sticky="w")
  Cte  <- tkcheckbutton(ttadf)
  Tdl  <- tkcheckbutton(ttadf)
  Vfet <- tkcheckbutton(ttadf)

  CteValue  <- tclVar("0")
  TdlValue  <- tclVar("0")
  VfetValue <- tclVar("0")

  tkconfigure(Cte, variable=CteValue)
    tkgrid(tklabel(ttadf, text="Intercept"), Cte)
  tkconfigure(Tdl, variable=TdlValue)
    tkgrid(tklabel(ttadf, text="Trend"), Tdl)
  tkconfigure(Vfet, variable=VfetValue)
    tkgrid(tklabel(ttadf, text="Seasonal dummys"), Vfet)

  rb3 <- tkradiobutton(ttadf)
  rb4 <- tkradiobutton(ttadf)
  rb5 <- tkradiobutton(ttadf)
  rbValue <- tclVar("BIC")
  #yself <- tclVar()
  #entry.yself <- tkentry(ttadf, width="3", textvariable=yself)

  tkconfigure(rb3, variable=rbValue, value="AIC")
  tkconfigure(rb4, variable=rbValue, value="BIC")
  tkconfigure(rb5, variable=rbValue, value="Signf")

  tkgrid(tklabel(ttadf, text="  Select the method for choosing lags:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttadf, text="AIC-top-down"), rb3)
  tkgrid(tklabel(ttadf, text="BIC-top-down"), rb4)
  tkgrid(tklabel(ttadf, text="Significant lags"), rb5)

  rbsb1 <- tkradiobutton(ttadf)
  rbsb2 <- tkradiobutton(ttadf)
  rbsb3 <- tkradiobutton(ttadf)
  rbsbValue <- tclVar("moving")
  tkconfigure(rbsb1, variable=rbsbValue, value="backw")
  tkconfigure(rbsb2, variable=rbsbValue, value="forw")
  tkconfigure(rbsb3, variable=rbsbValue, value="moving")

  tkgrid(tklabel(ttadf, text="  Select the type of subsamples:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttadf, text="Backwards subsamples"), rbsb1)
  tkgrid(tklabel(ttadf, text="Forwards subsamples"), rbsb2)
  tkgrid(tklabel(ttadf, text="Moving subsamples"), rbsb3)

  nsub <- tclVar()
  entry.nsub <- tkentry(ttadf, width="4", textvariable=nsub)
  tkgrid(tklabel(ttadf, text="  Type the length of each subsample:", fg="blue"), sticky="w")
  tkgrid(entry.nsub)
  tkgrid(tklabel(ttadf, text=" "))

  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
      tkdestroy(ttadf)

      mVal <- as.character(tclvalue(CteValue))
      if(mVal =="1")
         Ct <- 1
      if(mVal =="0")
         Ct <- 0
      mVal <- as.character(tclvalue(TdlValue))
      if(mVal =="1")
         TD <- 1
      if(mVal =="0")
         TD <- 0
      mVal <- as.character(tclvalue(VfetValue))
      if(mVal =="1")
         Vfe <- c(1:(frequency(.wts)-1))
      if(mVal =="0")
         Vfe <- 0

      # SelecP
      rbVal <- as.character(tclvalue(rbValue))
      if(tclvalue(rbValue) == "AIC")
        selecP <- list(mode="aic", Pmax=NULL)
       if(tclvalue(rbValue) == "BIC")
        selecP <- list(mode="bic", Pmax=NULL)
      if(tclvalue(rbValue) == "Signf")
        selecP <- list(mode="signf", Pmax=NULL)

      typesub <- tclvalue(rbsbValue)
      nsub <- as.numeric(tclvalue(nsub))
      if(type == "ADF")
        .out <<- ADF.rectest(.wts, type=typesub, nsub=nsub, itsd=c(Ct,TD,Vfe),selectlags=selecP)
      if(type == "HEGY")
        .out <<- HEGY.rectest(.wts, type=typesub, nsub=nsub, itsd=c(Ct,TD,Vfe), selectlags=selecP)
      show(.out)

      tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttadf,text="OK",command=OnOK)
  tkgrid(OK.but)
}

MakeCH.rectest <- function()
{
  ttch <- tktoplevel()
  tkwm.title(ttch, "CH test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  trend  <- tkcheckbutton(ttch);  trendValue  <- tclVar("0")
  lag1   <- tkcheckbutton(ttch);  lag1Value  <- tclVar("1")

  tkgrid(tklabel(ttch, text="  Select the elements to include in the auxiliar regression:",
        fg="blue"), sticky="w")
  tkconfigure(trend, variable=trendValue)
    tkgrid(tklabel(ttch, text="Trend"), trend)
  tkconfigure(lag1, variable=lag1Value)
    tkgrid(tklabel(ttch, text="First order lag"), lag1)

  rb1 <- tkradiobutton(ttch)
  rb2 <- tkradiobutton(ttch)
  rbValue <- tclVar("nsumm")
  tkconfigure(rb1, variable = rbValue, value = "nsumm")
  tkconfigure(rb2, variable = rbValue, value = "summ")

  tkgrid(tklabel(ttch, text = "   ---   ---   --- "))
  #tkgrid(tklabel(ttch, text = "  Show a summary.", fg="blue"), rb2, sticky="w")
  #tkgrid(tklabel(ttch, text = "  Analyse selected frequencies:", fg="blue"), rb1, sticky="w")
  tkgrid(tklabel(ttch, text = "  Analyse selected frequencies:", fg="blue"), sticky="w")

  if(frequency(.wts)==12){
    pi6  <- tkcheckbutton(ttch)
    pi3  <- tkcheckbutton(ttch)
    pi23 <- tkcheckbutton(ttch)
    pi56 <- tkcheckbutton(ttch)
           }
  pi2  <- tkcheckbutton(ttch)
  pi   <- tkcheckbutton(ttch)

  if(frequency(.wts)==12){
    pi6Value  <- tclVar("0")
    pi3Value  <- tclVar("0")
    pi23Value <- tclVar("0")
    pi56Value <- tclVar("0")
           }
  pi2Value  <- tclVar("0")
  piValue   <- tclVar("0")

  if(frequency(.wts)==12){
    tkconfigure(pi6, variable=pi6Value)
    tkgrid(tklabel(ttch, text="pi/6"), pi6)
    tkconfigure(pi3, variable=pi3Value)
    tkgrid(tklabel(ttch, text="pi/3"), pi3)
  }
  tkconfigure(pi2, variable=pi2Value)
  tkgrid(tklabel(ttch, text="pi/2"), pi2)
  if(frequency(.wts)==12){
    tkconfigure(pi23, variable=pi23Value)
    tkgrid(tklabel(ttch, text="2pi/3"), pi23)
    tkconfigure(pi56, variable=pi56Value)
    tkgrid(tklabel(ttch, text="5pi/6"), pi56)
  }
  tkconfigure(pi, variable=piValue)
  tkgrid(tklabel(ttch, text="pi"), pi)

  rbsb1 <- tkradiobutton(ttch)
  rbsb2 <- tkradiobutton(ttch)
  rbsb3 <- tkradiobutton(ttch)
  rbsbValue <- tclVar("moving")
  tkconfigure(rbsb1, variable=rbsbValue, value="backw")
  tkconfigure(rbsb2, variable=rbsbValue, value="forw")
  tkconfigure(rbsb3, variable=rbsbValue, value="moving")

  tkgrid(tklabel(ttch, text="  Select the type of subsamples:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttch, text="Backwards subsamples"), rbsb1)
  tkgrid(tklabel(ttch, text="Forwards subsamples"), rbsb2)
  tkgrid(tklabel(ttch, text="Moving subsamples"), rbsb3)

  nsub <- tclVar()
  entry.nsub <- tkentry(ttch, width="4", textvariable=nsub)
  tkgrid(tklabel(ttch, text="  Type the length of each subsample:", fg="blue"), sticky="w")
  tkgrid(entry.nsub)
  tkgrid(tklabel(ttch, text=" "))

  #
  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
    tkdestroy(ttch)
    trendVal <- as.character(tclvalue(trendValue))
      if(trendVal =="1")
         DetTr <- TRUE
      if(trendVal =="0")
         DetTr <- FALSE
   lag1Val <- as.character(tclvalue(lag1Value))
      if(lag1Val =="1")
         f0 <- 1
      if(lag1Val =="0")
         f0 <- 0

    if (tclvalue(rbValue) == "nsumm")
    {
      frec <- rep(0, frequency(.wts)/2)
      if(frequency(.wts) == 12)
      {
        mVal <- as.character(tclvalue(pi6Value))
        if(mVal =="1")
           frec[1] <- 1
        if(mVal =="0")
           frec[1] <- 0
        mVal <- as.character(tclvalue(pi3Value))
        if(mVal =="1")
          frec[2] <- 1
        if(mVal =="0")
           frec[2] <- 0
        mVal <- as.character(tclvalue(pi23Value))
        if(mVal =="1")
           frec[4] <- 1
        if(mVal =="0")
           frec[4] <- 0
        mVal <- as.character(tclvalue(pi56Value))
        if(mVal =="1")
           frec[5] <- 1
        if(mVal =="0")
           frec[5] <- 0
      }

      if(frequency(.wts) == 12){ aux <- c(3,6) }
      if(frequency(.wts) == 4){ aux <- c(1,2) }
      mVal <- as.character(tclvalue(pi2Value))
      if(mVal =="1")
         frec[aux[1]] <- 1
      if(mVal =="0")
         frec[aux[1]] <- 0
      mVal <- as.character(tclvalue(piValue))
      if(mVal =="1")
         frec[aux[2]] <- 1
      if(mVal =="0")
         frec[aux[2]] <- 0

      typesub <- tclvalue(rbsbValue)
      nsub <- as.numeric(tclvalue(nsub))
      .out <<- CH.rectest(.wts, type=typesub, nsub=nsub, frec, f0, DetTr=DetTr)
      show(.out)
    }
    tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttch,text="OK", command=OnOK)
  tkgrid(OK.but)
}

MakeKPSS.rectest <- function()
{
  ttkpss <- tktoplevel()
  tkwm.title(ttkpss, "KPSS test")

  string <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
  .wts <<- ExeString(c(string, ""))

  ltrunc <- tclVar(as.integer(3*sqrt(length(.wts))/13))
  tkgrid(tklabel(ttkpss, text="  Introduce the lag truncation parameter: \n (By default 3*sqrt(N)/13)",
       fg="blue"), rowspan=2)
  entry.ltrunc <- tkentry(ttkpss, width="5", textvariable=ltrunc)
  tkgrid(entry.ltrunc)

  rbsb1 <- tkradiobutton(ttkpss)
  rbsb2 <- tkradiobutton(ttkpss)
  rbsb3 <- tkradiobutton(ttkpss)
  rbsbValue <- tclVar("moving")
  tkconfigure(rbsb1, variable=rbsbValue, value="backw")
  tkconfigure(rbsb2, variable=rbsbValue, value="forw")
  tkconfigure(rbsb3, variable=rbsbValue, value="moving")

  tkgrid(tklabel(ttkpss, text="  Select the type of subsamples:", fg="blue"), sticky="w")
  tkgrid(tklabel(ttkpss, text="Backwards subsamples"), rbsb1)
  tkgrid(tklabel(ttkpss, text="Forwards subsamples"), rbsb2)
  tkgrid(tklabel(ttkpss, text="Moving subsamples"), rbsb3)

  nsub <- tclVar()
  entry.nsub <- tkentry(ttkpss, width="4", textvariable=nsub)
  tkgrid(tklabel(ttkpss, text="  Type the length of each subsample:", fg="blue"), sticky="w")
  tkgrid(entry.nsub)
  tkgrid(tklabel(ttkpss, text=" "))

  tkconfigure(.treeWidget, cursor="watch")
  OnOK <- function()
  {
     tkdestroy(ttkpss)
      typesub <- tclvalue(rbsbValue)
      nsub <- as.numeric(tclvalue(nsub))
     .out <<- KPSS.rectest(.wts, type=typesub, nsub=nsub, ltrunc=as.numeric(tclvalue(ltrunc)))
     show(.out)
     tkconfigure(.treeWidget, cursor="xterm")
  }
  OK.but <- tkbutton(ttkpss, text="OK", command=OnOK)
  tkgrid(OK.but)
}
