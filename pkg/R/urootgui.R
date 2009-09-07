# GUI SCHEME

urootgui <- function()
{
  require(tcltk) || stop("The package tcltk is not available")
  addTclPath(system.file(package="uroot"))
  tclRequire("BWidget")
#
  .tt <<- tktoplevel()
  tkwm.title(.tt, "uroot R-GUI 1.4")

  xScr        <- tkscrollbar(.tt, command=function(...)tkxview(.treeWidget,...),
                            orient="horizontal")
  yScr        <- tkscrollbar(.tt, command=function(...)tkyview(.treeWidget,...))

  .treeWidget <<- tkwidget(.tt, "Tree", xscrollcommand=function(...)tkset(xScr,...),
           yscrollcommand=function(...)tkset(yScr,...), width=45, height=17, bg="aquamarine3")

  #xScr        <- tkscrollbar(.tt, command=function(...)tkxview(.treeWidget,...),
  #                          orient="horizontal")
  #yScr        <- tkscrollbar(.tt, command=function(...)tkyview(.treeWidget,...))
  tkgrid(.treeWidget, yScr)
  tkgrid.configure(yScr, stick="nsw")
  tkgrid(xScr)
  tkgrid.configure(xScr, stick="new")
#

# Mouse right button
  editPopupMenu <- tkmenu(.tt, tearoff=FALSE)
  # tkadd(editPopupMenu, "command", label="Export graphic", command=ExportGraph)
  tkadd(editPopupMenu, "command", label="Info", command=function() wtsinfo_treeW())
  tkadd(editPopupMenu, "command", label="Delete", command=function()
         tkdelete(.treeWidget, tclvalue(tkcmd(.treeWidget,"selection","get"))))
  RightClick <- function(x,y) # x e y son las coordenadas del ratón
  {
    rootx <- as.integer(tkwinfo("rootx", .tt))
    rooty <- as.integer(tkwinfo("rooty", .tt))
    xTxt <- as.integer(x)+rootx
    yTxt <- as.integer(y)+rooty
    .Tcl(paste("tk_popup",.Tcl.args(editPopupMenu,xTxt,yTxt)))
  }
  tkbind(.tt, "<Button-3>", RightClick)

  topMenu <- tkmenu(.tt)
  tkconfigure(.tt, menu=topMenu)

# ARCHIVO

FileMenu <- tkmenu(topMenu, tearoff=FALSE)
OpenMenu <- tkmenu(topMenu, tearoff=FALSE)

#tkadd(OpenMenu, "command", label="workspace", command=function() LoadWorkSpace())
tkadd(OpenMenu, "command", label="source code", command=function() OpenSourceFile())
tkadd(FileMenu, "cascade", label="Open", menu=OpenMenu)

#tkadd(FileMenu, "command", label="Save", command=function() save.image())
#tkadd(FileMenu, "command", label="Save as", command=function() SaveAsWorkSpace())

tkadd(FileMenu, "command", label="Quit", command=function() closerusea())

tkadd(topMenu, "cascade", label="File", menu=FileMenu)

# DATOS

DataMenu      <- tkmenu(topMenu, tearoff=FALSE)
#DataInfoMenu <- tkmenu(topMenu, tearoff=FALSE)
ImportMenu    <- tkmenu(topMenu, tearoff=FALSE)
BDatosMenu    <- tkmenu(topMenu, tearoff=FALSE)
TransfMenu    <- tkmenu(topMenu, tearoff=FALSE)
PMtrMenu      <- tkmenu(topMenu, tearoff=FALSE)

tkadd(ImportMenu, "command", label="from CSV file",
      command=function() ReadDataCSV())
# tkadd(ImportMenu, "command", label="from SPSS",
#      command=function() ReadSPSS())
tkadd(DataMenu, "cascade", label="Import data", menu=ImportMenu)
#tkadd(DataMenu, "command", label="Description", command=function() GetDataInfo())

tkadd(BDatosMenu, "command", label="CAPV data bank", command=function() dataIKERBIDE())
tkadd(BDatosMenu, "command", label="INE data bank", command=function() dataINE())
tkadd(BDatosMenu, "command", label="Franses (1996)", command=function() dataFranses())
tkadd(DataMenu, "cascade", label="Data bank", menu=BDatosMenu)

#tkadd(DataMenu, "command", label="Simulate DGP", command=function() MakeDGPsim())

tkadd(TransfMenu, "command", label="Logarithmic", command=function(){
      string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
      .wts   <<- ExeString(c(string, ""))
      newstring <- paste("log_", string, sep="")

      wts <- log(.wts, base=exp(1))
      assign(newstring, wts, env=.GlobalEnv)
      tkinsert(.treeWidget,"end", string, newstring, text=newstring)
})

tkadd(TransfMenu, "command", label="Box-Cox transformation", command=function() Makeboxcox())

tkadd(TransfMenu, "command", label="--- --- ---", command=function(){})

tkadd(TransfMenu, "command", label="First differences", command=function(){
        string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
        .wts   <<- ExeString(c(string, ""))
        newstring <- paste("fdiff_", string, sep="")

        wts  <- diff(.wts, lag=1)

        assign(newstring, wts, env=.GlobalEnv)
        tkinsert(.treeWidget,"end", string, newstring, text=newstring)
})

tkadd(TransfMenu, "command", label="Seasonal differences", command=function(){
        string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
        .wts   <<- ExeString(c(string, ""))
        newstring <- paste("sdiff_", string, sep="")

        wts  <- diff(.wts, lag=frequency(.wts))

        assign(newstring, wts, env=.GlobalEnv)
        tkinsert(.treeWidget,"end", string, newstring, text=newstring)
})

tkadd(TransfMenu, "command", label="First and seasonal differences", command=function(){
        string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
        .wts   <<- ExeString(c(string, ""))
        newstring <- paste("fsdiff_", string, sep="")

        wts <- diff(diff(.wts, lag=frequency(.wts)), lag=1)

        assign(newstring, wts, env=.GlobalEnv)
        tkinsert(.treeWidget,"end", string, newstring, text=newstring)
})

#tkadd(TransfMenu, "command", label="Periodic differences", command=function(){
#       string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
#       .wts   <<- ExeString(c(string, ""))
#       newstring <- paste("pdiff_", string, sep="")
#
#       wts  <- perdiff(.wts)
#       t0aux <- ysooys(start(.wts), start(.wts), length(.wts), frequency(.wts))
#       t0    <- t0aux[[2]][(t0aux[[1]]+1),1:2]
#       wts  <- ts(wts[2:length(wts)], frequency=frequency(.wts), start=t0)
#       assign(newstring, wts, env=.GlobalEnv)
#       tkinsert(.treeWidget,"end", string, newstring, text=newstring) })
#
tkadd(TransfMenu, "command", label="--- --- ---", command=function(){})

tkadd(TransfMenu, "command", label="Filter", command=function(){Makefactorsdiff()})

tkadd(TransfMenu, "command", label="--- --- ---", command=function(){})

tkadd(TransfMenu, "command", label="Remove deterministic component", command=function(){
      #MakeTransfdet()
      string    <- tclvalue(tkcmd(.treeWidget, "selection", "get"))
      .wts   <<- ExeString(c(string, ""))
      newstring <- paste("undet_", string, sep="")

      wts  <- Transfdet(.wts)
      wts  <- ts(wts, frequency=frequency(.wts), start=start(.wts))
      assign(newstring, wts, env=.GlobalEnv)
      tkinsert(.treeWidget,"end", string, newstring, text=newstring)
})

tkadd(DataMenu, "cascade", label="Transformation", menu=TransfMenu)

tkadd(PMtrMenu, "command", label="Change",
      command=function() CambiarPeriodo())
#tkadd(PMtrMenu, "command", label="Restore", command=function(){
#        t0 <<- start(.wts)t
#        wts <<- ts(.wtst, frequency=frequency(.wts), start=t0); N <<- length(wts)
#
        #change.wts.label(c("t0", "wts", "N"))
#        tkmessageBox(message="Original sample period has been restored", icon="info") })

tkadd(DataMenu, "cascade", label="Sample period", menu=PMtrMenu)
tkadd(topMenu, "cascade", label="Data", menu=DataMenu)

# GRÁFICOS

GrafMenu      <- tkmenu(topMenu, tearoff=FALSE)
TransGrafMenu <- tkmenu(topMenu, tearoff=FALSE)
SpecMenu      <- tkmenu(topMenu, tearoff=FALSE)
BBGrafMenu    <- tkmenu(topMenu, tearoff=FALSE)
SeasMenu      <- tkmenu(topMenu, tearoff=FALSE)
BBmpMenu      <- tkmenu(topMenu, tearoff=FALSE)
MonthplotMenu <- tkmenu(topMenu, tearoff=FALSE)

tkadd(TransGrafMenu, "command", label="original", command=function() Makeplotwts())
tkadd(TransGrafMenu, "command", label="logarithms", command=function() Makeplotlog())
tkadd(TransGrafMenu, "command", label="Box-Cox transformation", command=function()
      Makeplotboxcox())
tkadd(TransGrafMenu, "command", label="--- --- ---", command=function(){})

tkadd(TransGrafMenu, "command", label="first differences",
      command=function() Makeplotdelta())
tkadd(TransGrafMenu, "command", label="seasonal differences",
      command=function() Makeplotdeltas())
tkadd(TransGrafMenu, "command", label="first and seasonal differences", command=function()
      Makeplotddeltas())
#tkadd(TransGrafMenu, "command", label="periodic differences", command=function() Makeplotperdiff())
tkadd(TransGrafMenu, "command", label="--- --- ---", command=function(){})

tkadd(TransGrafMenu, "command", label="without deterministic component",
      command=function(){ wtsodet <- Transfdet(.wtsf); plot(wtsodet)})
tkadd(GrafMenu, "cascade", label="Series", menu=TransGrafMenu)

tkadd(GrafMenu, "command", label="Range-mean", command=function() Makermp())
tkadd(GrafMenu, "command", label="Correlograms", command=function() Makecorrgrm())

tkadd(SpecMenu, "command", label="of the original series",
      command=function() Makespec(dif1=FALSE))
tkadd(SpecMenu, "command", label="of the first differences",
      command=function() Makespec(dif1=TRUE))
tkadd(GrafMenu, "cascade", label="Spectrum", menu=SpecMenu)

tkadd(BBGrafMenu, "command", label="Anual path", command=function() Makebbaplot())

tkadd(SeasMenu, "command", label="of the original series",
      command=function() Makebbplot("orig"))
tkadd(SeasMenu, "command", label="of the first differences",
      command=function() Makebbplot("fdiff"))
#tkadd(SeasMenu, "command", label="of the periodic differences",
#      command=function() Makebbplot("pdiff"))
tkadd(BBGrafMenu, "cascade", label="Seasonal paths", menu=SeasMenu)

tkadd(BBGrafMenu, "command", label="Buys-Ballot 3D",
      command=function() Makebb3D())
tkadd(BBGrafMenu, "command", label="Contour",
      command=function() Makebbcn())
tkadd(GrafMenu, "cascade", label="Buys-Ballot",
      menu=BBGrafMenu)

tkadd(MonthplotMenu, "command", label="of the original series", command=function() Makemonthplot("orig"))
tkadd(MonthplotMenu, "command", label="of the first differences", command=function() Makemonthplot("fdiff"))
tkadd(GrafMenu, "cascade", label="Seasonal series", menu=MonthplotMenu)

tkadd(GrafMenu, "command", label="Seasonal box plot", command=function() MakeSeasboxplot())

tkadd(GrafMenu, "command", label="Filter frequencies", command=function() MakeFactors())

tkadd(topMenu, "cascade", label="Graphics", menu=GrafMenu)

# CONTRASTES

TestMenu  <- tkmenu(topMenu, tearoff=FALSE)
ADFMenu    <- tkmenu(topMenu, tearoff=FALSE)
KPSSMenu  <- tkmenu(topMenu, tearoff=FALSE)
HEGYMenu  <- tkmenu(topMenu, tearoff=FALSE)
CHMenu    <- tkmenu(topMenu, tearoff=FALSE)

tkadd(ADFMenu, "command", label="original sample", command=function() MakeADF.test())
tkadd(ADFMenu, "command", label="recursive testing", command=function() MakeADFHEGY.rectest("ADF"))
tkadd(TestMenu, "cascade", label="ADF", menu=ADFMenu)

tkadd(KPSSMenu, "command", label="original sample", command=function() MakeKPSS.test())
tkadd(KPSSMenu, "command", label="recursive testing", command=function() MakeKPSS.rectest())
tkadd(TestMenu, "cascade", label="KPSS", menu=KPSSMenu)

tkadd(HEGYMenu, "command", label="original sample", command=function() MakeHEGY.test())
tkadd(HEGYMenu, "command", label="recursive testing", command=function() MakeADFHEGY.rectest("HEGY"))
tkadd(TestMenu, "cascade", label="HEGY", menu=HEGYMenu)

tkadd(CHMenu, "command", label="original sample", command=function() MakeCH.test())
tkadd(CHMenu, "command", label="recursive testing", command=function() MakeCH.rectest())
tkadd(TestMenu, "cascade", label="CH", menu=CHMenu)

tkadd(topMenu, "cascade", label="Tests", menu=TestMenu)

# UTILIDADES

UtilMenu   <- tkmenu(topMenu, tearoff=FALSE)
LatexMenu  <- tkmenu(topMenu, tearoff=FALSE)
PanelMenu  <- tkmenu(topMenu, tearoff=FALSE)
PanelqMenu <- tkmenu(topMenu, tearoff=FALSE)
PanelmMenu <- tkmenu(topMenu, tearoff=FALSE)

#tkadd(UtilMenu, "command", label="Remove all objects", command=function() Makermall())
tkadd(UtilMenu, "command", label="Save current graph", command=function() ExportGraph())

#tkadd(LatexMenu, "command", label="View .out object in LaTeX format", command=function(){
#  if(exists(".out") == FALSE){
#    stop()
#    cat("There is no output to view. A test has not been yet performed.\n")
#  } else
#    urt.xtable(.out)
#})

tkadd(LatexMenu, "command", label="Export .out object to a LaTeX file", command=function(){
  if(exists(".out") == FALSE){
    stop("There is no output to view. A test has not been yet performed.\n")
  } else
    save.xtable(.out)
})
tkadd(UtilMenu, "cascade", label="LaTeX output", menu=LatexMenu)

tkadd(PanelMenu, "command", label="Correlograms", command=function() MakePanelqmCorrg())
tkadd(PanelMenu, "command", label="Seasonal frequencies", command=function() Makeplotcycles())

tkadd(PanelqMenu, "command", label="Selected graphics", command=function() MakeQPanel1())

tkadd(PanelmMenu, "command", label="Selected graphics", command=function() MakePanelmSerie())
tkadd(PanelMenu, "cascade", label="Quarterly Series ", menu=PanelqMenu)
tkadd(PanelMenu, "cascade", label="Monthly series ", menu=PanelmMenu)
tkadd(UtilMenu, "cascade", label="Panel", menu=PanelMenu)

tkadd(topMenu, "cascade", label="Utilities", menu=UtilMenu)

# urca PACKAGE
#urcaMenu  <- tkmenu(topMenu, tearoff=FALSE)
#ca.joMenu <- tkmenu(topMenu, tearoff=FALSE)
#tkadd(ca.joMenu, "command", label="eigen statistic", command=function() Make.ca.jo(type="eigen"))
#tkadd(ca.joMenu, "command", label="trace statistic", command=function() Make.ca.jo(type="trace"))
#tkadd(urcaMenu, "cascade", label="Tests", menu=ca.joMenu)
#tkadd(topMenu, "cascade", label="urca", menu=urcaMenu)

# AYUDA

AyudaMenu <- tkmenu(topMenu, tearoff=FALSE)

tkadd(AyudaMenu, "command", label="About uroot", command=function()
     mytkpager(system.file("DESCRIPTION", package="uroot"), title="About uroot",
               header="", delete.file=FALSE, wwidth=90, wheight=30, export=FALSE))
#browseURL(file.path(R.home(), "library/urootcook/html/about.html"), browser=getOption("browser")))

tkadd(AyudaMenu, "command", label="Html help", command=function()
      browseURL(system.file("html", "00Index.html", package="uroot"), browser=getOption("browser")))

#tkadd(AyudaMenu, "command", label="Maintainer homepage", command=function()
#      browseURL("http://www.bl.ehu.es/~jedlobej", browser=getOption("browser")))

tkadd(topMenu, "cascade", label="  Help", menu=AyudaMenu)
}
urootgui()
