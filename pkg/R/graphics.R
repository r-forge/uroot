
rmg <- function(wts, krmg=NULL)
{
 # k es el número de periodos entre los que
   # se calcula el rango y la media

  N <- length(wts)   
  if(class(krmg) == "NULL")
    krmg <- integer(sqrt(N))
  media <- c(1:(N-krmg+1))
  rango <- c(1:(N-krmg+1))

  for(i in 1:length(media)){
    media[i]<- mean(wts[i:(i+(krmg-1))])
    rango[i]<- (max(wts[i:(i+(krmg-1))])-min(wts[i:(i+(krmg-1))]))
                          }
  cor.mr <- cor(media, rango, use = "complete")

  plot(media, rango, pch=20, main="Range-mean plot", xlab="Mean", ylab="Range")
  mtext(as.expression(substitute(cor(R,M)==cor.mr,
        list(cor.mr=round(cor.mr, 4)))), side = 3, line = 0.35, cex=0.7)

  cat("\n  Correlation range-mean: ", round(cor.mr, 4), "\n")
}

##~ Ver hacer como método de una clase sts, clase sts todo lo de ts más atributo Mseas. Ver class mts ???
##~ chequeado para s=4, s=12.
##~ probarlo con diferentes start(wts) end(wts)

# Build a matrix containing seasonal paths.

Msts <- function(wts)  ##~ hacer que trate a las columnas como ts, ts.union.
{
  s <- frequency(wts)
  seas.data <- split(wts, cycle(wts))
  Mseas <- matrix(NA, nrow=ceiling(length(wts)/s), ncol=s)
  ref1 <- c(rep(2, start(wts)[2]-1), rep(1, s-start(wts)[2]+1))
  ref2 <- c(rep(0, end(wts)[2]), rep(1, s-end(wts)[2]))
  for(i in 1:s)
    Mseas[ref1[i]:(nrow(Mseas)-ref2[i]),i] <- seas.data[[i]]  # unlist(seas.data[[i]])

  ynames <- as.integer(time(wts))
  if(s==4)
    snames <- c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
  else if(s==12)
    snames <- month.abb
  else
    snames <- paste("Seas", 1:s, sep="")

  Mseas <- matrix(Mseas, nrow=nrow(Mseas), ncol=ncol(Mseas),
           dimnames=list(as.character(ynames[1]:ynames[length(ynames)]), snames))

#  for(i in 1:ncol(Mseas))
#    Mseas[,i] <- ts(Mseas[,i], frequency=1, start=start(wts))
#  as.matrix(Mseas)
  Mseas
}

# Buys-Ballot plot.
##~ poner np-nl como opción.
bbplot <- function(wts, colour=c("SlateBlue","SeaGreen","red","magenta"))
{
  s <- frequency(wts)
  if(s==4){
    nl <- 4; np <- 1; fr <- 2
    snames <- c("Qrt1", "Qrt2", "Qrt3", "Qrt4")
  }
  else if(s==12){
    nl <- 3; np <- fr <- 4
    snames <- month.abb
  }
  else{
    if(identical(c(s/2 - as.integer(s/2)), 0)){
      nl <- 4; np <- s/nl
    }
    else{
      np <- floor((s+1)/4); nl <- c(rep(4, np-1), s/np+1)
    }
  }

  Mseas <- Msts(wts)

  aux <- Mseas[nrow(Mseas),]
  labaux <- c(na.omit(aux), Mseas[(nrow(Mseas)-1),which(is.na(aux))])

  xlim <- c(start(wts)[1], end(wts)[1]+1.5)
  ylim <- c(min(wts), max(wts))
  aux1 <- seq(1,s,nl); aux2 <- seq(nl,s,nl)

  if(s==4)
    opar <- par(mar=c(3,3.5,2,2), las=1)
  if(s==12)
    opar <- par(mfrow=c(fr/2, fr/2), mar=c(3,3.5,2,2), las=1)
  for(i in 1:np){
    ref <- aux1[i]:aux2[i]
    ts.plot(ts(Mseas[,ref], start=start(wts)[1]), xlab="", ylab="",
      xlim=xlim, ylim=ylim , col=colour)

    #text(xlim[2]-0.2, as.matrix(Mseas[,ref])[nrow(Mseas),], month.abb[ref])
    text(xlim[2]-0.2, labaux[ref], snames[ref], col=colour)
  }
  par(opar)
}

# Buys-Ballot annual plot.

bbaplot <- function(wts, years=NULL, colour=c("SlateBlue","SeaGreen","red","magenta"))
{
  if(class(years) == "NULL")
    years <- c(start(wts)[1], as.integer((start(wts)[1]+end(wts)[1])/2), end(wts)[1])

  s <- frequency(wts)
  c1 <- which(years < start(wts)[1])
  c2 <- which(years > end(wts)[1])
  if(length(c1) != 0 || length(c2) != 0)
    stop("  Years", years[c1], years[c2], "are out of the sample.")

  Mseas <- Msts(wts)
  ylabs <- numeric(0)
  ref <- numeric(0)
  for(i in 1:length(years))
    ref <- c(ref, which(as.numeric(dimnames(Mseas)[[1]]) == years[i]))

  opar <- par(las=1)
  ts.plot(t(Mseas[ref,]), xlab="Seasons", ylab="", xlim=c(1,s+0.7), col=colour)

  for(j in 1:length(years)){
    aux <- na.omit(Mseas[ref[j],])
    ylabs <- c(ylabs, aux[length(aux)])
  }

  text(s+0.6, ylabs, years, col=colour)
  par(opar)
}

# Buys-Ballot 3D plot.


bb3D <- function(wts, color=TRUE, x=30, y=30)
{
  require(tcltk)
  ttplot <- tktoplevel()
  tkconfigure(ttplot, cursor="fleur")
  tkwm.title(ttplot, "Rotate")

  bbpersp <- function(wts=wts, color=color, x=x, y=y)
  {
    if(color==TRUE)
       colores <- "lightgoldenrod"
    if(color==FALSE)
       colores <- grey((0:6)/6)

    Mseas <- Msts(wts)
  
    if(frequency(wts) == 12){ xlabel <- "Months"; ntic <- frequency(wts)/2}
    if(frequency(wts) == 4) { xlabel <- "Quarters"; ntic <- frequency(wts)}
    xx <- c(1:ncol(Mseas))
    yy <- c(start(wts)[1]:(start(wts)[1]+nrow(Mseas)-1))
  
    persp(xx, yy, t(Mseas), # main="Buys-Ballot 3D"
          theta = x, phi = y, expand = 0.5, xlab=xlabel, ylab="", zlab="", 
          shade=0.4, col = colores, ticktype="detailed", nticks=ntic)
  }

  bbpersp(wts, color=color, x=as.integer(x), y=as.integer(y))

  RightClick2 <- function(x,y)
  {
    rootx <- as.integer(tkwinfo("rootx",ttplot))
    rooty <- as.integer(tkwinfo("rooty",ttplot))
    xTxt <- as.integer(x)+rootx
    yTxt <- as.integer(y)+rooty
    bbpersp(wts, color=color, x=as.integer(x), y=as.integer(y))
  }
  tkbind(ttplot, "<Button-3>", RightClick2)
}

# Buys-Ballot contour plot.

bbcn <- function(wts, color=TRUE)  # MMp ó Mq de bbmp() y quarterg()
{
  if(color==TRUE)
     colores <- terrain.colors(200)
  if(color==FALSE)
     colores <- grey((0:32)/32)
  if(frequency(wts) == 12)
     xlabel <- month.abb
  if(frequency(wts) == 4){ xlabel <- c("Qrtr1", "Qrtr2", "Qrtr3", "Qrtr4")}

  Mseas <- Msts(wts)
  x <- c(1:ncol(Mseas))
  y <- c(start(wts)[1]:(start(wts)[1]+nrow(Mseas)-1))

  image(x, y, t(Mseas), # main="Buys-Ballot contour"
        las=1, xlab="", ylab="", xaxt="n", col = colores)
  mtext(xlabel[1:frequency(wts)], side=1, line=1, at=c(1:frequency(wts))) #, cex=0.7)
  contour(x, y, t(Mseas), add = TRUE, drawlabels = TRUE, col="blue")
}

##~ plotcycles: poner el polinomio en lugar de frecuencias (en doc tabla con frecuencias).

plotcycles <- function(wts)
{
  s <- frequency(wts)
  if(s==4)
   pdim <- c(2,2)
  if(s==12)
   pdim <- c(4,2)

  mains <- c("Frequency zero", "Frequency pi", "Frequencies (pi/2; 3pi/2)", "Frequencies (2pi/3; 4pi/3)",
             "Frequencies (pi/3; 5pi/3)", "Frequencies (5pi/6; 7pi/6)", "Frequencies (pi/6; 11pi/6)")

  Mfilt <- matrix(nrow=length(wts), ncol=(s/2+1))

  for(i in 1:ncol(Mfilt)){
    factors <- rep(1, (s/2+1))
    factors[i] <- 0
    Mfilt[,i] <- factorsdiff(wts, factors=factors)$Fil.wts
    ts(Mfilt, frequency=s, start=start(wts))
  }

  opar <- par(mfrow=pdim, las=1)
  plot(diff(wts, lag=s), xlab="", ylab="", main="Seasonal difference")
  for(i in 1:ncol(Mfilt)){
    fts <- ts(Mfilt[,i], frequency=s, start=start(wts))
    plot(fts, xlab="", ylab="", main=mains[i])
  }
  par(opar)
}
