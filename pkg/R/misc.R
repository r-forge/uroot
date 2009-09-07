
## Auxiliar functions

ret <- function(wts, k)
{
  N <- length(wts)
  wts.r <- matrix(NA, nrow=N, ncol=k); wts.r[,1] <- wts
  for(i in 1:(k-1))
    wts.r[(i+1):N, (i+1)] <- wts[1:(N-i)]
  wts.r <- data.frame(wts.r); dimnames(wts.r)[[2]] <- paste("Lag", 0:(k-1), sep=".")
  as.matrix(wts.r)
}

SeasDummy <- function(wts, type)
{
  s <- frequency(wts); t0 <- start(wts); N <- length(wts)

  if(type == "alg"){        # Empleada en Barsky & Miron (1989)
   auxD <- matrix(0,nrow=N, ncol=s)
   sq   <- seq(1,N,s)
   k    <- 0

   for(j in 1:s){
     ifelse(sq[length(sq)] + k > N, n <- length(sq)-1, n <- N)
     for(i in 1:n)
       auxD[sq[i]+k,j] <- 1
     k <- k+1
   }
   VFE <- auxD
   if(t0[2] != 1){
      VFE <- matrix(nrow=N, ncol=s)
      VFE[,1:(t0[2]-1)] <- auxD[,(s-t0[2]+2):s]; VFE[,t0[2]:s] <- auxD[,1:(s-t0[2]+1)]
                  }
   if(t0[2] == 1){ VFE <- auxD }
  }

  if(type == "trg"){        # Empleada en Granger & Newbold (1986)
   qq  <- s/2
   VFE <- matrix(nrow=N, ncol=(s-1))

   sq1 <- seq(1,qq*2,2)
   sq2 <- seq(2,qq*2,2)
   j   <- c(1:(qq-1))

   for(i in 1:N){
     for(k in 1:(s-qq-1)){
         VFE[i,sq1[k]] <- cos((j[k]*pi/qq)*i)
         VFE[i,sq2[k]] <- sin((j[k]*pi/qq)*i)
     }
     VFE[i,(s-1)] <- (-1)^i
   }
  }
  VFE
}

contts <- function(lm, a)
{
  var.coef <- vcov(lm)[a,a]
  se.coef  <- sqrt(var.coef)
  et       <- lm$coef[a]/se.coef
  list(se.coef=se.coef, t.stat=et)
}

# HEGY regressors

hegy.reg <- function(wts)
{
  s <- frequency(wts); ML <- ret(ret(wts, 2)[,2], s+1)

  if(s==4){
    y1 <- ML[,1] + ML[,2] + ML[,3] + ML[,4]
    y2 <- -(ML[,1] - ML[,2] + ML[,3] - ML[,4])
    y4 <- -(ML[,1] - ML[,3])
    y3 <- ret(y4, 2)[,2]
    Mypi <- data.frame(y1, y2, y3, y4)
  }

  if(s==12){
    y1 <- ML[,1] + ML[,2] + ML[,3] + ML[,4] + ML[,5] + ML[,6] +
          ML[,7] + ML[,8] + ML[,9] + ML[,10] + ML[,11] + ML[,12]
    y2 <- -(ML[,1] - ML[,2] + ML[,3] - ML[,4] + ML[,5] - ML[,6] +
            ML[,7] - ML[,8] + ML[,9] - ML[,10] + ML[,11] - ML[,12])
    y3 <- -(ML[,2] - ML[,4] + ML[,6] - ML[,8] + ML[,10] - ML[,12])
    y4 <- -(ML[,1] - ML[,3] + ML[,5] - ML[,7] + ML[,9] - ML[,11])
    y5 <- -0.5*(ML[,1] + ML[,2] - 2*ML[,3] + ML[,4] + ML[,5] - 2*ML[,6] +
                ML[,7] + ML[,8] - 2*ML[,9] + ML[,10] + ML[,11] - 2*ML[,12])
    y6 <- (sqrt(3)/2)*(ML[,1] - ML[,2] + ML[,4] - ML[,5] + ML[,7] - ML[,8] + ML[,10] - ML[,11])
    y7 <- 0.5*(ML[,1] - ML[,2] - 2*ML[,3] - ML[,4] + ML[,5] + 2*ML[,6] +
               ML[,7] - ML[,8] - 2*ML[,9] - ML[,10] + ML[,11] + 2*ML[,12])
    y8 <- -(sqrt(3)/2)*(ML[,1] + ML[,2] - ML[,4] - ML[,5] + ML[,7] + ML[,8] - ML[,10] - ML[,11])
    y9 <- -0.5*(sqrt(3)*ML[,1] - ML[,2] + ML[,4] - sqrt(3)*ML[,5] + 2*ML[,6] - sqrt(3)*ML[,7] +
                ML[,8] - ML[,10] + sqrt(3)*ML[,11] - 2*ML[,12])
    y10 <- 0.5*(ML[,1] - sqrt(3)*ML[,2] + 2*ML[,3] - sqrt(3)*ML[,4] + ML[,5] - ML[,7] +
                sqrt(3)*ML[,8] - 2*ML[,9] + sqrt(3)*ML[,10] - ML[,11])
    y11 <- 0.5*(sqrt(3)*ML[,1] + ML[,2] - ML[,4] - sqrt(3)*ML[,5] - 2*ML[,6] - sqrt(3)*ML[,7] -
                ML[,8] + ML[,10] + sqrt(3)*ML[,11] + 2*ML[,12])
    y12 <- -0.5*(ML[,1] + sqrt(3)*ML[,2] + 2*ML[,3] + sqrt(3)*ML[,4] + ML[,5] - ML[,7] -
                 sqrt(3)*ML[,8] - 2*ML[,9] - sqrt(3)*ML[,10] - ML[,11])
    Mypi <- data.frame(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12)
  }

  dimnames(Mypi)[[2]] <- paste("Ypi", 1:s, sep="")
  Mypi <- as.matrix(Mypi)
  Mypi
}

crpp <- function(vcoef1, vrfil1, vcoef2, vrfil2)
{
  n1 <- length(vcoef1)
  n2 <- length(vcoef2)
  fcoef <- rep(NA, n1*n2)
  rfil  <- rep(NA, n1*n2)
  fcoef0 <- rep(0, n1*n2)
  rfil0  <- rep(0, n1*n2)
  k <- 1
  for(i in 1:n1){
    for(j in 1:n2){
       fcoef0[k] <- vcoef1[i]*vcoef2[j]
       rfil0[k]  <- vrfil1[i]+vrfil2[j]
       k <- k+1
                  }
                }
  # Simplificar
  for(i in 1:(n1*n2)){
     simpl <- which(rfil0 == rfil0[i])
     if(length(simpl)>0){
        fcoef[i] <- sum(fcoef0[simpl]); fcoef0[simpl] <- NA
        rfil[i]  <- rfil0[i]  ; rfil0[simpl]  <- NA
                        }
                   }
  fcoef[which(fcoef==0)] <- rfil[which(fcoef==0)] <- NA
  fcoef <- na.omit(fcoef)[1:length(na.omit(fcoef))]
  rfil  <- na.omit(rfil)[1:length(na.omit(rfil))]

  # Ordenar
  fcoef0 <- rfil0 <- c(1:length(rfil))
  for(j in 1:length(rfil)){
    wm        <- which.min(rfil)
    rfil0[j]  <- rfil[wm]
    fcoef0[j] <- fcoef[wm]
    rfil[wm]  <- fcoef[wm] <- Inf
                          }
  rfil <- rfil0; fcoef <- fcoef0
  list(fcoef, rfil)
}

factorsdiff <- function(wts, factors)
{
  N     <- length(wts)
  ML    <- ret(wts, frequency(wts)+1)
  VCOEF <- cbind(c(1,-1,0),c(1,1,0),c(1,1,0),c(1,1,1),c(1,-1,1), c(1,sqrt(3),1),c(1,-sqrt(3),1))
  VRFIL <- cbind(c(0,1,0),c(0,1,0),c(0,2,0),c(0,1,2), c(0,1,2),c(0,1,2),c(0,1,2))

  if(length(which(factors == 1)) == 1)
  {
    Rfil  <- c(1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
    Fcoef <- c(-1,1,1,1,1,-1,1,sqrt(3),1,-sqrt(3),1)

    frfil <- which(factors == 1)
    Fil.vari <-  ML[,1] + Fcoef[frfil]*ML[,Rfil[frfil]+1]

    auxt0 <- length(wts)-length(na.omit(Fil.vari))
    Fil.vari <- ts(Fil.vari, frequency=frequency(wts), start=start(wts))
    fcoef <- Fcoef[frfil]
    rfil <- Rfil[frfil]
  }

  if(length(which(factors == 1)) > 1)
  {
    frfil <- which(factors == 1)
    fcoefrfil <- crpp(VCOEF[,frfil[1]], VRFIL[,frfil[1]], VCOEF[,frfil[2]], VRFIL[,frfil[2]])
    fcoef <- fcoefrfil[[1]]
    rfil  <- fcoefrfil[[2]]
    if(length(which(factors == 1)) > 2){
      for(i in 3:length(frfil)){
        fcoefrfil <- crpp(fcoef, rfil, VCOEF[,frfil[i]], VRFIL[,frfil[i]])
        fcoef <- fcoefrfil[[1]]
        rfil  <- fcoefrfil[[2]]
                               }
                                      }
    Fil.vari.aux  <- matrix(nrow=N, ncol=length(rfil))
    Fil.vari      <- matrix(nrow=N, ncol=1)

    for(i in 1:length(rfil))
       Fil.vari.aux[,i] <- fcoef[i]*ML[,(rfil[i]+1)]
    for(i in 1:N)
       Fil.vari[i,1] <- sum(Fil.vari.aux[i,])
    Fil.vari <- ts(Fil.vari, frequency=frequency(wts), start=start(wts))
  }
  list(Fil.wts=Fil.vari, fcoef, rfil)
}

## selectP

# Basado en top-down AIC y BIC.  ##~ Probar bic en lm1 y aic en lm2.

selPabic <- function(lmdet, type, Pmax=NULL)
{
  if(mode(Pmax) != "numeric") Pmax <- round(10*log10(length(lmdet$model[,1])))
  switch(type, aic = k <- 2,
               bic = k <- log(length(lmdet$model[,1])),)

  ML <- ret(lmdet$model[,1], Pmax+1)[,-1]
  drop <- NULL
  for(i in Pmax:1){
    ifelse(length(drop) == 0,
      lm1 <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)]),
      lm1 <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)[-drop]]))
    aic1 <- AIC(lm1, k=k)
    #ifelse(length(drop) == (Pmax-1),
    ifelse(identical(drop, as.numeric(c(2:Pmax))),
      aic2 <- AIC(lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1])), k=k),
      aic2 <- AIC(lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML[,c(1:Pmax)[-c(drop,i)]]), k=k))

    if(aic1 >= aic2)
      drop <- c(drop, i)
  }

  ifelse(length(drop) == 0, lags <- c(1:Pmax), lags <- c(1:Pmax)[-drop])
  lags
}

# Basado en retardos significativos, al 10% por defecto.
## ** en bsadf puede ocurrit que matriz de retardos tiene columas distintas pero todas aproximadamente cero, entonces el coeficiente estimado es NA, hay que quitarlo. En selPabic no se considera esto.

selPsignf <- function(lmdet, cvref=1.65, Pmax=NULL)
{
  if(mode(cvref) != "numeric") cvref <- 1.65  # 10% approx.
  if(mode(Pmax) != "numeric") Pmax <- round(10*log10(length(lmdet$model[,1])))

  ref <- ncol(model.matrix(lmdet))
  ML <- ret(lmdet$model[,1], Pmax+1)[,-1]
  drop <- NULL; cond <- TRUE

  while(cond == TRUE){
    lmref <- lm(lmdet$model[,1] ~ 0+as.matrix(lmdet$model[,-1]) + ML)
    #Nreg <- length(coef(lmref))
    #tstats <- coef(summary(lmref))[(ref+1):Nreg,3]; drop <- which(abs(tstats) < cvref)  **
    rcoefs <- na.omit(coef(summary(lmref))[,3]); Nreg <- length(coef(lmref))
    aux1 <- which(is.na(coef(lmref)[(ref+1):Nreg]))
    tstats <- rcoefs[(ref+1):length(rcoefs)]; drop <- c(aux1, which(abs(tstats) < cvref))

    if(length(aux1) == 0){
      cond <- FALSE
      aux <- names(data.frame(ML))[-drop]
      lags <- as.numeric(substr(aux, 5, nchar(aux)))
    }
    if(length(aux1) == ncol(ML)){
      cond <- FALSE
      lags <- numeric(0)
    }
    if(length(aux1) > 0 && length(drop) < ncol(ML)){
      aux <- dimnames(ML)[[2]][-drop]
      ML <- as.matrix(data.frame(ML[,-drop]))
      dimnames(ML)[[2]] <- aux
    }
  }
  lags
}

#polymake <- function(roots)
#{
#  coefs <- rep(NA, length(roots))
#  coefs[1] <- 1
#
#  coefs[2] <- -roots[1]
#  if(length(roots) > 1){
#    for(i in 2:length(roots)){
#      coefs[i+1] <- -coefs[i]*roots[i]
#      for(j in i:2)
#        coefs[j] <- coefs[j] - coefs[j-1]*roots[i]
#    }
#  }
#
#  #coefs <- c(1, coefs)
#  coefs <- coefs/coefs[length(coefs)]
#  #coefs <- round(coefs[c(length(coefs):1)], 2)  ##~ ver. al redondear una raíz 0.999 pasa a ser 1.
#  coefs <- Re(coefs[c(length(coefs):1)])  ## para posibles problemas de redondeo.
#  coefs
#}

elapsedtime <- function(ptm1, ptm2)
{
  elaps <- (ptm2 - ptm1)[1]  # en segundos

  if(elaps < 60)
         units <- "seconds"
  else if(elaps < 3600){
         elaps <- elaps/60
         units <- "minutes" }
  else if(elaps < 86400){
         elaps <- elaps/3600
         units <- "hours" }
  else { elaps <- elaps/86400
         units <- "days" }

  list(elaps=as.numeric(elaps), units=units)
}

#elapsedtime <- function(time1, time2)
#{
#  t1 <- as.numeric(unlist(strsplit(substring(time1, 9,19), ":")))
#  t2 <- as.numeric(unlist(strsplit(substring(time2, 9,19), ":")))
#
#  d1<- as.numeric(unlist(strsplit(substring(time1, 1,8), "/")))
#  d2<- as.numeric(unlist(strsplit(substring(time2, 1,8), "/")))
#
#  elaps <- ISOdatetime(year=d2[3],month=d2[1],day=d2[2], hour=t2[1], min=t2[2], sec=t2[3]) -
#           ISOdatetime(year=d1[3],month=d1[1],day=d1[2], hour=t1[1], min=t1[2], sec=t1[3])
#
#  if(length(which(d1 != d2)) == 0){
#    units <- c("hours", "minutes", "seconds")[which(t1 != t2)[1]]
#  } else
#      units <- c("years", "days", "months")[which(d1 != d2)[1]]
#
#  list(elaps=as.numeric(elaps), units=units)
#}

# Función para redondear las tabla que se exportan (usando cat...) permitiendo que el último
# decimal sea cero.
  # tabla es una coulumna de matriz de datos
  # colum es una columna de la tabla
  # digits es el número de decimales (se permite que el último sea cero)
# format indicando un valor de digits vale si es una matriz con todo números, no otos caracteres, **,...

Tround <- function(tabla, column, digits)
{
  catround <- function(x, digits)
  {
    rx <- as.character(round(x, digits=digits))

    rxchar <- rep(NA, nchar(rx))
    for(i in 1:nchar(rx))
      rxchar[i] <- substring(rx, i, i)

    if(length(which(rxchar==".")) == 0)
    {
       aux1 <- rep("0", digits); aux2 <- "0"
       for(i in 1:(digits-1))
          aux2 <-  paste(aux2, aux1[i], sep="")
       rx <- paste(rx, aux2, sep=".")
    }
    if(digits > 0 && length(which(rxchar==".")) > 0)
    {
      logic <- FALSE; i <- 1
      if(substr(rx, 2, 2) != "")
      {
        while(logic == FALSE){
          logic <- substr(rx, i, i) == "."
          i <- i+1
        }
        if(nchar(substr(rx, i, i+digits)) < digits){
           n0 <- digits - nchar(substr(rx, i, i+digits))
           aux1 <- rep("0", n0)
           for(i in 1:(n0-1))
              aux2 <-  paste(aux1[i], aux1[i+1], sep="")
           rx <- paste(rx, aux2, sep="")
        }
      }
      if(nchar(rx) == 1){
        aux1 <- rep("0", digits)
        for(i in 1:(digits-1))
          aux2 <-  paste(aux1[i], aux1[i+1], sep="")
        rx <- paste(rx, aux2, sep=".")
      }
    }
    rx
  }

  for(i in 1:nrow(tabla))
    tabla[i,column] <- catround(as.numeric(tabla[i,column]), digits)
  tabla
}
