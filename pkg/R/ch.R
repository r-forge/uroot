
setClass("chstat", representation(wts="ts", frec="numeric", DetTr="logical", ltrunc="numeric",
  Fhat="matrix", Omfhat="matrix", MA="matrix", stat="numeric"))

CH.test <- function(wts, frec=NULL, f0=1, DetTr=FALSE, ltrunc=NULL)
{
  s <- frequency(wts); t0 <- start(wts); N <- length(wts)
  if(class(frec)=="NULL") frec <- rep(1, s/2)
  if(class(ltrunc)=="NULL") ltrunc <- round(s*(N/100)^0.25)

  # Regressors for the auxiliar regression

  R1 <- SeasDummy(wts, "trg")
  VFEalg <- SeasDummy(wts, "alg")
  tiempo <- c(1:N)
 
  if(DetTr ==FALSE){
    if(f0 == 1){
      lmch  <- lm(wts[2:N] ~ wts[1:(N-1)] + R1[2:N,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
    if(f0 == 0){
      lmch  <- lm(wts ~ R1[,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
  }
  if(DetTr ==TRUE){
    if(f0 == 1){
      lmch  <- lm(wts[2:N] ~ wts[1:(N-1)] + tiempo[1:(N-1)] + R1[2:N,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
    if(f0 == 0){
      lmch  <- lm(wts ~ tiempo + R1[,1:(s-1)])
      ehat <- matrix(lmch$residuals, ncol=1)
    }
  }

  # Fhat
  Fhat <- Fhataux <- matrix(nrow=length(ehat), ncol=(s-1))
 
  #ifelse(f0 == 1, op <- 1, op <- 0) # también cambia varnw[3:13...]
  for(i in 1:length(ehat))
    Fhataux[i,] <- R1[i+f0,]*ehat[i]   # No producto matricial %*%
 
  for(i in 1:nrow(Fhat)){
    for(n in 1:(s-1))
      Fhat[i,n] <- sum(Fhataux[1:i,n])
  }
 
  # Omega supra-f estimate  (Omfhat)
  
  wnw <- 1 - seq(1,ltrunc,1)/(ltrunc+1)
  Ne <- nrow(Fhataux)
  Omnw <- 0
  for(k in 1:ltrunc)
    Omnw <- Omnw + (t(Fhataux)[,(k+1):Ne] %*% Fhataux[1:(Ne-k),]) * wnw[k]
  Omfhat <- (crossprod(Fhataux) + Omnw + t(Omnw))/Ne
  
  # Matriz A
  # frec <- c(0,1)          # input for quarterly series
  # frec <- c(1,0,0,0,0,0)  # input for monthly series

  #sq     <- seq(1,11,2)
  sq <- seq(1,(s-1),2)
  frecob <- rep(0,(s-1))

  for(i in 1:length(frec)){
    if(frec[i] == 1 && i == s/2)
       frecob[sq[i]]     <- 1
    if(frec[i] == 1 && i < s/2)
       frecob[sq[i]] <- frecob[sq[i]+1] <- 1
  }
 
  a <- length(which(frecob == 1))
  A <- matrix(0, nrow=s-1, ncol=a)
 
  j <- 1
  for(i in 1:(s-1))
    if(frecob[i] == 1){
      A[i,j] <- 1
      ifelse(frecob[i] == 1, j <- j+1, j <- j)
    }

  # Statistic (equation (13) Canova & Hansen (1995))
 
  stL <- (1/N^2)*sum(diag( solve(t(A) %*% Omfhat %*% A)
                           %*%
                           t(A) %*% t(Fhat) %*% Fhat %*% A ))
  
  new("chstat", wts=wts, frec=frec, DetTr=DetTr, ltrunc=ltrunc,
    Fhat=Fhat, Omfhat=Omfhat, MA=A, stat=stL)
}

setMethod("show", "chstat",
  function(object)
  {
    s <- frequency(object@wts)
    if(s == 4)
      fnames <- c("pi/2", "pi")
    if(s == 12)
      fnames <- c("pi/6", "pi/3", "pi/2", "2pi/3", "5pi/6", "pi")
    ref <- which(object@frec==1)
    cvtable <- lookupCVtable(code=paste("CHp", sum(object@frec*c(rep(2, s/2-1),1)), sep=""))[1,]
    dimnames(cvtable)[[1]] <- ""

    cat("\n  ------ - ------ ----")
    cat("\n  Canova & Hansen test")
    cat("\n  ------ - ------ ----\n\n")

    cat("  Null hypothesis: Stationarity.\n")
    cat("  Alternative hypothesis: Unit root.\n")
    cat("  Frequency of the tested cycles:", paste(fnames[ref], collate=","), "\n\n")

    cat(c("  L-statistic:", round(object@stat, 3), " \n"))
    cat(c("  Lag truncation parameter:", object@ltrunc, "\n\n"))
    cat("  Critical values:\n\n")
    print(cvtable)
    cat("\n")
  }
)
