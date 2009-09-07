
setClass("vdate", representation(input="numeric", output="matrix", wts="ts",
  ys="matrix", obs="matrix", Myso="matrix"))

Mdates <- function(wts, yso)
{
  ys <- as.integer(time(wts))
  seas <- cycle(wts)@.Data

  Myso <- matrix(nrow=length(seas), ncol=4,
          dimnames=list(c(1:length(seas)), c("Year", "Season", "Observation", "ys")))
  Myso[,1] <- ys
  Myso[,2] <- seas
  Myso[,3] <- c(1:nrow(Myso))
  Myso[,4] <- paste(ys, seas, sep=":")

  if(length(yso) == 2){
    ys <- matrix(t(yso), ncol=2, dimnames=list("", c("Year", "Season")))
    obs <- out <- which(Myso[,4] == paste(yso[1], yso[2], sep=":"))
    if(length(out) != 0){
      obs <- matrix(obs, dimnames=list("", "Observation")) }
  }
  if(length(yso) == 1){
    obs <- matrix(yso, dimnames=list("", "Observation"))
    ys <- out <- Myso[which(Myso[,3] == yso),1:2]
    #if(nrow(out) != 0)
    #  dimnames(ys)[[1]] <- ""
  }

  new("vdate", input=as.numeric(yso), output=as.matrix(t(out)), wts=wts,
    ys=matrix(ys), obs=matrix(obs), Myso=Myso)
}

setMethod("show", "vdate",
  function(object)
  {
    input <- object@input
    output <- object@output

    if(length(input) == 1){
      if(length(output) == 0){
        cat("  Observation", as.numeric(object@obs), "is out of sample.\n")
      } else{
          refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
          cat("  Observation", as.numeric(object@obs), "corresponds to date", refys, ".\n")
        }
    }
    if(length(input) == 2){
      refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
      if(length(output) == 0){
        cat("  Date", refys, "is out of sample.\n")
      } else{
          refys <- paste(object@ys[[1]], object@ys[[2]], sep=":")
          cat("  Date", refys, "corresponds to observation", as.numeric(object@obs), ".\n")
        }
    }
  }
)

setMethod("as.numeric", "vdate",
  function(x,...)
  {
    out <- x@output
    out
  }
)

as.vdate <- function(object, yso=1){
}
setMethod("as.vdate", "ts",
  function(object, yso)
  {
    #Mdates(object, yso=start(object))
    Mdates(object, yso=yso)
  }
)

stepdate <- function(object, step=1){
}
setMethod("stepdate", "vdate",
  function(object, step)
  {
    obs0 <- object@obs
    if(obs0+step > length(object@wts))
      stop("Observation ", obs0+step, " is out of sample.\n")
    ys0 <- object@ys
    Myso <- object@Myso

    obsout <- matrix(obs0+step, dimnames=list("", "Observation"))

    ref <- which(Myso[,4] == paste(ys0[1], ys0[2], sep=":"))
    ysout <- Myso[ref+step,4]
    ysout <- matrix(unlist(strsplit(ysout, ":")), nrow=1,
             dimnames=list("", c("Year", "Season")))

    new("vdate", input=object@input, output=object@output, wts=object@wts,
      ys=ysout, obs=obsout, Myso=object@Myso)
  }
)
