# Voreinstellungen
# par(mar=rep(0,4),oma=rep(0,4))
# library(shape)
#
# Icons
#source("icons.R")
#
# specialized plot functions
#

# GLM fuer Lokalisation, Farbe, Groesse, Rotation
# Datenerzeugung
rglm <- function(n = 36, # number of symbols to be drawn
# formula = "~x1*x2",     # string describing the linear predictor
 link    = "id",         # link function "id", "sqrt", "log", "logit", "cloglog"
 dist    = "normal",     # one of "normal", "poisson", "binomial"
# random  = "pat",        # random effects variable name
 beta    = c(1,1,1,.1,.1,.1), # regression coefficients
 xes=list(x1=c(10,170),x2=3,x3=c(5,25),x4=2), # description of all regressors
 s       = 1,            # sd if "normal", size if "binomial"
 seed = NULL){
  set.seed(seed)
 rx <- function(x) {
  if(length(x) == 1) return(rmultinom(n,x-1,rep(1/x,x))[1,])
  if(length(x) == 2) return(runif(n,x[1],x[2]))
 }
 X <- sapply(xes,rx)
 # symbols need to be 5 to 15 units apart
 eta <- cbind(1,X,X[,1]*X[,2]) %*% beta
 if(link=="id")   mu <- eta
 if(link=="sqrt") mu <- eta^2
 if(link=="log")  mu <- exp(eta)
 if(link=="logit")mu <- exp(eta)/(1+exp(eta))
 if(link=="cloglog") mu <- exp(-exp(eta))
 if(dist=="normal") y <- rnorm(n,mu,s)
 if(dist=="poisson") y <- rpois(n,mu)
 if(dist=="binomial") y <- rbinom(n,prob=mu,size=s)
 data <- data.frame(y=y,x=X)
 #names(data) <- varnames
 return(data)         # simulated data
}
#daten <- rglm()
# Plot
plotglm <- function(
 icon = fir,     # icon matrix of coordinates
  iex = NULL,      # iconsize scaling factor
 colo = c("darkgreen","green"), # extremes of shadepalette
  rot = 0,         # rotation in degrees for every icon
  str = 1,         # horizontal stretch factor applied to every icon
 data,...){        # dataframe of simulated date, e.g. by function rglm
 #attach(data)      # columns define ordinate, abscissa, color, size, rotation
 n <- length(data[,1])# in that order
 oy <- order(data[,1],decreasing = TRUE)
 data <- data[oy,] # for pseudo-3D impression
 y <- data[,1]
 x <- data[,2]
 if(!(colo[1]=="n"|colo[1]=="no"|colo[1]=="none")) {
   m <- length(unique(data[,3]))
   color <- shape::shadepalette(m,colo[1],colo[2])
   cr <- color[(data[,3])+1]
   co = TRUE} else {co <- FALSE; cr <- "white"}
 size <- data[,4]     #*iex
 # automatic rescaling of icon
 if(is.null(iex)){
  iex <- sqrt(n)/min(outer(x,y,function(x,y) sqrt(x^2+y^2)))
 }
 size <- size * iex
 rote <- data[,5]*rot
 #str[1:n] <- str
 rota <- function(a){matrix(c(cos(a),sin(a),-sin(a),cos(a)),2)}
 ricon <- icon %*% rota(rot) * max(size)
 # analyze plotsize needed and prepare plot
 xlim <- c(min(x)+min(str*ricon[,1],na.rm=T),
           (max(x)+1)+max(str*ricon[,1],na.rm=T))
 ylim <- c(min(y)+min(ricon[,2],na.rm=T),
           (max(y)+1)+max(ricon[,2],na.rm=T))
 plot(xlim, ylim, pch="",xlim=xlim,ylim=ylim,
      xlab=names(data)[2],ylab=names(data)[1])
 how <- mapply(list,y,x,size,rote,cr)
 paint <- function(howi,icon=icon,co=co,...){
   xij <- (icon %*% rota(howi[[4]]))[,1] * howi[[3]] * str + howi[[2]]
   yij <- (icon %*% rota(howi[[4]]))[,2] * howi[[3]] + howi[[1]]
   if(co) polygon(xij,yij,col=howi[[5]])
   lines(xij,yij,...)
 }
 apply(how,2,paint,icon=icon,co=co,...)
 return(NULL)
}
