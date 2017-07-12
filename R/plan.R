# Voreinstellungen
# par(mar=rep(0,4),oma=rep(0,4))
# library(shape)
#
# Icons
#source("icons.R")
#
# specialized plot function
#

# Design of experiments
#

# rxc-plan
plan <- function(nij = 5, # group size
                 r  = 2, # rows
                 c  = 2, # cols
                 re = 1, # row effects
                 ce = 1, # col effekts
                 ie = 1, # interaction
                 icon = rat,
                 colo = c("darkgrey","white"),
                 seed = 1,
                 ...) {
  m <- length(icon)/2
  N <- r*c*nij;
  xi <- 1.5*rep(c(1,3,2),len = nij)
  yi <- 1.5*rep(c(1,1,2,3,3,4,5,5,6,7,7,8,9,9), len = nij)
  x <- as.vector(outer(xi,rep(10*(1:c),r),"+"))
  y <- as.vector(outer(yi,rep(2*nij*(1:r),each=c),"+"))
  z <- rep(8*(as.vector(outer(re*(1:r),ce*(1:c),"+"))+ie*(1:(r*c)))/
             (r*re+c*ce+r*c*ie),
           each=nij)
  if(!(colo[1]=="n"|colo[1]=="no"|colo[1]=="none")) {
    color <- 10*round(z+1+rnorm(N),1)
    colors <- shape::shadepalette(4*N,colo[1],colo[2])
    colors <- colors[color]
    co = TRUE} else {co <- FALSE}
  size <- rnorm(N,1.7,0.1)-0.1*(z+1)
  icons <- matrix(NA,1,2)
  for(i in 1:N){
    icons <- rbind(icons,cbind(size[i]*icon[,1]+25*x[i],
                               size[i]*icon[,2]+25*y[i]))
  }
  plot(range(icons[,1],na.rm=T),
       range(icons[,2],na.rm=T),pch="")
  if(co) {for(i in N:1){polygon(icons[((i-1)*m):(i*m),],col=colors[i])}}
  lines(icons, ...)
}
