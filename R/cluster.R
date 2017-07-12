# Voreinstellungen
# par(mar=rep(0,4),oma=rep(0,4))
# library(shape)
#
# Icons
#source("icons.R")
#
# specialized plot function
#

#' Cluster
#' @param nij symbols per size
#'
#' @param age vector of sizes, default c(2,1.4,1,0.7,0.5)
#' @param delta standardized mean difference
#' @param colo vector of extreme colors in both clusters, default c("blue","lightblue","darkgreen","green")
#' @param icon symbol as a matrix
#' @param seed for random number generator
#' @param ...
#'
#' @import base
#' @import stats
#' @examples
#' \dontrun{
#' Cluster(delta=1.5)
#' Cluster(7,c(1.41,1.3,1.21,1.13,1.06,1,0.95,0.91),1)
#' Cluster(icon=5*fir)
#' Cluster(icon=3*worm,colo=c("lightgrey","white","lightgray","cyan"))
#' Cluster(icon=5*flower,colo=c("blue","lightblue","red","pink"))
#' Cluster(7,c(1.41,1.3,1.21,1.13,1.06,1,0.95,0.91),2, icon=5*man,colo=c("blue","lightblue","red","white"))
#' Cluster(icon=5*bike,colo="no",col="blue",lwd=3)
#' }
Cluster <- function(nij = 4,    # fish per age
 age   = c(2,1.4,1,0.7,0.5),    # fish sizes
 delta = 2,                     # standardized mean difference
 colo  = c("blue","lightblue","darkgreen","green"),
 icon  = 8*cod,
 seed  = 12345, ...) {
 set.seed(seed)
# lim <- 150*delta/2+400
# plot(c(-lim,lim),c(-360,360),pch="")
  m <- length(age)
 li <- length(icon[,1])
 if(colo[1]=="n"|colo[1]=="no"|colo[1]=="none") {
       co <- FALSE; color <- rep("",2*m*nij)
      }
 else {co <- TRUE
       color <- cbind(shape::shadepalette(m,colo[1],colo[2]),
                      shape::shadepalette(m,colo[3],colo[4]))
       color <- as.vector(apply(color,1,rep,nij))
      }
  sig <- (-1)^(0:(2*m*nij-1))         # alternate signs
   xx <- rnorm(2*m*nij,150*delta/2,150) # coordinates of icons
   xx <- xx * sig                     # seperate clusters
   yy <- rnorm(2*m*nij,0,150)         # two clusters of m age groups each
   yy <- sort(yy,decreasing=TRUE)     # sorted for pseudo-perspective
  age <- rep(age[m:1],each=2*nij)     # back to front
 plot(range(xx)+kronecker(c(-1,1),max(icon[,1],na.rm=TRUE)*max(age)),
      range(yy)+kronecker(c(-1,1),max(icon[,2],na.rm=TRUE)*max(age)),pch="")
  how <- mapply(list,sig,xx,yy,age,color)
 paint <- function(howi,icon=icon,co=co){
   xij <- icon[,1]*howi[[4]]*howi[[1]]+howi[[2]]
   yij <- icon[,2]*howi[[4]]          +howi[[3]]
   lines(xij,yij,...)
   if(co) polygon(xij,yij,col=howi[[5]])
   }
 apply(how,2,paint,icon=icon,co=co)
 return(NULL)
}


