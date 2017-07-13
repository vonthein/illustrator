# Voreinstellungen
# par(mar=rep(0,4),oma=rep(0,4))
# library(shape)
#
# Icons
#source("icons.R")
#
# specialized plot functions
#
#

#
#' Title
#'
#' @param ni trees per stand
#' @param path gap between stands
#' @param nrow number of rows of trees, if 0, uniformly random
#' @param near factor to the distance between symbols
#' @param icon symbol as a matrix
#' @param sizeratio factor by which symbols in right stand are larger
#' @param colri extreme colors of right stand
#' @param colle extreme colors of left stand
#' @param iex symbol expansion factor
#' @param ip symbol height factor
#' @param seed for random number generator
#' @param ... input to function lines()
#'
#' @return plot of two groups of symbols in rows and columns or uniformly distributed
#' @export
#'
#' @examples
#' \dontrun{
#' Forest()
#' Forest(ni=28,nrow=5,near=5)
#' Forest(path=2,ni=50)
#' Forest(ni=28,nrow=5,near=5,icon=bush)
#' Forest(ni=15,nrow=5,near=-2,icon=wheat-3,path=11,
#'        colle=c("yellow","gold"),colri=c("lightgreen","yellow"),col="brown")
#' Forest(ni=12,nrow=4,near=-2,icon=0.8*bike,path=11,
#'        colle=c("white","white"),colri=c("white","white"),col="blue",lwd=5)
#' Forest(ni=15,nrow=5,near=-2,icon=arabidopsis/2.2,path=11,
#'        colle=c("lightgreen","green"),colri=c("green","darkgreen"),col="darkgreen")
#' }
Forest <-function(ni = 30,        # trees per stand
 path = 5, nrow = 0, near = 3,    # closeness in row
 icon = fir, sizeratio = 1.4,     # right icons larger
 colri = c("green","lightgreen"), # colorpalette right
 colle = c("darkgreen","green"),  # colorpalette left
 iex  = 1, ip = 1, # icon size and proportion
 seed  = 12345,
 ...) {
 set.seed(seed)
 icon <- iex * cbind(icon[,1], ip * icon[,2])
 par(c(0, 0, 0, 0) + 0.1)
 if(nrow>0) {
  nij<- (ni %/% nrow) + 1 # firs per row
  sb <- path + 14*nrow # width of stands
  plot(c(-sb,sb),c(-near,(10 - near+1)*nij+2),pch="", xlab="", ylab="",
       xaxt  = "n", yaxt  = "n", bty = "n")
  # coordinates of twig tips
  x1 <- path + rep(14*(0:(nrow - 1)),nij)[1:ni] + runif(ni,-1,1)
  y1 <- 0+rep((10 - near)*((nij - 1):0),each = nrow, len = ni) + runif(ni,-1,1)
  # second stand
  x2 <- path + rep(14*(0:(nrow - 1)),nij)[1:ni] + runif(ni,-1,1)
  y2 <- 0+rep((10 - near)*((nij - 1):0),each = nrow, len = ni) + runif(ni,-1,1)
 } else {
  plot(c(-80,80),c(-30,45),pch="", xlab="", ylab="",
       xaxt  = "n", yaxt  = "n", bty = "n")
  x1 <- runif(ni,path,60)   # coordinates of twig tips
  y1 <- runif(ni,-30,30)
  x1 <- x1[order(y1,decreasing=T)]
  y1 <- sort(y1,decreasing=TRUE)
  x2 <- runif(ni,path,60)    # for two stands
  y2 <- runif(ni,-30,30)
  x2 <- x2[order(y2,decreasing=T)]
  y2 <- sort(y2,decreasing=TRUE)
 }
 stands <- cbind(c(as.vector(outer(sizeratio*icon[,1], x1,"+")),
                         as.vector(outer(-1 *icon[,1],-x2,"+"))),
                 c(as.vector(outer(sizeratio*icon[,2], y1,"+")),
                         as.vector(outer(    icon[,2], y2,"+"))))
 lines(stands,...)
 polygon(stands[(1:length(stands)/4),],
         col=shape::shadepalette(ni,colri[1],colri[2]))
 polygon(stands[-(1:(length(stands)/4)),],
         col=shape::shadepalette(ni,colle[1],colle[2]))
 par(c(5, 4, 4, 2) + 0.1)
}


