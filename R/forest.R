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
Forest <-function(ni = 30,        # trees per stand
 path = 5, nrow = 0, near = 3,    # closeness in row
 icon = fir, sizeratio = 1.4,     # right icons larger
 colri = c("green","lightgreen"), # colorpalette right
 colle = c("darkgreen","green"),  # colorpalette left
 ...){
 if(nrow>0) {
  nij<- (ni %/% nrow) + 1 # firs per row
  sb <- path + 14*nrow # width of stands
  plot(c(-sb,sb),c(-near,(10 - near+1)*nij+2),pch="")
  # coordinates of twig tips
  x1 <- path + rep(14*(0:(nrow - 1)),nij)[1:ni] + runif(ni,-1,1)
  y1 <- 0+rep((10 - near)*((nij - 1):0),each = nrow, len = ni) + runif(ni,-1,1)
  # second stand
  x2 <- path + rep(14*(0:(nrow - 1)),nij)[1:ni] + runif(ni,-1,1)
  y2 <- 0+rep((10 - near)*((nij - 1):0),each = nrow, len = ni) + runif(ni,-1,1)
 } else {
  plot(c(-80,80),c(-30,45),pch="")
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
 polygon(stands[(1:length(stands)/4),],
         col=shape::shadepalette(ni,colri[1],colri[2]))
 polygon(stands[-(1:(length(stands)/4)),],
         col=shape::shadepalette(ni,colle[1],colle[2]))
 lines(stands,...)
}


