# Voreinstellungen
# par(mar=rep(0,4),oma=rep(0,4))
#library(shape)
#
# Icons
#source("icons.R")
#
# specialized plot functions
#
#
gradient <- function(age = c(2,1.4,1,0.7,0.5), # fish size
 ni  = 5, # fish per age
 nij = 1, # fish density
 a   = 0, # regression coefficients
 b   = 1,
 icon = cod*8,
 colo  = c("blue","lightblue"),
 plotx  = c(-400-ni*200,400),
 ploty  = c(a-200,b*ni*200+200),
 seed = 12345,
 ...){
 set.seed(seed)
 m <- length(age)
 if(colo[1]=="n"|colo[1]=="no"|colo[1]=="none") co <- FALSE
 else {co <- TRUE; color  = shape::shadepalette(m,colo[1],colo[2])}
 plot(plotx,ploty,pch="")
 for (i in 1:m){ for (j in 1:nij) {
  x <- 0-  200*(1:ni)+rnorm(ni,0,200) # coordinates of nose tips
  y <- a+b*200*(1:ni)+rnorm(ni,0,100) # along a line
  shoal <- cbind(as.vector(outer(age[i]*icon[,1],x,"+")),
                         as.vector(outer(age[i]*icon[,2],y,"+")))
  lines(shoal,...)
  if(co) polygon(shoal ,col=colo[i])
 } }
}
#gradient()
#gradient(c(1.41,1.3,1.21,1.13,1.06,1),ni=6,nij=3,a=-100,b=1.6,
#         icon=8*cod%*%diag(c(-1,1)), # opposite direction
#         plotx=c(-2000,300),ploty=c(-100,2100))
#gradient(icon=-8*cod,nij=9) # fish kill
#gradient(icon=5*worm,colo=c("white","lightgrey"))
#gradient(icon=9*zebrafish,colo="no",col="darkgrey")


# Gradient
Gradient <- function(age = c(2,1.4,1,0.7,0.5), # fir sizes
 ni = 9, # firs per age
 near = 1,# closeness of age-groups
 a = 0, b = 1, # Regressionskoeffizienten
 icon = fir,
 colo = c("darkgreen","green"),
 seed = 12345,
 ...){
 set.seed(seed)
 m <- length(age)
 plot(c(20*near,20*(m*near+3)),c(20*near,20*(m*near+3)),pch="")
 x <- vector(); y <- vector(); year <- vector()
 for (i in m:1){
  year <- rbind(year,rep(i,ni))
  x <- rbind(x,20*i*near+runif(ni,0,50)) # coordinates of twig tips
  y <- rbind(y,a+b*20*i*near+runif(ni,0,50)) # along (a+bx)-line
 }
 oy   <- order(y,decreasing=TRUE)
 x    <-    x[oy]
 year <- year [oy]
 y    <- sort(y,decreasing=TRUE)
 apply(cbind(x,y,year),1,function(z){
  year <- z[3]
  wood <- cbind(icon[,1]*age[year]+z[1],
                icon[,2]*age[year]+z[2])
  if(!(colo[1]=="n"|colo[1]=="no"|colo[1]=="none"))
   polygon(wood,col=shape::shadepalette(m,colo[1],colo[2])[year])
  lines(wood,...)
 })
}
