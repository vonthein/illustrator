# library(shape)
# library(splines)
# library(survival)
# library(mclust)
IMBSgreen     <- rgb(153,255,0,240,,255)
IMBSgreen.ms  <- rgb(102,255,0,240,,255)
IMBSgreen.    <- rgb(102,245,10,240,,255)
IMBSorange    <- rgb(245,130,5,240,,255) # darkorange = rgb(255,140,0)
IMBSorange.ms <- rgb(255,102,26,240,,255)
blaugruen     <- rgb(0,115,115,240,,255)
#imbs <- rgb(c(153,255),c(255,140),0,240,c("green","orange"),255)
#' Battleship plot
#'
#' @param y vector containing dependent variable
#' @param cens vector of length(y), is > 0 where y is right-censored, is < 0 where left-censored, 0 elsewhere implemented for dist "lognormal", "normal", "gamma" or "weibull"
#' @param x factor, same length as y
#' @param dis distribution, "lognormal", "normal", "2normal", "t", "beta","gamma", "weibull","mixnorm", "poisson", "binomial", "betabinomial"
#' @param xprop x-axis proportional to group sizes (joint distribution)? recommended, if dot=TRUE
#' @param space multiplicative spacing in x-direction
#' @param his plot histogram or probability function?
#' @param binz numbers of bins for dotplots and histograms
#' @param err draw error bar? Not drawn when data are censored
#' @param loc location of error bar or error bars, if vector: "mean", "geomean"
#' @param prec whiskers of error bar, "se", "sd", "2se", "2sd", applied to logarithms, if loc="geomean".
#' @param connect whether arithmetic or geometric means according to loc are connected, as scalar logical value or matrix of integers <= number of x levels with one column per polygon
#' @param box draw boxplots?
#' @param vio draw something like a violin plot?
#' @param dot plot dotplot? logical vector of length 1 or number of groups
#' @param icon either a character name of predefined icons or a matrix with coordinates, when character vector or list of matrices of length = levels of x, then icon by x, when list of length = levels of x of character vectors of number of ys per level, or list of lists of matrices then single icons sorted first by x second by y
#' @param fill fill icons? If sum(abs(cens)) > 0, then censored values are empty, others filled
#' @param ship plot battleship? logical vector of length 1 or number of x levels
#' @param plank line width for plotting ships and quantiles, if vector then for each ship
#' @param dtr density trace residual?
#' @param fillcol color the icons are filled with, when scalar then all the same, when vector of length = levels of x, then color by x, when list of length = levels of x of vectors of number of icons per level, color of single icons sorted first by x second by y
#' @param elicol color the icons are bordered with, similar to fillcol
#' @param errcol color the error bars are drawn in, similar to boxcol
#' @param concol color the connecting lines are drawn in, similar to boxcol
#' @param hiscol histogram color, if vector then for each histogram
#' @param boxcol boxplot color, if vector then for each boxplot
#' @param viocol violin color, if vector then for each violin
#' @param shipcol ship color, if vector then for each ship
#' @param quant vector of quantile for which bars are drawn across
#' @param cline plot line(s) through the middle at x label?
#' @param clc color(s) of line(s) through the middle
#' @param whisker battleships are drawn to the whisker quantile, default = 0.5 / n_i
#' @param confluence "Tukey" outlined bars, "Tufte" thin bars, "Duerr" circles
#' @param trials size parameter of binomial distribution(s)
#' @param start list of starting values for mixture parameters
#' @param mix number of normals in a mixture
#' @param genmix whether mixtures differ in proportions only
#' @param df degrees of freedom of t-distribution(s)
#' @param kern kernel of density trace for dtr or violin
#' @param sdsk smoothness parameter of density trace for dtr or violin
#' @param ... parameters passed on to function plot()
#'
#' @return Prints tuning parameters binz and space and returns histograms of y for values of x. Plots the specified plot.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rep(c(1:4),c(100,100,100,100)*.1); y <- x+rnorm(40,x/2)
#'  battleship(y=y,x=x,dtr=FALSE,fill=TRUE,xprop=FALSE,
#'             ship=TRUE, dis="normal", plank=1, cline=TRUE,
#'             space=1.7, dot=TRUE, binz=13, icon=rabbit,
#'             fillcol=c("brown","gold","lightgray","white"))
#'
#' # Aesthetic Symbol
#' # requires binz according to format
#' x <- rep(c(1:6),c(50,50,50,50,50,50)*2);y <- rep((rnorm(100)),6)
#' battleship(y=y,cens=0,x=x,binz=40,dtr=c(F,F,F,F,F,T),fill=T,xprop=F,
#'            space=2.2,dis="normal",plank=0.5,
#'            cline=c(F,T,F,F,F,F),clc="white",
#'            box=c(F,T,F,F,F,F),
#'            boxcol="black",
#'            dot=c(T,T,T,F,T,F),icon="arabidopsis",# fast quadratisch
#'            err=c(T,F,F,F,F,T),loc="mean",prec="2se",
#'            errcol=c("black","red","orange","white","blue","brown"),
#'            his=c(F,F,F,T,F,F),hiscol="darkgreen",
#'            vio=c(F,F,T,F,F,F),#viocol="green",
#'            ship=c(F,F,F,T,T,T),connect=matrix(c(1,2,5,6),2,2,F))
#'
#' x <- rep(c(1:2),c(150,50)*.6); y <- x+rnorm(80,x/2)
#' battleship(y=y,x=x,dtr=FALSE,fill=TRUE,xprop=TRUE,
#'            ship=TRUE, dis="normal", plank=3, cline=TRUE, space=1.2,
#'            dot=TRUE, binz=c(11,23),
#'            icon=list(man%*%diag(c(5,1)), woman))
#'
#' battleship(y=y, x=x, dtr=FALSE, fill=TRUE, xprop=FALSE,
#'            ship=TRUE, dis="normal", plank=4, cline=TRUE, space=1.2,
#'            dot=FALSE)
#'
#' x <- rep(c(1:3),c(50,50,50)*3);y <- rep((rnorm(150)),3)
#' battleship(y=y,cens=0,x=x,binz=30,dtr=c(F,F,F),fill=T,xprop=F,
#'            #fillcol="lightblue",elicol="blue",
#'            space=3.3,dis="normal",plank=0.5,
#'            cline=c(T,F,F),clc="white",
#'            box=c(T,F,F),
#'            boxcol="black",
#'            dot=c(T,T,F),
#'            err=c(F,F,T),loc="mean",prec="2se",
#'            errcol=c("black","red","brown"),
#'            his=c(T,F,F),hiscol="darkgreen",
#'            vio=c(F,T,F),#viocol="green",
#'            ship=c(F,F,T))
#'
#' # censored
#' x <- rep(c(1),c(80)*1);y <- rep((rnorm(80)),1)
#' cens <- rbinom(80,1,0.05)+(y>quantile(y,0.7))
#' battleship(y=y,cens=cens,x=x,dot=TRUE,dtr=FALSE,space=2.25,
#'            dis="normal",plank=1)
#'
#' # Confluence
#' x <- rep(c(1,2,3),c(20,20,20)*1);y <- rep((rpois(20,5)),3)
#' battleship(y=y,x=x,dot=TRUE,dtr=FALSE,space=1.25,
#'            dis="poisson",confluence=c("Tufte","Tukey","Duerr"))
#'
#' # Graphem = violin plot or density trace residual
#' x <- rep(c(1:2),c(50,50)*3);y <- rep((rnorm(150)),2)
#' battleship(y=y,x=x,space=2,dis="normal",dot=F,quant=NA,
#'  ship=T,vio=c(T,F),dtr=c(F,T))
#'
#' # Graphem = error bar
#' y1 <- exp(rnorm(200,1,0.4));x1 <- rep(1,200)
#' battleship(y=y1,x=x1,space=2,dis="lognormal",dot=F,
#'  quant=NA,fill=T,ship=T,vio=T,dtr=T)
#'
#' # Graphem = discret probability function bar plot
#' y2 <- rpois(30,5);x2 <- rep(2,30)
#' battleship(y=y2,x=x2,space=2,dis="poisson",dots=T,
#'  elicol="gray",fill=T,ship=T,shipcol="blue")
#'
#' # Graphem = connecting line
#' x <- rep(c(1:5),10)
#' y <- rbinom(50,size=20,prob=c(0.05,0.2,0.5,0.8,0.95))
#' battleship(y=y,x=x,space=0.5,binz=30,dis="gamma",dot=T,
#'  elicol="gray",fill=T,ship=F,err=T,loc="mean",prec="2se",
#'  errcol="orange",connect=matrix(1:5,nrow=1),concol="black")
#' battleship(y=y,x=x,dis="binomial",trials=rep(20,5),dot=F,ship=F,
#'  err=F,loc="mean",prec="2se",errcol="orange",
#'  connect=matrix(1:5,nrow=1),concol="black")
#' battleship(y=y,cens=0,x=x,binz=89,dtr=F,fill=T,xprop=T,space=1.8,dot=T,ship=F)
#'
#' # Aestetic = distribution
#' x <- rep(c(1:2),c(50,50)*3);y <- rep((rgamma(150,3)),2)
#' battleship(y=y,x=x,space=1.5,binz=55,dis=c("normal","lognormal"),
#'   dot=T,fill=T,elicol=c("darkgreen","darkgreen"),fillcol=c("green","green"),
#'   quant=NA,ship=T,vio=c(F,F),dtr=T)
#' battleship(y=y,x=x,space=1.5,binz=55,dot=T,
#'   fill=T,elicol=c("gray","gray"),fillcol=c("lightgray","lightgray"),
#'   quant=NA,ship=F,vio=T,dtr=T,sdsk=c(.2,.5))
#'
#' # Aesthetic = bin numbers
#' battleship(y=y[1:150],x=x[1:150],space=2,binz=55,his=T,dot=F,
#'   fill=T,elicol=c("darkgreen","darkgreen"),fillcol=c("green","green"),
#'   quant=NA,ship=F,vio=c(F,F),dtr=T)
#' battleship(y=y[1:150],x=x[1:150],space=2,binz=12,his=T,dot=F,
#'   fill=T,elicol=c("darkgreen","darkgreen"),fillcol=c("green","green"),
#'   quant=NA,ship=F,vio=c(F,F),dtr=T)
#'
#' # Mix of three genotypes
#' # Proportions Mendel
#' maf <- 0.1
#' p <- maf^2
#' pq <- 2*maf*(1-maf)
#' q <- (1-maf)^2
#' n <- 3000*c(q,pq,p)
#' x <- rep((1:length(n)),n);y <- rnorm(sum(n),x)
#' battleship(y=y,cens=0,x=x,dtr=F,fill=T,xprop=T,dot=T,ship=T,space=0.6)
#' CasesControls <- rep(c(1,2),1500)
#' battleship(y=y,cens=0,x=CasesControls,dis="mixnorm",genmix=T,mix=3,
#'            dtr=F,fill=T,xprop=T,dot=T,ship=T,space=1.5)
#' }
battleship <- function(y, cens=0, x=1, dis="normal", xprop=T, space=1, his=NA, binz=0,
                       err=F, loc="none", prec="2sd", connect=F,
                       box=F, vio=F, dot=NA, icon="loop", fill=FALSE, ship=T, plank=2, dtr=F,
                       fillcol=shape::shadepalette(length(unique(x)),"lightblue",IMBSgreen.),
                        elicol=shape::shadepalette(length(unique(x)),"blue","darkgreen"),
                        errcol=shape::shadepalette(length(unique(x)),IMBSorange,"brown"),
                        concol=shape::shadepalette(length(as.matrix(connect)[1,]),IMBSorange,"brown"),
                        hiscol=shape::shadepalette(length(unique(x)),"darkgreen","darkblue"),
                        boxcol=shape::shadepalette(length(unique(x)),"brown",IMBSorange),
                        viocol=shape::shadepalette(length(unique(x)),IMBSorange,"brown"),
                       shipcol=shape::shadepalette(length(unique(x)),IMBSorange,"brown"),
                       quant=c(0.025,0.25,0.5,0.75,0.975),
                       cline=FALSE, clc = "white", whisker=0, confluence="Tufte",
                       trials=0, start=list(m), mix=NA, genmix=F, df=4, kern="gaussian",
                       sdsk=sqrt(var(y))/log(length(y)),...){
 # y       vector containing dependent variable
 # cens    vector of length(y), is > 0 where y is right-censored,
 #          is < 0 where left-censored, 0 elsewhere
 #	     implemented for dist "lognormal", "normal", "gamma" or "weibull"
 # x       factor, same length as y
 # dis     distribution, "lognormal", "normal", "2normal", "t",
 #         "beta","gamma", "weibull","mixnorm",
 #         "poisson", "binomial", "betabinomial"
 # xprop   x-axis proportional to group sizes (joint distribution)?
 #         recommended, if dot=TRUE
 # err     draw error bar? Not drawn when data are censored
 # loc     location of error bar or error bars, if vector: "mean", "geomean"
 # prec    whiskers of error bar, "se", "sd", "2se", "2sd",
 #          applied to logarithms, if loc="geomean".
 # connect whether arithmetic or geometric means according to loc are
 #          connected, as scalar logical value or matrix of
 #         integers <= number of x levels with one column per polygon
 # box     draw boxplots?
 # vio     draw something like a violin plot?
 # dot     plot dotplot? logical vector of length 1 or number of groups
 # icon    either a character name of predefined icons or a matrix with coordinates,
 #         when character vector or list of matrices of length = levels of x, then icon by x,
 #         when list of length = levels of x of character vectors of number of ys per level,
 #         or list of lists of matrices then single icons sorted first by x second by y
 # fill    fill icons?
 #         If sum(abs(cens)) > 0, then censored values are empty, others filled
 # fillcol color the icons are filled with, when scalar then all the same,
 #         when vector of length = levels of x, then color by x,
 #         when list of length = levels of x of vectors of number of
 #         icons per level, color of single icons sorted first by x second by y
 # elicol  color the icons are bordered with, similar to fillcol
 # boxcol  boxplot color, if vector then for each boxplot
 # errcol  color the error bars are drawn in, similar to boxcol
 # concol  color the connecting lines are drawn in, similar to boxcol
 # hiscol  histogram color, if vector then for each histogram
 # viocol  violin color, if vector then for each violin
 # shipcol ship color, if vector then for each ship
 # his     plot histogram or probability function?
 # ship    plot battleship? logical vector of length 1 or number of x levels
 # plank   line width for plotting ships and quantiles, if vector then for each ship
 # dtr     density trace residual?
 # binz    numbers of bins for dotplots and histograms
 # space   multiplicative spacing in x-direction
 # quant   vector of quantile for which bars are drawn across
 # cline   plot line(s) through the middle at x label?
 # clc     color(s) of line(s) through the middle
 # whisker battleships are drawn to the whisker quantile,
 #         default = 0.5 / n_i
 # confluence:
 #         "Tukey" outlined bars
 #         "Tufte" thin bars
 #         "Duerr" circles
 # trials  size parameter of binomial distribution(s)
 # start   list of starting values for mixture parameters
 # mix     number of normals in a mixture
 # genmix  whether mixtures differ in proportions only
 # df      degrees of freedom of t-distribution(s)
 # kern    kernel of density trace for dtr or violin
 # sdsk    smoothness parameter of density trace for dtr or violin

 n <- length(y)                   # total sample size
 xl <- unique(x)                  # group labels
 m <- length(xl)                  # number of levels of x or groups
 tab <- table(x)[xl]              # group sizes
 pp <- m*tab/n                    # group proportions times number of groups
 pps <- m*sqrt(tab/n)             # root group proportions times number of groups
 dat <- split(y,x)                # data as list of groups
 dat <- dat[xl]                   # in the intended order
 if (length(cens)==1) {cens <- rep(cens,n)}
 event <- 1-cens 		  # censoring variable
 event[cens==0] <- 1		  # recoded to event variable
 event[cens>0] <- 0 		  #
 event[cens<0] <- 2 		  #
 eve <- split(event,x)            # events as list of groups
 eve <- eve[xl]                   # in the intended order
 ql <- length(quant)              # number of quantiles to be marked
 cs <- length(as.matrix(connect)[1,]) # no. of connecting lines
 cp <- length(as.matrix(connect)[,1]) # max. no. of connected mean
 xs <- cumsum(pps^xprop)-pps^xprop/2
 diskret <- vector()

 if(is.na(dot[1])){ if(binz>60){dot<-F;his<-T}else{dot<- T} }
 # default: draw dots, if they are few
 # extend short form graphem and aesthetics indicators
 if (length(binz)==1){binz<- rep(binz,m)}
 for (i in 1:m) {if (binz[i]==0){
   binz[i] <- 3*round(sqrt(max(unlist(lapply(dat,length)))))      # number of bins
 }}
 if (length(err)==1) {err <- rep(err,m)}
 if (length(loc)==1) {loc <- rep(loc,m)}
 if (length(prec)==1){prec<- rep(prec,m)}
 if (length(connect)==1 & connect[1]){connect <- matrix(1:m,ncol=1)}
 if (length(box) ==1) {box  <- rep(box,m)}
 if (length(dot) ==1) {dot  <- rep(dot,m)}
 if (length(icon)==1) {icon <- rep(icon,m)}
# if (length(fill)==1) {fill <- rep(fill,m)}
 if (length(his) ==1) {his  <- rep(his,m)}
 if (length(cline)==1){cline<- rep(cline,m)}
 if (length(vio) ==1) {vio  <- rep(vio,m)}
 if (length(sdsk)==1) {sdsk <- rep(sdsk,m)}
 if (length(ship)==1) {ship <- rep(ship,m)}
 if (length(dtr) ==1) {dtr  <- rep(dtr,m)}
 if (length(confluence)==1) {confluence <- rep(confluence,m)}
 if (length(trials)==1) {trials <- rep(trials,m)}
 if (length(mix) ==1) {mix <- rep(mix,m)}
 if (length(df)==1) {df <- rep(df,m)}
 # sort out distributions
 if (length(dis)==1) {dis <- rep(dis,m)}
 else {
  if (length(dis)!=m) {cat("only first",m,"distributions used\n")}
   dist <- dis[1:m]
  }
 for (i in 1:m) {
  diskret[i] <- ((dis[i]=="binomial")|(dis[i]=="poisson")|(dis[i]=="betabinomial"))
  if (diskret[i]) { ship[i] <- F; his[i] <- T }
  else { if (is.na(his[i])) { his[i] <- F } }
  if ((dis[i]=="binomial")|(dis[i]=="betabinomial")) {
    if (round(trials[i])!=trials[i]) {
     trials[i] <- round(trials[i])
     cat("number of trials in group",i,"rounded to",trials[i],"\n")
    }
    if (trials[i]<max(dat[[i]])) {
     trials[i] <- max(dat[[i]])
     cat("number of trials in group",i,"set to minimum: ",trials[i],"i.e. maximum\n")
    }
   }
   if (dis[i]=="t") {
    if (round(df[i])!=df[i]) {
     df[i] <- round(df[i])
     cat("number of degrees of freedom in group",i,"rounded to",df,"\n")
    }
    if (df[i]<1) {
     df[i] <- 1
     cat("number of degrees of freedom in group",i,"set to minimum: ",df[i],".\n")
    }
   }
  }
 # sort out icons
 icons <- list(); for(i in 1:m) {icons[[i]] <- list()}
 scale.icon <- function(ico) {
   if(dim(ico)[2]>2) {
     cat("\nOnly first two columns of icon used.\n")
     ico <- ico[,1:2]
   }
   ran <- apply(ico,2,range,na.rm=TRUE); scales <- ran[2,]-ran[1,]
   return((ico - 0.5 * matrix(scales,dim(ico)[1],2,TRUE)) / mean(scales))
 }
 if(is.matrix(icon)) { # cat("is matrix")
   scaled.icon <- scale.icon(icon)
   for(i in 1:m){for(j in 1:tab[i]) {icons[[i]][[j]]<-scaled.icon}}
 }
 if(is.list(icon)) {
   if(is.character(icon[[1]][1])) { # cat("list of character\n")
     if(is.list(icon[[1]])){ # cat("list of list of character, every single symbol")
       for(i in 1:m) {for(j in 1:tab[i]) {icons[[i]][[j]] <- scale.icon(eval(parse(text=icon[[i]][[j]])))}}
     }
     else { # cat("list of vectors of character")
       if (length(icon[[i]])==1) { # cat("\nlist of single character")
        scaled.icon <- scale.icon(ico=eval(pars(text=icon[[i]])))
        for(i in 1:m) {for(j in 1:tab[i]) {icons[[i]][[j]] <- scaled.icon}}
       }
       else { # cat("list of vectors of character")
         for(i in 1:m) {for(j in 1:tab[i]) {icons[[i]][[j]] <- scale.icon(eval(parse(text=icon[[i]][j])))}}
       }
      }
   }
   else { # cat("not character\n")
    if (is.list(icon[[1]])) { # cat("list of list of matrices")
        for(i in 1:m) {for(j in 1:tab[i]) {icons[[i]][[j]] <- scale.icon(icon[[i]][[j]])}}
    }
     else { # cat("list of matrices")
       for(i in 1:m) {
         scaled.icon <- scale.icon(icon[[i]])
         for(j in 1:tab[i]) {icons[[i]][[j]] <- scaled.icon}
       }
     }
   }
 }
 else {
   if(is.vector(icon)) { # cat("vector must be character")
     mm <- length(icon)
     if(mm<m){
       icon <- rep(icon[1],m)
       cat("\nToo few icons. Only first icon used.\n")
     }
     if(mm>m){
       icon <- icon[1:m]
       cat("\nToo many icons. Only first",m,"icons used.\n")
     }
     for(i in 1:m) {
       scaled.icon <- scale.icon(eval(parse(text=icon[i])))
       for(j in 1:tab[i]) {icons[[i]][[j]] <- scaled.icon}}
   }
 }
   pz <- 50                              # number of polygon nodes

 # quantiles of mixture of two normal distributions
 q2norm <- function(quantil,para,q){
        (q - para$mp *pnorm(quantil,para$my1,para$s1)-
          (1-para$mp)*pnorm(quantil,para$my2,para$s2) )^2
 }
 # quantiles of mixture of normal distributions
 qmixnorm <- function(quantil,para,q){
        (q - sum(para$pro*pnorm(quantil,para$mean,sqrt(para$variance$sigmasq))))^2
 }
 # density function of mixture of normal distributions
 dmixnorm <- function(q,para){
  mix <- length(para$pro); nq <- length(q)
  cd <- matrix(NA,nq,mix)
  for(j in 1:mix){
   cd[,j] <- para$pro[j]*dnorm(q,para$mean[j],sqrt(para$variance$sigmasq[j]))
  }
  return(apply(cd,1,sum))
 }
 # quantiles of beta-binomial distribution
 qbetabin <- function(q,para){
  a <- para$a; b <- para$b; n <- para$n
  x <- 0:n
  fx <- exp( lgamma(a+b)  -lgamma(a)  -lgamma(b)+
             lgamma(a+b+n)-lgamma(a+x)-lgamma(b+n-x)+
             lgamma(n+1)  -lgamma(x+1)-lgamma(n-x+1) )
  px <- cumsum(fx)
  max(c(0,x[px<=q]),na.rm=T)
 }
 # density function of beta-binomial distribution
 dbetabin <- function(x,para){
  a <- para$a; b <- para$b; n <- para$n
  exp( lgamma(a+b)  -lgamma(a)  -lgamma(b)+
       lgamma(a+b+n)-lgamma(a+x)-lgamma(b+n-x)+
       lgamma(n+1)  -lgamma(x+1)-lgamma(n-x+1) )
 }
 # probability function of beta-binomial distribution
 pbetabin <- function(x,para){
  a <- para$a; b <- para$b; n <- para$n
  x <- 0:x
  sum(exp( lgamma(a+b)  -lgamma(a)  -lgamma(b)+
           lgamma(a+b+n)-lgamma(a+x)-lgamma(b+n-x)+
           lgamma(n+1)  -lgamma(x+1)-lgamma(n-x+1) ) )
 }

 # estimate parameters
 epa <- function(y,event,dist,trials,df,start,mix){
  ni <- length(y)
  if (dist=="poisson"){
   lambda <- mean(y)
   para <- list(lambda=lambda,amean=lambda,gmean=exp(mean(log(y))))
  }
  if (dist=="binomial"){
   size <- trials
   prob <- mean(y)/size
   cat("n=",size," p=",prob,"\n")
   para <- list(size=size,prob=prob,amean=trials*prob,
                gmean=exp(mean(log(y)/size)))
  }
  if (dist=="lognormal"){
   if(sum(event==1)<length(event)) { # in case of censoring
    censoring <- ifelse(sum(event==0)==0,"left","interval")
    if(sum(event==2)==0) {censoring <- "right"}
    est <- survreg(Surv(y,event, type=censoring)~1, dist=dist)
    para <- list(my=est$coef, s=est$scale,
                 amean=exp(est$coef-(ni-1)*est$scale^2/(2*ni)),
                 gmean=exp(est$coef-est$scale^2/(2*ni)))
   }
   else { # without censoring
    yy <- log(y)
    my <- mean(yy)
    s  <- sqrt(var(yy))
    para <- list(my=my,s=s,
                 amean=exp(my-(ni-1)*s^2/(2*ni)),
                 gmean=exp(my-s^2/(2*ni)))
   }
  }
  if (dist=="normal"){
   if(sum(event==1)<length(event)) { # in case of censoring
    censoring <- ifelse(sum(event==0)==0,"left","interval")
    if(sum(event==2)==0) {censoring <- "right"}
    est <- survival::survreg(survival::Surv(y,event, type=censoring)~1, dist="gaussian")
    para <- list(my=est$coef, s=est$scale,
                 amean=est$coef,
                 gmean=est$coef) # needs to be corrected !
   }
   else { # without censoring
    my <- mean(y)
    s  <- sqrt(var(y))
    para <- list(my=my,s=s,amean=my,
                 gmean=exp(mean(log(y)))) # needs to be corrected !
   }
  }
  if (dist=="2normal"){
   negloglik <- function(ab=c(1,1,1,1,0.5),y){
    mp <- exp(ab[5])/(1+exp(ab[5]))
    return(-sum(log(mp/exp(ab[2])^2*exp(-0.5*(y-ab[1])^2/exp(ab[2])^4)+
                (1-mp)/exp(ab[4])^2*exp(-0.5*(y-ab[3])^2/exp(ab[4])^4) )))
    }
   if(length(start)==0) {
    st <- c(mean(y)*1.05,log(var(y))/2,
            mean(y)*0.95,log(var(y))/2, 0.5)
   }
   else {
    st <- c(start[1],log(start[2]),
            start[3],log(start[4]), log(start[5]/(1-start[5])))
   }
   ab <- nlm(negloglik,st,y=y)$estimate
   if (ab[1]<min(y)|ab[3]<min(y)|ab[1]>max(y)|ab[3]>max(y)) {
    cat("cannot extimate mixture\n")
   }
   if (exp(ab[2])>max(y)|exp(ab[4])>max(y)) cat("can not extimate mixture\n")
   para <- list(my1=ab[1],s1=exp(2*ab[2]),
                my2=ab[3],s2=exp(2*ab[4]), mp=exp(ab[5])/(1+exp(ab[5])),
                amean=mean(y),gmean=exp(mean(log(y))))
   cat(unlist(para),"\n",start,"\n")
  }
  if (dist=="mixnorm"){
   if (!genmix){
    if(is.na(mix)){ para <- mclust::Mclust(y)$parameters }
    else {          para <- mclust::Mclust(y,mix)$parameters }
   if(length(para$variance$sigmasq)==1){
    para$variance$sigmasq <- rep(para$variance$sigmasq,length(para$pro))
    }
   } else { # if(genmix)
    if(is.na(mix)){ para <- mclust::Mclust(unlist(dat))$parameters }
    else {          para <- mclust::Mclust(unlist(dat),mix)$parameters }
    if(length(para$variance$sigmasq)==1){
     para$variance$sigmasq <- rep(para$variance$sigmasq,length(para$pro))
    }
    negloglik <- function(lor,y,para){
     pp <- exp(lor)/(1+exp(lor))
     para$pro <- c(pp,1-sum(pp))
     return(-sum(log(dmixnorm(y,para))))
    }
    lor <- nlm(negloglik,log(para$pro/(1-para$pro))[1:(mix-1)],y=y,para=para)$estimate
    pp <- exp(lor)/(1+exp(lor)); pp <- c(pp,1-sum(pp))
    print(pp)
    para$pro <- pp
   }
  }
  if (dist=="t"){
   my <- mean(y)
   s  <- sqrt(var(y))
   para <- list(my=my,s=s,df=df,amean=my,gmean=exp(mean(log(y))))
  }
  if (dist=="beta"){
   #   negloglik <- function(ab=c(1,1),y){
   #    sum(lbeta(ab[1],ab[2])-(ab[1]-1)*log(y)-(ab[2]-1)*log(1-y))
   #    }
   #   ab <- nlm(negloglik,c(1*mean(y),1-1*mean(y)),y=y)$estimate
   # mme instead of mle
   ybar <- mean(y);v <- var(y)
   size <- ybar*(1-ybar)/v - 1
   para <- list(a=size*ybar,b=size*(1-ybar),amean=mean(y),
                gmean=exp(mean(log(y)))) # needs to be corrected !
  print(para)
  }
#  if (dist=="beta2"){   # mme
#   m1 <- mean(y);
#   m3 <- mean((y-m1)^2);
#   m3 <- mean((y-m1)^3);
#   m3 <- mean((y-m1)^4);
#   size <- ybar*(1-ybar)/v - 1
#   para <- list(a=size*ybar,b=size*(1-ybar))
#  }
  if (dist=="gamma"){
   negloglik <- function(ab=c(1,1),y){
    sum(lgamma(ab[1])+ab[1]*log(ab[2])-(ab[1]-1)*log(y)+y/ab[2])
    }
   ab <- nlm(negloglik,c(1,mean(y)),y=y)$estimate
   para <- list(a=ab[1],b=ab[2],amean=ab[1]*ab[2], # needs to be corrected ?
                gmean=exp(mean(log(y)))) # needs to be corrected !
  }
  if (dist=="betabinomial"){
   negloglik <- function(ba=c(1,1),y,no){
    a <- exp(ba[1]); b <- exp(ba[2])
    nll<- sum(lgamma(a+b)   -lgamma(a)  -lgamma(b)+
              lgamma(a+b+no)-lgamma(a+y)-lgamma(b+no-y)+
              lgamma(no+1)  -lgamma(y+1)-lgamma(no-y+1)  )
    cat(nll,a,b,"\n")
    return(nll)
    }
   empod <- mean(y)/2 # mean(y)/(trials-mean(y))#0.5
   ab <- nlm(negloglik,log(c(empod,(trials/2-empod))),y=y,no=trials)$estimate
   para <- list(n=trials,a=exp(ab[1]),b=exp(ab[2]),
                amean=trials*exp(ab[1])/(exp(ab[1])+exp(ab[2])),
                gmean=exp(mean(log(y)))) # needs to be corrected !
  }
  if (dist=="weibull"){
    censoring <- ifelse(sum(event==0)==0,"left","interval")
    if(sum(event==2)==0) {censoring <- "right"}
    est <- survival::survreg(survival::Surv(y,event, type=censoring)~1, dist=dist)
    para <- list(a=exp(est$coef), c=1/est$scale, y0=0,
                 amean=est$scale*est$coef,  # needs to be corrected ?
                 gmean=exp(mean(log(y)))) # needs to be corrected !
  }
  return(para)
 }
 paras <- list()
 for (i in 1:m) {
  paras[[i]] <- epa(dat[[i]],eve[[i]],dis[i],trials[i],df[i],start[[i]],mix[i])
  if(is.na(mix[i])&dis[i]=="mixnorm"){mix[i] <- length(paras[[i]]$mean)}
 }

 # whiskers quantiles and maxima of densities
 rost <- function(y,dist,para,whisk){
  if (dist=="poisson"){
   qq <- c(qpois(whisk,lambda=para$lambda), qpois(1-whisk,lambda=para$lambda))
   ww <- dpois(round(para$lambda),lambda=para$lambda)
  }
  if (dist=="binomial"){
   qq <- c(qbinom(whisk,size=para$size,prob=para$prob),
         qbinom(1-whisk,size=para$size,prob=para$prob))
   ww <- dbinom(round(para$size*para$prob),size=para$size,prob=para$prob)
  }
  if (dist=="lognormal"){
   qq <- c(qlnorm(whisk,para$my,para$s), qlnorm(1-whisk,para$my,para$s))
   ww <- dlnorm(exp(para$my-para$s^2),para$my,para$s)
  }
  if (dist=="normal"){
   qq <- c(qnorm(whisk,mean=para$my,sd=para$s),
         qnorm(1-whisk,mean=para$my,sd=para$s))
   ww <- dnorm(para$my,mean=para$my,sd=para$s)
  }
  if (dist=="2normal"){
   qq <- c(nlm(q2norm,quantile(y,whisk),para=para,q=whisk)$estimate,
           nlm(q2norm,quantile(y,1-whisk),para=para,q=1-whisk)$estimate)
   carrier <- qq[1]+(1:99)*(qq[2]-qq[1])/100
   ww <- max(para$mp *dnorm(carrier,para$my1,para$s1)+
          (1-para$mp)*dnorm(carrier,para$my2,para$s2)  )
  }
  if (dist=="mixnorm"){
   qq <- c(nlm(qmixnorm,quantile(y,whisk),para=para,q=whisk)$estimate,
           nlm(qmixnorm,quantile(y,1-whisk),para=para,q=1-whisk)$estimate)
   carrier <- qq[1]+(1:99)*(qq[2]-qq[1])/100
   ww <- max(dmixnorm(carrier,para))
  }
  if (dist=="t"){
   qq <- c(qt(whisk,df=para$df)*para$s+para$my,
           qt(1-whisk,df=para$df)*para$s+para$my)
   ww <- dt(0,df=para$df,ncp=0)/para$s
  }
  if (dist=="beta"){
   qq <- c(qbeta(whisk,para$a,para$b), qbeta(1-whisk,para$a,para$b))
   ww <- dbeta((para$a-1)/(para$a+para$b-2),para$a,para$b)
  }
  if (dist=="betabinomial"){
   qq <- c(qbetabin(q=whisk,para=para),qbetabin(q=1-whisk,para=para))
   ww <- max(dbetabin(0:para$n,para))
  }
  if (dist=="gamma"){
   qq <- c(qgamma(whisk,para$a,,para$b), qgamma(1-whisk,para$a,,para$b))
   ww <- dgamma((para$a-1)/para$b,para$a,,para$b)
  }
  if (dist=="weibull"){
   qq <- c(qweibull(whisk,para$c,para$a),
         qweibull(1-whisk,para$c,para$a))+para$y0
   ww <- dweibull( para$a*((para$c-1)/para$c)^(1/para$c),para$c,para$a)
  }
  return(list(range(qq,y,na.rm=T),ww))
 }

 # densities for ships and quantile marks
 wurst <- function(y,ry,dist,para,fein=F,mix){
  if(fein){br <- ry[1]+(ry[2]-ry[1])*((0:(10*pz+1))-0.5)/(10*pz)}
  else    {br <- ry[1]+(ry[2]-ry[1])*((0:(pz+1))-0.5)/(pz)}
  if (dist=="poisson"){
   dx <- dpois(br,lambda=para$lambda)
   qw <- c(qpois(quant,lambda=para$lambda))
   wp <- dpois(qw,lambda=para$lambda)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="binomial"){
   dx <- dbinom(0:para$size,size=para$size,prob=para$prob)
   qw <- c(qbinom(quant,size=para$size,prob=para$prob))
   wp <- dbinom(qw,size=para$size,prob=para$prob)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="lognormal"){
   dx <- dlnorm(br,para$my,para$s)
   qw <- qlnorm(quant,para$my,para$s)
   wp <- dlnorm(qw,para$my,para$s)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="normal"){
   dx <- dnorm(br,para$my,para$s)
   qw <- qnorm(quant,para$my,para$s)
   wp <- dnorm(qw,para$my,para$s)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="2normal"){
   dx <-    para$mp *dnorm(br,para$my1,para$s1)+
         (1-para$mp)*dnorm(br,para$my2,para$s2)
# berechne iterativ und nichtlinearoptimierend die Quantile
#   qw <- vector()
#   for (i in 1:length(quant)){
#    qw[i] <- nlm(q2norm,quantile(y,quant[i]),para=para,q=quant[i])$estimate
#   }
#   if (sum(is.na(qw))>0) {
#    qw <- quantile(y,quant)
#    cat("empirical quantiles used\n")
#   }
## zeige statt der Quantile den Trennpunkt an
##   ss <- paras$s2^2-paras$s1^2
##   qw <- (para$s2^2*para$my1-para$s1^2*para$my2)/ss
##   qw <- 0.5*(qw+sqrt(qw^2-
##              4*(para$my2^2*para$s1^2+para$m1^2*para$s2^2)/ss-
##              8*para$s2^2*para$s1^2/ss*
##               log(para$mp*para$s2/((1-para$mp)*para$s1)) ))
##   qw <- rep(qw,ql)
# use empirical quantiles
   qw <- quantile(y,quant,na.rm=T,names=F)
   wp <-    para$mp *dnorm(qw,para$my1,para$s1)+
         (1-para$mp)*dnorm(qw,para$my2,para$s2)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="mixnorm"){
   dx <- dmixnorm(br,para)
   qw <- vector()
   for (j in 1:length(quant)){
    qw[j] <- quantile(y,quant[j])
#nlm(qmixnorm,quantile(y,quant[j]),para=para,q=quant[j])$estimate
   }
   wp <- dmixnorm(qw,para)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="t"){
   dx <- dt((br-para$my)/para$s,df=para$df,ncp=0)/para$s
   qw <- qt(quant,df=para$df)
   wp <- dt(qw,df=para$df,ncp=0)/para$s
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="beta"){
   dx <- dbeta(br,para$a,para$b)
   qw <- qbeta(quant,para$a,para$b)
   wp <- dbeta(qw,para$a,para$b)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="betabinomial"){
   dx <- dbetabin(br,para)
   qw <- vector()
   for(j in 1:ql) {qw[j] <- qbetabin(q=quant[j],para=para)}
   wp <- dbetabin(qw,para)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="gamma"){
   dx <- dgamma(br,para$a,,para$b)
   qw <- qgamma(quant,para$a,,para$b)
   wp <- dgamma(qw,para$a,,para$b)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  if (dist=="weibull"){
   dx <- dweibull(br-para$y0,para$c,para$a)
   qw <- qweibull(quant,para$c,para$a)
   wp <- dweibull(qw,para$c,para$a)
   dx <- c(dx,as.vector(t(cbind(NA,wp,-wp))))
  }
  return(dx)
 }

 # extrem values of data or whiskers
 # and densities or histograms
 qq <- list() # y-axis
 ww <- list() # f(y)-axis
 ff <- list()
 for(i in 1:m) {
   if (whisker==0) {whiske <- 0.5/tab[i]} else {whiske <- whisker}
   qw <- rost(dat[[i]], dis[i], paras[[i]], whiske)
   qq[[i]] <- qw[[1]]
   ww[[i]] <- qw[[2]]
 }
 ry <- range(c(unlist(qq),y),na.rm=T)

 for (i in 1:m) {
  if (diskret[i]) {brk <- (ry[1]:(ry[2]+1))-0.5}
  else {brk <- ry[1]+(ry[2]-ry[1])*((0:(binz[i]))-0.5)/(binz[i]-1)}
  ff[[i]] <- hist(dat[[i]],breaks=brk,plot=F)
  ww[[i]] <- max(ww[[i]],ff[[i]]$density[ff[[i]]$counts!=0])
 }
 rd <- space*max(unlist(ww),na.rm=T)

# preparation of histograms
 hislin <- function(y,ry,x,p,para,rd,diskret,dist,confluence,binz){
  if (diskret) {
   bins <- ry[2]-ry[1]+1
   br <- ry[1]:ry[2]
   if (dist=="poisson") {l1 <- dpois(br,lambda=para$lambda)}
   if (dist=="binomial"){l1 <- dbinom(br,size=para$size,prob=para$prob)}
   if (dist=="betabinomial"){l1 <- dbetabin(br,para)}
   if (confluence=="Tufte") {
    lx <- x+(as.vector(t( cbind(l1,-l1,NA) ))/rd)*p^xprop
    ly <- as.vector(t( outer((br),c(1,1,NA),"*") ))
   }
   if (confluence=="Tukey") {
    lx <- as.vector(t( cbind(l1,-l1,-l1,l1,l1,NA) ))*p^xprop/rd+x
    ly <- as.vector(t( outer((br),c(1,1,-1,-1,1,NA)/4,"+") ))
   }
   if (confluence=="Duerr") {
    ellipx <- c(0.5*sin(pi*(0:36)/18),NA)
    ellipy <- c(cos(pi*(0:36)/18),NA)
    ly <- as.vector(t( outer(l1,ellipx,"*") ))*p^xprop/rd *
          (ry[2]-ry[1]) +
          as.vector(t( outer((br),c(rep(1,37),NA),"*") ))
    lx <- as.vector(t( outer(l1,ellipy,"*") ))*p^xprop/rd+x
   }
  }
  else { # continuous
   fh <- ff[[i]]
   br <- ry[1]+(ry[2]-ry[1])*((0:binz)-0.5)/(binz-1)
   l1 <- as.vector(t(cbind(br,br)))
   ly <- c(l1[1:(2*binz+1)],NA,l1[1:(2*binz+1)])
   l2 <- as.vector(t( ((-1)^(1:binz)) *cbind(fh$density,fh$density)))
   lx <- x+(c(-l2[1],l2,NA,l2[1],-l2)/rd)*p^xprop
  }
  return(cbind(lx,ly))
 }
 boxes <- list()
 for(i in 1:m) {
  boxes[[i]] <- hislin(dat[[i]], ry,
                       sum(pps[1:i]^xprop)-(pps[i]^xprop)/2, pp[i]^xprop,
                       para=paras[[i]],rd,diskret[i],dis[i],
                       confluence[i],binz[i])
  }

 # preparation of dots for dotplots
 dotdot <- function(y,ry,x,p,para,diskret,dist,icons,binz){
  ni <- length(y)
  dx <- rep(0,ni)
  fh <- ff[[i]]
  if (diskret) {
   bins <- ry[2]-ry[1]+1
   br <- ry[1]:ry[2]
   if (dist=="poisson") {x1 <- dpois(br,lambda=para$lambda)}
   if (dist=="binomial"){x1 <- dbinom(br,size=para$size,prob=para$prob)}
   if (dist=="betabinomial"){x1 <- dbetabin(br,para)}
   xincrement <- 0.8*p^xprop*
                 (sum(x1,na.rm=T))/
                 (ni*rd)
   yincrement <- max(xincrement*(ry[2]-ry[1])/2,bins/1000)
#   # "circles" are smoothed through 36 points
#   ellipsx <- c(0.5*xincrement*sin(pi*(0:36)/18),NA)
#   ellipsy <- c(0.5*yincrement*cos(pi*(0:36)/18),NA)
   for(bin in 1:(bins)){
    en <- fh$count[bin]
    if (bin==1) {cs <- 0}
    else {cs <- sum(fh$counts[1:(bin-1)])}
    px <- (1:max(en,1,na.rm=T))-0.5
    if (is.na(en)|(en==0)) {}
    else {
     dx[cs+(1:en)] <- x+1.25*xincrement*px-x1[bin]*p^xprop/rd
    }
   }
  }
  else { # continuous
   yincrement <- 0.8*(ry[2]-ry[1])/(binz-1)
   xincrement <- 1.6*p^xprop*
                 (sum(fh$density,na.rm=T))/
                 (sum(fh$counts,na.rm=T)*rd)
   for(bin in 1:(binz)){
    en <- fh$count[bin]
    if(bin==1){cs <- 0}
    else  {cs <- sum(fh$counts[1:(bin-1)])}
    px <- (1:max(floor(en/2),1,na.rm=T))-0.5
    if(is.na(en)|(en==0)){}
    else {
     desi <- as.vector(t(cbind(-px,px)))
     if(floor(en/2)!=(en/2)){desi <- 0.5+c(desi,-en/2)}
     if(en==1){desi<-0}
     dx[(cs+1):(cs+en)] <- x+1.25*xincrement*desi
    }
   }
  }
  y <- sort(y)
  ret <- matrix(NA,1,2)
  for(i in 1:ni) {
    ret <- rbind(ret,cbind(dx[i]+xincrement*icons[[i]][,1],
                            y[i]+yincrement*icons[[i]][,2]))
  }
  return(ret)
 }
 dots <- list()
 for(i in 1:m) {
  dots[[i]] <- dotdot(dat[[i]], ry,sum(pps[1:i]^xprop)-pps[i]^xprop/2,
                      pp[i]^xprop,para=paras[[i]],diskret[i],dis[i],
                      icons[[i]],binz[i])
  }

# calculation of violins, ships, error bars and density trace residuals
 # and connecting lines
 rr <-  c(0,sum(pps[1:i]^xprop))
 dx <- list()
 ships <- list(); violins <- list(); vioquants <- list();
 errs <- list(); connects <- list()
 # connecting lines
 if(connect[1]!=FALSE){
  for(j in 1:cs){
   connects[[j]] <- matrix(NA,m,2)
   for(i in 1:cp){
    xgr <- connect[i,j]
    if(loc[xgr]=="mean"){loci <- paras[[xgr]]$amean}
    if(loc[xgr]=="geomean"){loci <- paras[[xgr]]$gmean}
    connects[[j]][i,] <- c(xs[xgr],loci)
 }}}
 # error bars
 erry <- matrix(c(1,1,NA,1,1,NA,1,1,NA,1,1,
                     0,  0,NA,-1,1,NA, -1,  -1,NA,   1,  1),
                ncol=11,nrow=2,byrow=T)
 errx <- matrix(c(1,1,NA,1,1,NA,1,1,NA,1,1,
                  -1/2,1/2,NA, 0,0,NA, -1/4,1/4,NA,-1/4,1/4),
                ncol=11,nrow=2,byrow=T)
 for(i in 1:m) {
  if(err[i]){
   if(loc[i]=="mean"){loci <- mean(dat[[i]])
    if(prec[i]=="se"){preci <- sd(dat[[i]])/sqrt(tab[i])}
    if(prec[i]=="2se"){preci <- 2*sd(dat[[i]])/sqrt(tab[i])}
    if(prec[i]=="sd"){preci <- sd(dat[[i]])}
    if(prec[i]=="2sd"){preci <- 2*sd(dat[[i]])}
   }
   if(loc[i]=="geomean"){loci <- mean(log(dat[[i]]))
    if(prec[i]== "se") {preci <-   sd(log(dat[[i]]))/sqrt(tab[i])}
    if(prec[i]=="2se") {preci <- 2*sd(log(dat[[i]]))/sqrt(tab[i])}
    if(prec[i]== "sd") {preci <-   sd(log(dat[[i]]))}
    if(prec[i]=="2sd") {preci <- 2*sd(log(dat[[i]]))}
   }
   errs[[i]] <- t(rbind(c(xs[i],
                        pp[i]^xprop/4+0.5*pp[i]^xprop)%*%errx,
                        c(loci,preci)%*%erry))
   if(loc[i]=="geomean"){errs[[i]][,2]<-exp(errs[[i]][,2])}
  }
  # ships
  if(diskret[i]) {
   ships[[i]] <- cbind(c(dat[[i]][1],NA),c(0,NA))
   dx[[i]] <- c(0,NA)
  }
  else { # continuous
   dx[[i]] <- wurst(dat[[i]], qq[[i]],dis[i],paras[[i]],F,mix=mix[i])
   # compute density trace
   dst <- density(x=dat[[i]],n=(10*pz+2),from=qq[[i]][1],to=qq[[i]][2],
                 bw=sdsk[i], kernel=kern)$y
   if (dtr[i]){dsi <- dst} else {dsi <- rep(0,10*pz+2)}
   # quantile marks
   quantv <- quantile(dat[[i]],quant,na.rm=T,names=F)
   # residuals
   dr <- c(-dsi,rep(NA,3*ql))+wurst(dat[[i]], qq[[i]],dis[i],paras[[i]],T,mix=mix[i])
   # drei Kurven
   dd <- sum(pps[1:i]^xprop)-pps[i]^xprop/2+pp[i]^xprop*
           c(dx[[i]],NA,-dx[[i]],NA,dr)/rd
   # support
    r <- qq[[i]]
    br <- r[1]+(r[2]-r[1])*((0:(pz+1))-0.5)/(pz)
    brr <- r[1]+(r[2]-r[1])*((0:(10*pz+1))-0.5)/(10*pz)
    # quantile marks
    if (dis[i]=="lognormal"){
     quants <- qlnorm(quant,paras[[i]]$my,paras[[i]]$s)
    }
    if (dis[i]=="normal"){
     quants <- qnorm(quant,paras[[i]]$my,paras[[i]]$s)
    }
    if (dis[i]=="2normal"){
#     quants <- quantile(dat[[i]],quant,na.rm=T,names=F)
     quants <- vector()
     for (j in 1:length(quant)){
      quants[j] <- nlm(q2norm,quantile(y,quant[j]),para=paras[[i]],q=quant[j])$estimate
     }
    }
    if (dis[i]=="mixnorm"){
#     quants <- quantile(dat[[i]],quant,na.rm=T,names=F)
     quants <- vector()
     for (j in 1:length(quant)){
      quants[j] <- nlm(qmixnorm,quantile(y,quant[j]),para=paras[[i]],q=quant[j])$estimate
     }
    }
    if (dis[i]=="t"){
     quants <- qt(quant,df=paras[[i]]$df)*paras[[i]]$s+paras[[i]]$my
    }
    if (dis[i]=="beta"){
     quants <- qbeta(quant,paras[[i]]$a,paras[[i]]$b)
    }
    if (dis[i]=="gamma"){
     quants <- qgamma(quant,paras[[i]]$a,,paras[[i]]$b)
    }
    if (dis[i]=="weibull"){
     quants <- qweibull(quant,paras[[i]]$c,paras[[i]]$a)+paras[[i]]$y0
    }
    br <- c(br,as.vector(t(cbind(NA,quants,quants))))
   # two curves
   qv <- max(dst)*4*quant*(1-quant)
   vioquants[[i]] <- cbind(sum(pps[1:i]^xprop)-pps[i]^xprop/2+pp[i]^xprop/space*
                           as.vector(t(cbind(NA,-qv,qv))),
                           as.vector(t(cbind(NA,quantv,quantv))))
   dv <- sum(pps[1:i]^xprop)-pps[i]^xprop/2+pp[i]^xprop*
         c(dst,NA,-dst)/rd
   violins[[i]] <- cbind(dv,c(brr,NA,brr))
   brr <- c(brr,rep(NA,(3*ql)) )
   ships[[i]] <- cbind(dd,c(br,NA,br,NA,brr))
  }
 }

 # draw it all
 par(xpd=NA,xaxt="n")
 plot(rr,ry,xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),pch=" ",...)
 text(xs,rep(ry[1]-0.11*(ry[2]-ry[1]),m),xl)
 if(fill|(sum(abs(cens))>0)) {fil <- NA} else {fil <- 0}
 if(is.vector(fillcol)) {
  if(length(fillcol)==1) {filcol <- rep(fillcol,n)}
  else {filcol <- rep(fillcol,tab)}
 }
 else filcol <- unlist(fillcoll)
 if(sum(abs(cens))>0) {          # in the presence of censoring
  for(i in 1:m) {eve[[i]]<-eve[[i]][order(dat[[i]])]} # sort like y
  filcol[unlist(eve)!=1] <- 0    # empty circles show censored values
  fillcol <- split(filcol,rep(1:m,tab))
 }
 # expand short indicators of graphems and aesthetics
 if(is.vector(elicol)) {
  if(length(elicol)==1) {elicol <- as.list(rep(elicol,m))}
  else {elicol <- as.list(elicol[1:m])}
 }
 if(length(hiscol)==1) hiscol <- rep(hiscol,m)
 if(length(viocol)==1) viocol <- rep(viocol,m)
 if(length(boxcol)==1) boxcol <- rep(boxcol,m)
 if(length(errcol)==1) errcol <- rep(errcol,m)
 if(length(concol)==1) concol <- rep(concol,cs)
 if(length(shipcol)==1) shipcol <- rep(shipcol,m)
 if(length(plank)==1) plank <- rep(plank,m)
 if(length(clc)==1) clc <- rep(clc,m)
 # for each x-level draw distribution
 for (i in 1:m) {
  if(his[i]) {
   lines(boxes[[i]],col=hiscol[i])
   lines(rep(xs[i],2),c(ry[1]/1.03,ry[2]*1.02),col="white",lwd=1)
  }
  if(vio[i]) lines(violins[[i]],col=viocol[i],lwd=plank[i]-2*ship[i])
  if(ship[i]) {
   lines(ships[[i]],lwd=plank[i],col=shipcol[i])
   if(cline[i]&!box[i]) lines(rep(xs[i],2),qq[[i]],col=clc[i],lwd=plank[i]-1)
  }
 }
 # for each x-level draw dots on top of distributions
 # and error bars and boxplots on top of dots
 for (i in 1:m) {
  if(dot[i]) polygon(dots[[i]],density=fil,col=fillcol[[i]],border=elicol[[i]])
  if(vio[i]) lines(vioquants[[i]],col=viocol[i],lwd=plank[i]-2*ship[i])
  if(err[i]&(sum(abs(eve[[i]])==1))){lines(errs[[i]],col=errcol[i],lwd=3)}
 }
 if(connect[1]!=F){for(j in 1:cs){lines(connects[[j]],col=concol[j],lwd=3)}}
 if(sum(box)>0) boxplot(y~x, data=data.frame(x=x,y=y),
        subset=x%in%(unique(x)[box]), border=boxcol[box],
        add=T, range = 1.5,
        width = pps[box]^xprop, varwidth = FALSE,
        notch = FALSE, outline = TRUE, names=NULL, plot = TRUE,
        col = NULL, log = "",
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
        horizontal = FALSE, at = xs[box])
 cat(binz,"bins und space",space,"\n")
} # end of function





