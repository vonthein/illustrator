

#' Tplot
#'
#' @param y vector containing dependent variable
#' @param  x      matrix with labels in rownames and absolute frequencies in x[,1]
#' @param  N      total number (denominator), if not sum(x[,1])
#' @param  nc     number of characters of labels
#' @param  ns     scal of labels
#' @param  verbose if TRUE, frequencies appear in labels
#' @param  icon   two-column matrix of coordinates or "bar" for barplot
#' @param  space  factor, usually 0.5 < 1, applied to x-dimension of icons for spacing
#' @param  icol   icon outline color
#' @param  col    icon fill color
#' @param  cut    shall partial icons be displayed, if representing more than one observation?
#' @param  xlab   xlab
#' @param  bcol   bar color
#' @param  border bar and axis line color
#' @param  s.x    SD to jitter icons horizontally in max(relative frequency)/50
#' @param  s.y    SD to jitter icons vertically in dim(x)[1]/200
#' @param  seed   seed for jittering
#' @param ...     graphics parameters handed to call of barplot

#' @return Plots the specified barplot using jittered icons.

#' @examples
#' \dontrun{x <- matrix(1:3,3,1)
#' rownames(x) <-c("eins","zwei","dreivierfünfsechssieben\n achtneunzehnelfzwölf")
#' Tplot(x,45,nc=50,ns=2)
#' Tplot(x,nc=30,ns=2,icon=fir,icol="darkgreen",col="green",bcol="#FAFAFA", border="#EEEEEE",cut=TRUE,cex.axis=1.7,cex.lab=1.7)
#' Tplot(x,nc=30,ns=2,icon=cod[,c(2,1)],icol="blue",col="lightblue",border="white")
#' Tplot(x,nc=30,ns=1.5,icon=human,icol="gray20",col="mistyrose",border="lavender")
#' x <- matrix(c(3,4,9,17,57),ncol=1)
#' rownames(x)<- c("Ziege","Schaf","Rind","Schwein","Huhn")
#' Tplot(x,nc=5,ns=1,icon=bone[,c(2,1)],space=0.7,icol="grey50",bcol="grey90",border="grey80")
#' }



#library(stringr)
# Darstellung der Häufigkeiten als Kombination aus
# Balkendiagramm und sortierter Liste

Tplot <- function(x,N=sum(x[,1]),nc=max(apply(as.matrix(row.names(x)),1,stringr::str_length)),ns=1,
                  verbose=F,icon="bar",space=1,icol="blue", col="lightblue",cut=FALSE,
                  xlab="Häufigkeit (%)",bcol="white",border=par("fg"),
                  s.x=1, s.y=1, seed=12345, ...){
#  Tornado-Plot
# x      matrix with labels in rownames and absolute frequencies in x[,1]
# N      total number (denominator), if not sum(x[,1])
# nc     number of characters of labels
# ns     scal of labels
# verbose if TRUE, frequencies appear in labels
# icon   two-column matrix of coordinates or "bar" for barplot
# space  factor, usually 0.5 < 1, applied to x-dimension of icons for spacing
# icol   icon outline color
# col    icon fill color
# cut    shall partial icons be displayed, if representing more than one observation?
# xlab   xlab
# bcol   bar color
# border bar and axis line color
# s.x    SD to jitter icons horizontally in max(relative frequency)/50
# s.y    SD to jitter icons vertically in dim(x)[1]/200

# Beispiel:
# x <- matrix(1:3,3,1)
# rownames(x) <-c("eins","zwei","dreivierfünfsechssieben\n achtneunzehnelfzwölf")
# Tplot(x,45,nc=50,ns=2)
# Tplot(x,nc=30,ns=2,icon=fir,icol="darkgreen",col="green",bcol="#FAFAFA", border="#EEEEEE",cut=TRUE,cex.axis=1.7,cex.lab=1.7)
# Tplot(x,nc=30,ns=2,icon=cod[,c(2,1)],icol="blue",col="lightblue",border="white")
# Tplot(x,nc=30,ns=1.5,icon=human,icol="gray20",col="mistyrose",border="lavender")
# x <- matrix(c(3,4,9,17,57),ncol=1)
# rownames(x)<- c("Ziege","Schaf","Rind","Schwein","Huhn")
# Tplot(x,nc=5,ns=1,icon=bone[,c(2,1)],space=0.7,icol="grey50",bcol="grey90",border="grey80")
  set.seed(seed)
  n <- length(rownames(x)) # categories
  if(is.na(nc)) nc <- max(nchar(rownames(x)))
  par(mar=c(5,0.9,3,nc),fg=border)
  ao <- x; dimnames(ao) <- NULL
  pe <- 100*ao[order(x)]/N
  am <- pe[1]
  # icons
  if(is.matrix(icon)){
    barplot(-pe, las=1, col=bcol,
            ylab=NULL, xlab=xlab, xaxt="n",
            width=1, space=0.7, horiz=TRUE, ...)
    # calculate scaling
    iwid <- max(icon[,1],na.rm=TRUE)-min(icon[,1],na.rm=TRUE)
    ihig <- max(icon[,2],na.rm=TRUE)-min(icon[,2],na.rm=TRUE)
    nii <- N*pe / (10^(round(log10(N*am),0)))
    if(cut){
      ni <- floor(nii)
      pi <- nii - ni                        # proportion to show
      ri <- min(icon[,1],na.rm=TRUE) + (1 - pi) * iwid # rim
      ri <- ri * am / ((nii[1]) * iwid)     # rescaled
    }
    else {
      ni <- round(nii) # obervations per category
    }
    icon[,1] <- space * icon[,1] * am / ((nii[1]) * iwid)
    icon[,2] <- -0.5 + icon[,2] / ihig
    # locations
    locy <- rnorm(n*ni,0,s.y*n/200) + rep(1:n,ni) * 1.7-0.5
    incr <-  am / (nii[1])
    locx <- rnorm(n*ni,0,s.x*am/50) - incr * sequence(ni)
    iconsx <- as.vector(outer(icon[,1],locx,"+"))
    iconsy <- as.vector(outer(icon[,2],locy,"+"))
    icons <- cbind(iconsx,iconsy)
    # plot and shade
    polygon(icons,col=col)
    lines(icons,col=icol,type="l")
    if(cut){
     for(i in 1:n) {
       picon <- icon
       picon[icon[,1]<ri[i],1] <- ri[i]
       picon[,1] <-  picon[,1] -incr * (ni[i] + 1)
       picon[,2] <- picon[,2] + i * 1.7-0.5
       polygon(picon,col=col)
       lines(picon,col=icol,type="l")
     }
    }
  }
  else {
    barplot(-pe, las=1, col=bcol,
            ylab=NULL, xlab=xlab, xaxt="n",
            width=1, space=0.7, horiz=TRUE, ...)
  }
  # labels
  labs <- axTicks(1)
  axis(1,at=labs,labels=-labs, ...)
  pabt <- vector()
  if(verbose){
    for(i in 1:n) {
      # prints absolute and relative frequencies in labels
      pabt[i] <- paste(round(100*ao[i,1]/N,0),"%",
                       " (",ao[i,1],"/",N,") ",
                       rownames(x)[i], sep="")
    }
  }
  else {
    pabt <- rownames(x)
  }
  pabt <- pabt[order(x)]
  text(x=rep(0.1+am/5,n),y=(1:n)*1.7-0.5,
       labels=pabt, col="black",
       adj=c(0,0.5), xpd=NA, cex=0.9*(68/nc)^0.3*ns)
  par(mar=c(5,4,4,1)+0.1,fg="black")
}

