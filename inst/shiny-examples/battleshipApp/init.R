
x <- rep(c(1:3),c(150,50,50)*.6)#*100
#y <- 1+x+rnorm(length(x),x/2)
y <- round(10+10*x+rnorm(length(x),sd=x+5),0)

if(!is.null(.GlobalEnv$iconsE)) iconsE <- .GlobalEnv$iconsE
choices <- as.list(iconsE)
names(choices) <- iconsE
