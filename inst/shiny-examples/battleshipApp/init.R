
x <- rep(c(1:3),c(150,50,50)*.6)
y <- 1+x+rnorm(length(x),x/2)

choices <- as.list(iconsE)
names(choices) <- iconsE
