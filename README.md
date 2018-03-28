## Installation
```
install.packages("devtools")
devtools::install_github("vonthein/illustrator")
#
library(illustrator)
runBattleshipApp() # explains function battleship
# similar shiny apps for plot functions:
# Tplot (sorted barplot)
# gradient
# forest
# cluster
# plan
# rglm & glmplot
#
# add a symbol
rup <- matrix(c(-1,1, 0,2, 1,0, -1,1), ncol = 2, byrow = TRUE)
add.icon(rup)
# make it the first and only symbol
set.icon(rup)
```
