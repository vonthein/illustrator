
source("init.R")

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("nij",
                    label = "nij Individuals per sub-group",
                    value = 5, min = 1, max = 25),
        sliderInput("r",
                    label = "r rows or levels of first factor",
                    value = 2, min = 1, max = 7, step=1),
        sliderInput("c",
                    label = "c columns or levels of second factor",
                    value = 2, min = 1, max = 7, step=1),
        sliderInput("re",
                    label = "re row effect per level of first factor",
                    value = 5, min = -20, max = 50, step=0.1),
        sliderInput("ce",
                    label = "ce column effect per level of second factor",
                    value = 5, min = -20, max = 50, step=0.1),
        sliderInput("ie",
                    label = "ie interaction effect per level of either factor",
                    value = 5, min = -20, max = 50, step=0.1),
        sliderInput("seed",
                    label = "seed",
                    value = 1,
                    min = 0, max = 99999)
      ),# panel
      mainPanel(
        selectInput("icon", label = "icon Symbol",
                    choices = choices,
                    selected = "mouse"),
        sliderInput("m",
                    label = "m magnification of symbol",
                    value = 2, min = 0, max = 5, step=0.1),
        sliderInput("p",
                    label = "p Proportion of symbol, < 1 is narrower",
                    value = 1, min = 0.1, max = 10, step=0.01),
        colourpicker::colourInput("filcol1", "Fill color from", "darkgrey"),
        colourpicker::colourInput("filcol2", "Fill color to", "white"),
        #p(tit),
        plotOutput("pdfPlot"),
        textOutput("pdfDescription"),
        strong(textOutput("pdfSum"))
      ) # panel
    ) # layout
  ) # page
)
