
source("init.R")

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("n",
                    label = "n Symbols per class",
                    value = 10, min = 1, max = 25),
        sliderInput("near",
                    label = "near each other",
                    value = 0.5, min = 0.2, max = 2, step=0.1),
        sliderInput("b1",
                    label = "b1 slope",
                    value = 1, min = -1, max = 2, step=0.01),
        sliderInput("b0",
                    label = "b0 intercept",
                    value = 0, min = -20, max = 50, step=1),
        sliderInput("seed",
                    label = "seed",
                    value = 1,
                    min = 0, max = 99999),
        colourpicker::colourInput("filcol1", "Fill color from", "darkgreen"),
        colourpicker::colourInput("filcol2", "Fill color to", "green")
      ),# panel
      mainPanel(
        selectInput("icon", label = "icon Symbol",
                    choices = choices,
                    selected = "fir"),
        sliderInput("m",
                    label = "m magnification of symbol",
                    value = 2, min = 0, max = 5, step=0.1),
        sliderInput("p",
                    label = "p Proportion of symbol, < 1 is narrower",
                    value = 1, min = 0.1, max = 10, step=0.01),
        p("Gradient"),
        plotOutput("pdfPlot"),
        textOutput("pdfDescription"),
        strong(textOutput("pdfSum"))
      ) # panel
    ) # layout
  ) # page
)
