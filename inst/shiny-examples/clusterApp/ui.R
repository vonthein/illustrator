
source("init.R")

shinyUI(
  fluidPage(
    fluidRow(
      column(2,
             selectInput("icon", label = "icon Symbol",
                         choices = choices,
                         selected = "cod"),
             sliderInput("nij",
                         label = "nij Symbols per size",
                         value = 7, min = 1, max = 21),
             sliderInput("delta",
                         label = "Standardized difference of means delta",
                         value = 1.5,
                         min = 0, max = 3, step=0.01),
             sliderInput("seed",
                         label = "seed",
                         value = 1.5,
                         min = 0, max = 99999)
      ),
      column(7,
             p("Cluster"),
             plotOutput("pdfPlot"),
             textOutput("pdfDescription"),
             strong(textOutput("pdfSum"))
      ),
      column(3,
             sliderInput("m",
                         label = "Magnification of symbols",
                         value = 3, min = 1, max = 21,step=0.1),
             sliderInput("p",
                         label = "Proportion of symbols",
                         value = 1, min = 0.1, max = 10,step=0.01),
             colourpicker::colourInput("col1", "Right color from", "blue"),
             colourpicker::colourInput("col2", "Right color to", "lightblue"),
             colourpicker::colourInput("col3", "Left color from", "darkgreen"),
             colourpicker::colourInput("col4", "Left color to", "green")
      )
    )
  )
)
