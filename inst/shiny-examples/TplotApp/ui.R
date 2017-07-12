
source("init.R")

shinyUI(
  fluidPage(
    fluidRow(
      column(2,
             sliderInput("N",
                         label = "N",
                         value = sum(x), min = 0, max = 10*sum(x)),
             sliderInput("nc",
                         label = "nc",
                         value = max(apply(as.matrix(row.names(x)),1,stringr::str_length)),
                         min = 0, max = 80),
             sliderInput("ns",
                         label = "ns",
                         value = 1, min = 0, max = 2, step=0.01),
             sliderInput("space",
                         label = "space",
                         value = 1, min = .1, max = 2, step=0.01),
             sliderInput("s.x",
                         label = "s.x",
                         value = 1, min = 0, max = 2, step=0.01),
             sliderInput("s.y",
                         label = "s.y",
                         value = 1, min = 0, max = 2, step=0.01)
             #  )
      ),# column
      column(8,
             p("Tplot"),
             plotOutput("pdfPlot"),

             fluidRow(column(3), column(7,textOutput("pdfDescription")), column(2)),
             fluidRow(
               column(3,
                      sliderInput("seed",
                                  label = "seed",
                                  value = 1, min = 0, max = 100000, step=1)
               ), # inner column
               #     column(5),
               column(3,
                      selectInput("icon", label = "icon Symbol",
                                  choices = choices,
                                  selected = "bone")
               ), # inner column
               column(5)
             ) # inner row
      ), # column
      column(2,
             colourpicker::colourInput("col", "Fill color", "lightblue"),
             colourpicker::colourInput("icol", "Symbol color", "grey50"),
             colourpicker::colourInput("bcol", "Bar color", "grey90"),
             colourpicker::colourInput("border", "Bar and axis line color", "grey80"),
             radioButtons("radio",
                          label = "If 1 symbol per 10^d observations",
                          choices = list("Draw partial symbol" = 1,
                                         "Round n of symbols" = 2)),
             sliderInput("cex.axis",
                         label = "cex.axis",
                         value = 1, min = 0.1, max = 3, step=0.01),
             sliderInput("cex.lab",
                         label = "cex.lab",
                         value = 1, min = 0.1, max = 3, step=0.01)
      ) # column
    ) # row
  ) # page
)
