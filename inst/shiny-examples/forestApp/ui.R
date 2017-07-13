
source("init.R")

shinyUI(
  fluidPage(
    # sidebarLayout(
    # sidebarPanel(
    fluidRow(
      column(2,#"sidebar",
             sliderInput("ni",
                         label = "ni Number of symbols per group",
                         value = 30, min = 1, max = 100, step=1),
             sliderInput("nrow",
                         label = "nrow Number of rows",
                         value = 5,
                         min = 0, max = 10, step=1),
             sliderInput("path",
                         label = "path Gap between groups",
                         value = 5,
                         min = 0, max = 15, step=0.1),
             sliderInput("near",
                         label = "near Factor to space between symbols in rows",
                         value = 3,
                         min = -3, max = 5, step=0.1),
             sliderInput("sizeratio",
                         label = "sizeratio Size proportion of right to left symbols",
                         value = 1.4,
                         min = 0, max = 5, step=0.1)
      ), # column
      column(7,#"main",
             plotOutput("pdfPlot"),
             fluidRow(
               column(4,
               selectInput("icon", label = "icon Symbol",
                                 choices = choices,
                                 selected = "fir")
               ),
               column(3,
                      sliderInput("iex",
                                  label = "iex Symbol expansion factor",
                                  value = 1,
                                  min = 0, max = 10, step=0.1)
               ),
               column(3,
                      sliderInput("ip",
                                  label = "ip Symbol height factor",
                                  value = 1,
                                  min = 0, max = 5, step=0.1)
               )) # row
       ), # column
      column(3,#"right",
             colourpicker::colourInput("colle1", "Left color from", "green"),
             colourpicker::colourInput("colle2", "Left color to", "lightgreen"),
             colourpicker::colourInput("colre1", "Right color from", "darkgreen"),
             colourpicker::colourInput("colre2", "Right color to", "green"),
             colourpicker::colourInput("col", "Line color", "black"),
             sliderInput("seed",
                         label = "seed for random number generator",
                         value = 12345, min = 1, max = 100000, step=1)
      ) # column
    ) # row
  ) # page
) # ui

