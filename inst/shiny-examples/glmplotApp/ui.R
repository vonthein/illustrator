
source("init.R")

shinyUI(
  fluidPage(
    fluidRow(
      column(2,
             selectInput("dist", label = "dist function",
                         choices = list("normal" = "normal",
                                        "binomial" = "binomial",
                                        "poisson" = "poisson"),
                         selected = "normal"),
             sliderInput("b0",
                         label = "b0 intercept",
                         value = 1, min = -2, max = 5, step=.1),
             sliderInput("b1",
                         label = "b1 slope",
                         value = 1, min = -1, max = 2, step=0.01),
             sliderInput("b2",
                         label = "b2 color slope",
                         value = 1, min = -1, max = 2, step=0.01),
             sliderInput("b3",
                         label = "b3 size slope",
                         value = .1, min = -1, max = 2, step=0.01),
             sliderInput("b4",
                         label = "b4 rotation slope",
                         value = .1, min = -1, max = 2, step=0.01),
             sliderInput("b5",
                         label = "b5 interaction x * color",
                         value = .1, min = -1, max = 2, step=0.01),
             sliderInput("s",
                         label = "s standard deviation or normal or size of binomial",
                         value = 1, min = 0, max = 10, step=0.1)    ),# column
      column(2,
             selectInput("link", label = "link function",
                         choices = list("id" = "id",
                                        "log" = "log",
                                        "sqrt" = "sqrt",
                                        "logit" = "logit",
                                        "cloglog" = "cloglog"),
                         selected = "id"),
             sliderInput("n",
                         label = "n Symbols",
                         value = 36, min = 1, max = 250),
             sliderInput("x1",
                         label = "x1 levels",
                         value = 2, min = 1, max = 25),
             sliderInput("x2",
                         label = "x2 levels",
                         value = 2, min = 1, max = 25),
             sliderInput("x3",
                         label = "x3 levels",
                         value = 2, min = 1, max = 25),
             sliderInput("x4",
                         label = "x4 levels",
                         value = 2, min = 1, max = 25),
             colourpicker::colourInput("col1", "Fill color from", "green"),
             colourpicker::colourInput("col2", "Fill color to", "darkgreen"),
             sliderInput("seed",
                         label = "seed",
                         value = 1,
                         min = 0, max = 99999)
      ),
      column(8,fluidRow(column(6,
                               selectInput("icon", label = "icon Symbol",
                                           choices = choices,
                                           selected = "germ"),
                               sliderInput("rot",
                                           label = "rot Symbol rotation",
                                           value = 0, min = 0, max = 180, step=1)
      ),
      column(6,
             sliderInput("iex",
                         label = "iex Symbol expansion",
                         value = 1, min = 0.1, max = 10, step=0.01),
             sliderInput("str",
                         label = "str Symbol strech",
                         value = 1, min = 0.1, max = 10, step=0.01)
      ) # inner column
      ), # inner row
      p("GLM plot"),
      plotOutput("pdfPlot")
      #    textOutput("pdfDescription"),
      #    strong(textOutput("pdfSum"))
      ) # column
    ) # row
  ) # page
)
