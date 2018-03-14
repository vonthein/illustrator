
source("init.R")

shinyUI(
  fluidPage(
    # sidebarLayout(
    # sidebarPanel(
    fluidRow(
      column(2,#"sidebar",
             sliderInput("binz",
                         label = "binz Number of bins",
                         value = 0, min = 0, max = round(5*sqrt(length(unique(y))),0),step=1),
             sliderInput("space",
                         label = "space Space between groups",
                         value = 1,
                         min = 0, max = 5, step=0.1),
             checkboxInput("dot", label = paste("dot","Plot symbols"), value = TRUE),
             checkboxInput("fill", label = paste("fill","Fill symbols"), value = TRUE),
             checkboxInput("xprop", label = "xprop Joint rather than conditional distribution", value = TRUE),
             checkboxInput("box", label = "box Boxplots", value = FALSE),
             checkboxInput("err", label = "err Error bars", value = TRUE),
             checkboxInput("connect", label = "connect Connect measures of location", value = FALSE),
             checkboxInput("his", label = "his Histograms", value = FALSE),
             checkboxInput("vio", label = "vio Violin plots", value = FALSE),
             checkboxInput("ship", label = "ship Battleships", value = TRUE),
             checkboxInput("dtr", label = "dtr Density trace residuals", value = TRUE),
             radioButtons("loc",
                          label = "loc Location of error bars",
                          choices = list("mean" = "mean",
                                         "geomean" = "geomean")),
             radioButtons("prec",
                          label = "prec Whiskers of error bars",
                          choices = list("se" = "se",
                                         "sd" = "sd",
                                         "2se" = "2se",
                                         "2sd" = "2sd"))
      ),
      column(7,#"main",
             #mainPanel(
             p("Dotplot, histograms, boxplots, violin plots, battleship plots, error bars"),
             fluidRow(
               column(6,
                      selectInput("dis", label = "dis Distribution for battleships",
                                  choices = list("normal" = "normal", "lognormal" = "lognormal", "t" = "t",
                                                 "gamma" = "gamma", "beta" = "beta", "weibull" = "weibull",
                                                 "poisson" = "poisson", "binomial" = "binomial"),
                                  selected = "normal")
               ),
               column(6,
                      selectInput("icon", label = "icon Symbol",
                                  choices = choices,
                                  selected = "loop")
               )             ),
             plotOutput("pdfPlot"),
             column(4,
                    sliderInput("plank",
                                label = "plank Ship-line width",
                                value = 2,
                                min = 0.1, max = 5, step=0.1)
             ),
             column(8,
             selectInput("confluence", label = "confluence Confluence of integer variables",
                         choices = list("Tufte" = "Tufte", "Tukey" = "Tukey", "Duerr" = "Duerr"),
                         selected = "Tufte")
             ),
             textOutput("pdfDescription"),
             strong(textOutput("pdfSum"))
             #    ) # row
      ), # column
      column(3,#"right",
             colourpicker::colourInput("filcol1", "Fill color from", "lightblue"),
             colourpicker::colourInput("filcol2", "Fill color to", "#66F50A"),
             colourpicker::colourInput("elicol1", "Symbol color from", "blue"),
             colourpicker::colourInput("elicol2", "Symbol color to", "green"),
             conditionalPanel(condition="input.err",
                              uiOutput("errcolfrom")        ),
             conditionalPanel(condition="input.err",
                              uiOutput("errcolto")        ),
             conditionalPanel(condition="input.connect",
                              uiOutput("concolfrom")        ),
             conditionalPanel(condition="input.connect",
                              uiOutput("concolto")        ),
             conditionalPanel(condition="input.box",
                              uiOutput("boxcolfrom")        ),
             conditionalPanel(condition="input.box",
                              uiOutput("boxcolto")        ),
             conditionalPanel(condition="input.his",
                              uiOutput("hiscolfrom")        ),
             conditionalPanel(condition="input.his",
                              uiOutput("hiscolto")        ),
             conditionalPanel(condition="input.vio",
                              uiOutput("viocolfrom")        ),
             conditionalPanel(condition="input.vio",
                              uiOutput("viocolto")        ),
             conditionalPanel(condition="input.ship",
                              uiOutput("shipcolfrom")        ),
             conditionalPanel(condition="input.ship",
                              uiOutput("shipcolto")        )
      )
    ) # row
  ) # page
)
