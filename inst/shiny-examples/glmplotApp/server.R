
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    daten <- rglm(n    = input$n,
                  link = input$link,
                  dist = input$dist,
                  beta = c(input$b0,input$b1,input$b2,input$b3,input$b4,input$b5),
                  xes  = list(x1=input$x1,x2=input$x2,x3=input$x3,x4=input$x4),
                  s    = input$s,
                  seed = input$seed)
    par(mar=rep(0,4),oma=rep(0,4))
    plotglm(icon = eval(parse(text=input$icon)),
            iex = input$iex,
            colo = c(input$col1,input$col2), # extremes of shadepalette
            rot = input$rot,
            str = input$str,
            data=daten
    ) # plotglm
  }) # pdfPlot
  #output$pdfDescription <- renderText({paste("Sizes are")})
  #output$pdfSum <- renderText({paste("c(2,1.4,1,0.7,0.5)")})
}
