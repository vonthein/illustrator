
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    battleship(y, x, cens=0, dis=input$dis, xprop=input$xprop, space=input$space,
               his=input$his, binz=input$binz, fill=input$fill,
               err=input$err, loc=input$loc, prec=input$prec, connect=input$connect,
               box=input$box, vio=input$vio, dot=input$dot,
               ship=input$ship, plank=2, dtr=input$dtr,
               confluence="Tufte", icon=input$icon,
               fillcol=shape::shadepalette(length(unique(x)),input$filcol1,input$filcol2),
               elicol=shape::shadepalette(length(unique(x)),input$elicol1,input$elicol2),
               errcol=shape::shadepalette(length(unique(x)),input$errcol1,input$errcol2),
               concol=shape::shadepalette(length(unique(x)),input$concol1,input$concol2),
               hiscol=shape::shadepalette(length(unique(x)),input$hiscol1,input$hiscol2),
               boxcol=shape::shadepalette(length(unique(x)),input$boxcol1,input$boxcol2),
               viocol=shape::shadepalette(length(unique(x)),input$viocol1,input$viocol2),
               shipcol=shape::shadepalette(length(unique(x)),input$shipcol1,input$shipcol2),
               trials=0, start=list(m), mix=NA, genmix=F, df=4, kern="gaussian",
               sdsk=sqrt(var(y))/log(length(y)))
  })
  output$pdfDescription <- renderText({paste(" ")})
  output$pdfSum <- renderText({paste(" ")})
  output$errcolfrom <- renderUI({
    colourpicker::colourInput("errcol1", "Error bar color from", "orange")
  })
  output$errcolto <- renderUI({
    colourpicker::colourInput("errcol2", "Error bar color to", "brown")
  })
  output$concolfrom <- renderUI({
    colourpicker::colourInput("concol1", "Connection color from", "brown")
  })
  output$concolto <- renderUI({
    colourpicker::colourInput("concol2", "Connection color to", "orange")
  })
  output$hiscolfrom <- renderUI({
    colourpicker::colourInput("hiscol1", "Histogram color from", "darkgreen")
  })
  output$hiscolto <- renderUI({
    colourpicker::colourInput("hiscol2", "Histogram color to", "darkblue")
  })
  output$boxcolfrom <- renderUI({
    colourpicker::colourInput("boxcol1", "Boxplot color from", "orange")
  })
  output$boxcolto <- renderUI({
    colourpicker::colourInput("boxcol2", "Boxplot color to", "brown")
  })
  output$viocolfrom <- renderUI({
    colourpicker::colourInput("viocol1", "Violin plot color from", "brown")
  })
  output$viocolto <- renderUI({
    colourpicker::colourInput("viocol2", "Violin plot color to", "orange")
  })
  output$shipcolfrom <- renderUI({
    colourpicker::colourInput("shipcol1", "Ship color from", "orange")
  })
  output$shipcolto <- renderUI({
    colourpicker::colourInput("shipcol2", "Ship color to", "brown")
  })
}
