
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    #tit <- renderText({paste(input$r,"x",input$c,"plan")})
    par(mar=rep(0,4), oma=rep(0,4))
    plan (nij = input$nij, r  = input$r,  c  = input$c,
          re = input$re, ce = input$ce, ie = input$ie,
          icon = input$m*(eval(parse(text=input$icon))%*%diag(c(input$p,1))),
          colo = c(input$filcol1,input$filcol2),
          seed = input$seed)
  })
  output$pdfDescription <- renderText({paste("Effects are")})
  output$pdfSum <- renderText({paste(input$re,"per row", input$ce, "per column and",
                                     input$ie,"interaction")})
}
