
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    par(mar=rep(0,4),oma=rep(0,4))
    Gradient(age = c(2, 1.4, 1, 0.7, 0.5), a = input$b0, b = input$b1,
             icon = input$m*(eval(parse(text=input$icon))%*%diag(c(input$p,1))),
             colo = c(input$filcol1,input$filcol2),
             ni = input$n, near = input$near, seed=input$seed)
  })
  output$pdfDescription <- renderText({paste("Sizes are")})
  output$pdfSum <- renderText({paste("c(2,1.4,1,0.7,0.5)")})
}
