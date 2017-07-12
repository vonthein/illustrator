
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    par(mar=rep(0,4),oma=rep(0,4))
    Cluster(nij=input$nij,delta=input$delta,seed=input$seed,
            icon=input$m*eval(parse(text=input$icon))%*%diag(c(input$p,1)),
            colo=c(input$col1,input$col2,input$col3,input$col4))
  })
  output$pdfDescription <- renderText({paste("Sizes are")})
  output$pdfSum <- renderText({paste("c(2,1.4,1,0.7,0.5)")})
}
