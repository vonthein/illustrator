
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    icon <- eval(parse(text=input$icon))
    if((input$icon=="bone")|(input$icon=="cod")) {icon <- icon[,c(2,1)]}
    Tplot(x, N=input$N, nc=input$nc, ns=input$ns, icon=icon,
          space=input$space, s.x=input$s.x, s.y=input$s.y, seed=input$seed,
          cex.axis=input$cex.axis, cex.lab=input$cex.lab, cut=(input$radio==1),
          col=input$col, icol=input$icol, bcol=input$bcol, border=input$border,
          xlab = "Häufigkeit in % von N"
    ) # Tplot
  }) # pdfPlot
  output$pdfDescription <- renderText({paste("Symbole zeigen absolute Häufigkeit, häufigste Klasse zuerst.")})
} # server
