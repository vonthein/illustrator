
source("init.R")

server <- function(input, output) {
  output$pdfPlot <- renderPlot({
    icon <- eval(parse(text=input$icon))
    if((input$icon=="bone")|(input$icon=="cod")|(input$icon=="capsule")) {icon <- icon[,c(2,1)]}
    Tplot(x, N=input$N, nc=input$nc, ns=input$ns, icon=icon,
          space=input$space, s.x=input$s.x, s.y=input$s.y, seed=input$seed,
          cex.axis=input$cex.axis, cex.lab=input$cex.lab, cut=(input$radio==1),
          col=input$col, icol=input$icol, bcol=input$bcol, border=input$border,
          xlab = "Häufigkeit in % von N"
    ) # Tplot
  }) # pdfPlot
  cps <- 10^round(log10(max(x))-2,0) # count per symbol
  output$pdfDescription <- renderText({paste("Symbole zeigen absolute Häufigkeit, 1 Symbol für",
                                             cps, "Beobachtungen, häufigste Klasse zuerst.")})
} # server
