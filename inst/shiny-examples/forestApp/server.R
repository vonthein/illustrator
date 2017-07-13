
source("init.R")

# input <- list(ni=30, nrow=5, near=3, icon="fir", path=2, colle1="green", colle2="lightgreen", colre1="green", colre2="darkgreen", iex = 1, ip = 1, seed = 12345, col="black")


server <- function(input, output) {
  output$pdfPlot <- renderPlot({
      Forest(ni    = input$ni,
             nrow  = input$nrow,
             near  = input$near,
             icon  = eval(parse(text=input$icon)),
             path  = input$path,
             sizeratio = input$sizeratio,
             colle = c(input$colle1,input$colle2),
             colri = c(input$colre1,input$colre2),
             iex   = input$iex,
             ip    = input$ip,
             seed  = input$seed,
             col   = input$col)
  })
  output$colle1 <- renderUI({
    colourpicker::colourInput("colle1", "Left color from", "green")
  })
  output$colle2 <- renderUI({
    colourpicker::colourInput("colle2", "Left color to", "lightgreen")
  })
  output$colri1 <- renderUI({
    colourpicker::colourInput("colri1", "Right color from", "darkgreen")
  })
  output$colir2 <- renderUI({
    colourpicker::colourInput("colir2", "Right color to", "green")
  })
  output$col <- renderUI({
    colourpicker::colourInput("col", "Line color", "black")
  })
 }
