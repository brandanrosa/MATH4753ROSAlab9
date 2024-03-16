library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

  titlePanel("Waiting For A Train"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "range",
                  label = "a and b endpoints P(a<=Y<=b)",
                  step = 0.0001,
                  round = -4,
                  value = c(-2,3),
                  min = -10,
                  max = 10,
                  ticks = TRUE
      )
    ),

    mainPanel(
      plotOutput("distPlot",
                 click = "plot_click",
                 brush = brushOpts(id = "plot_brush", direction = "xy")
      ),
      verbatimTextOutput("probs"),
      verbatimTextOutput("loc")
    )))

server <- function(input, output, session) {

  re_brush <- reactive({input$plot_brush})

  aval <- reactive(input$range[1])
  bval <- reactive(input$range[2])

  observeEvent(input$plot_brush, {
    updateSliderInput(inputId = "range",
                      session = session,
                      value = c(input$plot_brush$xmin, input$plot_brush$xmax))
  })

  output$distPlot <- renderPlot(
    mytrain(a = aval(), b = bval())
  )

  output$probs <- renderPrint({
    req(input$plot_brush)
    a <- round(input$plot_brush$xmin, 4)
    b <- round(input$plot_brush$xmax, 4)
    prob <- ptrain(bval()) - ptrain(aval())
    dec <- round(prob, 4)
    cat("P[", a, " ", "<= Y <=", " ", b, "] =", " ", dec, sep = "")
  })

  output$loc <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 4)
    y <- round(input$plot_click$y, 4)
    cat("[", x, ", ", y, "]", sep = "")
  })

}

shinyApp(ui = ui, server = server)
