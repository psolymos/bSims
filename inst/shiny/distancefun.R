library(shiny)

ui <- navbarPage("bSims",
  tabPanel("Detection function",
    plotOutput(outputId = "plot_dfun"),
    checkboxInput("hazard", "Hazard rate formulation (1-exp(...))"),
    sliderInput("tau", "tau", 0, 5, 1, 0.1),
    sliderInput("b", "b", 0, 10, 2, 0.25),
    sliderInput("rmax", "r max", 0, 10, 2, 0.25)
  )
)

server <- function(input, output) {
  output$plot_dfun <- renderPlot({
    d <- seq(0, input$rmax, input$rmax/1000)
    g <- if (input$hazard) {
      function(d) 1-exp(-(d/input$tau)^-input$b)
    } else {
      function(d) exp(-(d/input$tau)^input$b)
    }
    plot(d, g(d), type="l", col=4, ylim=c(0,1),
      xlab="Distance (100 m)", ylab="P(detection)")
  })
}

shinyApp(ui = ui, server = server)
