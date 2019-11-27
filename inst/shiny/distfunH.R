library(shiny)


ui <- navbarPage("bSims",
  tabPanel("Detection function (H)",
    tagList(
      singleton(
        tags$head(
          tags$script(src = 'clipboard.min.js')
        )
      )
    ),
    plotOutput(outputId = "plot_dfun"),
    hr(),
    column(6,
      checkboxInput("hazard", "Hazard rate formulation (1-exp(...))"),
      sliderInput("tau", "tau", 0, 5, 1, 0.1),
      sliderInput("b", "b", 0, 10, 2, 0.25),
      sliderInput("rmax", "r max", 0, 10, 2, 0.25)
    ),
    column(6,
      verbatimTextOutput("settings"),
      uiOutput("clip")
    )
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
    plot(d, g(d), type="n", ylim=c(0,1), axes=FALSE,
      xlab="Distance (100 m)", ylab="P(detection)")
    axis(1)
    axis(2)
    polygon(c(0, max(d), max(d), 0), c(0, 0, 1, 1),
      border=NA, col="darkolivegreen1")
    lines(d, g(d), col=2, lwd=3)
  })
  getset <- reactive({
    c(
      "dist_fun <- function(d, tau) {",
      {if (input$hazard) {
        paste0("\n  1-exp(-(d/tau)^-", input$b, ")", collapse="")
      } else {
        paste0("\n  exp(-(d/tau)^", input$b, ")", collapse="")
      }},
      "\n}"
    )
  })
  output$settings <- renderText({
    getset()
  })
  output$clip <- renderUI({
    tagList(
      actionButton("clipbtn",
        label = "Copy settings to clipboard",
        icon = icon("clipboard"),
        `data-clipboard-text` = paste(
          getset(),
          collapse="")
      ),
      tags$script(
        'new ClipboardJS(".btn", document.getElementById("clipbtn") );')
    )
  })
}

shinyApp(ui = ui, server = server)
