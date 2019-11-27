library(shiny)
library(bSims)

ui <- navbarPage("bSims",
  tabPanel("Detection function (HER)",
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
      sliderInput("tauH", "tau H", 0, 5, 1, 0.1),
      sliderInput("tauE", "tau E", 0, 5, 1, 0.1),
      sliderInput("tauR", "tau R", 0, 5, 1, 0.1),
      sliderInput("b", "b", 0, 10, 2, 0.25),
      sliderInput("rmax", "r max", 0, 10, 2, 0.25)
    ),
    column(6,
      uiOutput("breaks")
    ),
    column(12,
      verbatimTextOutput("settings"),
      uiOutput("clip")
    )
  )
)

server <- function(input, output) {
  output$breaks <- renderUI({
    tagList(
      sliderInput("offset", "offset", 0, input$rmax, 0, input$rmax/10000),
      sliderInput("wR", "road width", 0, input$rmax, 0, input$rmax/10000),
      sliderInput("wE", "edge width", 0, input$rmax, 0, input$rmax/10000)
    )
  })
  output$plot_dfun <- renderPlot({
    br <- input$offset + c(-(input$wE+input$wR/2), -(input$wR/2), input$wR/2, input$wE+input$wR/2)
    br[br < 0] <- 0
    br[br > input$rmax] <- input$rmax
    tau <- c(input$tauH, input$tauE, input$tauR, input$tauE, input$tauH)
    d <- seq(0, input$rmax, input$rmax/1000)
    g <- if (input$hazard) {
      function(d, tau) 1-exp(-(d/tau)^-input$b)
    } else {
      function(d, tau) exp(-(d/tau)^input$b)
    }

    plot(d, dist_fun2(d, tau[1], g), type="n", ylim=c(0,1), axes=FALSE,
      xlab="Distance (100 m)", ylab="P(detection)")
    axis(1)
    axis(2)
    for (i in seq_len(length(br)+1)) {
      x1 <- c(0, br, input$rmax)[i]
      x2 <- c(0, br, input$rmax)[i+1]
      polygon(c(0, br, input$rmax)[c(i, i, i+1, i+1)], c(0, 1, 1, 0),
        border=NA,
        col=c("darkolivegreen1", "burlywood1", "lightgrey",
        "burlywood1", "darkolivegreen1")[i])
    }
    lines(d, dist_fun2(d, tau[1], g))
    lines(d, dist_fun2(d, tau[2], g))
    lines(d, dist_fun2(d, tau[3], g))
    req(d, tau, g, br)
    lines(d, dist_fun2(d, tau, g, br), col=2, lwd=3)
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
