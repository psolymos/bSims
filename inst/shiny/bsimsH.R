library(shiny)
library(detect)
library(bSims)

MAXDIS <- 20
EXTENT <- 10
DURATION <- 10
TINT <- list(
  "0-3-5-10 min"=c(3, 5, 10),
  "0-10 min"=c(10),
  "0-1-2-3 min"=c(1, 2, 3),
  "0-5-10 min"=c(5, 10),
  "0-3 min"=c(3),
  "0-1-2-3-4-5 min"=c(1, 2, 3, 4, 5)
)
RINT <- list(
  "0-50-100-Inf m"=c(0.5, 1, Inf),
  "0-Inf m"=c(Inf),
  "0-50-Inf m"=c(0.5, Inf),
  "0-50-100-150-Inf m"=c(0.5, 1, 1.5, Inf),
  "0-50-100-150-200-Inf m"=c(0.5, 1, 1.5, 2, Inf),
  "0-50-100 m"=c(0.5, 1),
  "0-50 m"=c(0.5),
  "0-50-100-150 m"=c(0.5, 1, 1.5),
  "0-50-100-150-200 m"=c(0.5, 1, 1.5, 2)
)
rv <- reactiveValues(seed=0)

estimate_bsims <- function(REM) {
  MaxDur <- max(tint)
  MaxDis <- max(rint)
  Ydur <- matrix(colSums(REM), 1)
  Ddur <- matrix(tint, 1)
  Ydis <- matrix(rowSums(REM), 1)
  Ddis <- matrix(rint, 1)
  if (length(tint) > 1 && sum(REM) > 0) {
    Mrem <- cmulti.fit(Ydur, Ddur, type="rem")
    phi <- exp(Mrem$coef)
    p <- 1-exp(-MaxDur*phi)
  } else {
    Mrem <- NULL
    phi <- NA
    p <- NA
  }
  if (length(rint) > 1 && sum(REM) > 0) {
    Mdis <- cmulti.fit(Ydis, Ddis, type="dis")
    tau <- exp(Mdis$coef)
    q <- if (is.infinite(MaxDis))
      1 else (tau^2/MaxDis^2) * (1-exp(-(MaxDis/tau)^2))
    A <- if (is.infinite(MaxDis))
      pi * tau^2 else pi * MaxDis^2
  } else {
    Mdis <- NULL
    tau <- NA
    q <- NA
    A <- NA
  }
  D <- sum(REM) / (A * p * q)
  list(
    Ydur=Ydur, Ddur=Ddur,
    Ydis=Ydis, Ddis=Ddis,
    Mrem=Mrem,
    Mdis=Mdis,
    phi=phi, tau=tau,
    A=A, p=p, q=q,
    D=D)
}
summarize_bsims <- function(res) {
  data.frame(
    D=sapply(res, "[[", "D"),
    phi=sapply(res, "[[", "phi"),
    tau=sapply(res, "[[", "tau"))
}

ui <- navbarPage("bSims (H)",
  tabPanel("Initialize",
    column(6,
      plotOutput(outputId = "plot_ini")),
    column(6,
      actionButton("seed", "Change random seed")
    )
  ),
  tabPanel("Populate",
    column(6,
      plotOutput(outputId = "plot_pop"),
      plotOutput(outputId = "plot_spfun")
    ),
    column(6,
      sliderInput("D", "Density", 0, 20, 1, 0.1),
      radioButtons("spfun", "Spatial pattern",
        c("Random"="random", "Regular"="regular",
          "Clustered"="clustered"))
    )
  ),
  tabPanel("Animate",
    column(6,
      plotOutput(outputId = "plot_ani")),
    column(6,
      sliderInput("phi1", "Vocal rate (group 1)", 0, 10, 0.5, 0.1),
      sliderInput("phi2", "Vocal rate (group 2)", 0, 10, 0, 0.1),
      sliderInput("mix", "Mixture (group 1)", 0, 1, 1, 0.05),
      sliderInput("phim", "Movement rate", 0, 10, 1, 0.1),
      sliderInput("SDm", "Movement SD", 0, 1, 0, 0.05),
      checkboxInput("overlap", "Territory overlap allowed", TRUE),
      checkboxInput("show_tess", "Show tessellation", FALSE),
      checkboxInput("init_loc", "Initial location", FALSE)
    )
  ),
  tabPanel("Detect",
    column(6,
      plotOutput(outputId = "plot_det"),
      plotOutput(outputId = "plot_dfun")
    ),
    column(6,
      sliderInput("tauV", "Detection parameter (tau), vocalizations", 0, MAXDIS, 1, MAXDIS/200),
      sliderInput("tauM", "Detection parameter (tau), movement", 0, MAXDIS, 1, MAXDIS/200),
      sliderInput("bpar", "Hazard rate parameter (b), vocalizations", 0, MAXDIS, 1, MAXDIS/200),
      radioButtons("dfun", "Distance function",
        c("Half Normal"="halfnormal",
          "Negative Exponential"="negexp",
          "Hazard rate"="hazrate")),
      radioButtons("event", "Event type",
        c("Vocalization"="vocal",
          "Movement"="move",
          "Both"="both")),
      sliderInput("dir_sens", "Anisotropy in tau due to direction", 0, 2, 1, 0.1)
    )
  ),
  tabPanel("Transcribe",
    fluidRow(
      column(6,
        plotOutput(outputId = "plot_tra")
      ),
      column(6,
        selectInput("tint", "Time intervals", names(TINT)),
        selectInput("rint", "Distance intervals", names(RINT)),
        sliderInput("derr", "Distance error", 0, 1, 0, 0.1),
        sliderInput("dbias", "Distance bias", 0, 2, 1, 0.1),
        radioButtons("condition", "Condition",
          c("1st event"="event1",
            "1st detection"="det1",
            "All detections"="alldet")),
        sliderInput("percept", "Percepted ratio", 0, 2, 1, 0.05),
        checkboxInput("oucount", "Over/under count", FALSE)
      )
    ),
    fluidRow(
      column(6,
        tableOutput(outputId = "table_rem")
      ),
      column(6,
        plotOutput(outputId = "plot_est")
      )
    )
  ),
  tabPanel("Settings",
    tagList(
      singleton(
        tags$head(
          tags$script(src = 'clipboard.min.js')
        )
      )
    ),
    column(12,
      verbatimTextOutput("settings"),
      uiOutput("clip")
    )
  ),
  tabPanel("Documentation",
    column(12,
      tags$iframe(src="https://psolymos.github.io/bSims/",
        height=600, width="100%", frameBorder=0)
    )
  )
)

server <- function(input, output) {
  observeEvent(input$seed, {
    rv$seed <- rv$seed + 1
  })
  dis <- seq(0, MAXDIS, MAXDIS/200)
  l <- reactive({
    set.seed(rv$seed)
    bsims_init(extent = EXTENT)
  })
  xy_fun <- reactive({
    switch(input$spfun,
      "random"=function(d) rep(1, length(d)),
      "regular"=function(d)
        (1-exp(-d^2/1^2) + dlnorm(d, 2)/dlnorm(exp(2-1),2)) / 2,
      "clustered"=function(d)
        exp(-d^2/1^2) + 0.5*(1-exp(-d^2/4^2))
      )
  })
  a <- reactive({
    margin <- switch(input$spfun,
      "random"=0,
      "regular"=2,
      "clustered"=5)
    bsims_populate(l(),
      density = input$D,
      xy_fun = xy_fun(),
      margin = margin)
  })
  b <- reactive({
    bsims_animate(a(),
      duration = DURATION,
      vocal_rate = c(input$phi1, input$phi2),
      move_rate = input$phim,
      movement = input$SDm,
      mixture = c(input$mix, 1-input$mix),
      allow_overlap = input$overlap,
      initial_location = input$init_loc)
  })
  dfun <- reactive({
    switch(input$dfun,
      "halfnormal"=function(d, tau) exp(-(d/tau)^2),
      "negexp"    =function(d, tau) exp(-d/tau),
      "hazrate"   =function(d, tau) 1-exp(-(d/tau)^-input$bpar)
    )
  })
  o <- reactive({
    bsims_detect(b(),
      xy = c(0, 0),
      tau = c(input$tauV, input$tauM),
      dist_fun = dfun(),
      event_type = input$event,
      direction=input$dir_sens != 1,
      sensitivity=input$dir_sens)
  })
  m <- reactive({
    pr <- if (!input$oucount)
      NULL else input$percept
    bsims_transcribe(o(),
      tint = TINT[[input$tint]],
      rint = RINT[[input$rint]],
      error = input$derr,
      bias = input$dbias,
      condition = input$condition,
      event_type = input$event,
      perception = pr
    )
  })
  e <- reactive({
    REM <- get_table(m())
    MaxDur <- max(TINT[[input$tint]])
    MaxDis <- max(RINT[[input$rint]])
    Ydur <- matrix(colSums(REM), 1)
    Ddur <- matrix(TINT[[input$tint]], 1)
    Ydis <- matrix(rowSums(REM), 1)
    Ddis <- matrix(RINT[[input$rint]], 1)
    if (length(TINT[[input$tint]]) > 1 && sum(REM) > 0) {
      Mrem <- try(cmulti.fit(Ydur, Ddur, type="rem"))
      if (!inherits(Mrem, "try-error")) {
        phi <- exp(Mrem$coef)
        p <- 1-exp(-MaxDur*phi)
      } else {
        Mrem <- NULL
        phi <- NA
        p <- NA
      }
    } else {
      Mrem <- NULL
      phi <- NA
      p <- NA
    }
    if (length(RINT[[input$rint]]) > 1 && sum(REM) > 0) {
      Mdis <- try(cmulti.fit(Ydis, Ddis, type="dis"))
      if (!inherits(Mdis, "try-error")) {
        tau <- exp(Mdis$coef)
        q <- if (is.infinite(MaxDis))
          1 else (tau^2/MaxDis^2) * (1-exp(-(MaxDis/tau)^2))
        A <- if (is.infinite(MaxDis))
          pi * tau^2 else pi * MaxDis^2
      } else {
        Mdis <- NULL
        tau <- NA
        q <- NA
        A <- NA
      }
    } else {
      Mdis <- NULL
      tau <- NA
      q <- NA
      A <- NA
    }
    D <- sum(REM) / (A * p * q)
    list(
      Ydur=Ydur, Ddur=Ddur,
      Ydis=Ydis, Ddis=Ddis,
      Mrem=Mrem,
      Mdis=Mdis,
      phi=phi, tau=tau,
      A=A, p=p, q=q,
      D=D)
  })
  getset <- reactive({
    xc <- function(x) paste0("c(", paste0(x, collapse=", "), ")")
    xq <- function(x) paste0("'", x, "'", collapse="")
    margin <- switch(input$spfun,
      "random"=0,
      "regular"=2,
      "clustered"=5)
    pr <- if (!input$oucount)
      "NULL" else input$percept
    paste0("bsims_all(",
    "\n  extent = ", EXTENT,
    ",\n  density = ", input$D,
    ",\n  xy_fun = ", paste0(deparse(xy_fun()), collapse=''),
    ",\n  margin = ", margin,
    ",\n  duration = ", DURATION,
    ",\n  vocal_rate = ", xc(c(input$phi1, input$phi2)),
    ",\n  move_rate = ", input$phim,
    ",\n  movement = ", input$SDm,
    ",\n  mixture = ", xc(c(input$mix, 1-input$mix)),
    ",\n  allow_overlap = ", input$overlap,
    ",\n  initial_location = ", input$init_loc,
    ",\n  tau = c(", input$tauV, ", ", input$tauM, ")",
    ",\n  dist_fun = ", paste0(deparse(dfun()), collapse=''),
    ",\n  xy = c(0, 0)",
    ",\n  direction = ", input$dir_sens != 1,
    ",\n  sensitivity = ", input$dir_sens,
    ",\n  event_type = ", xq(input$event),
    ",\n  tint = ", xc(TINT[[input$tint]]),
    ",\n  rint = ", xc(RINT[[input$rint]]),
    ",\n  error = ", input$derr,
    ",\n  bias = ", input$dbias,
    ",\n  condition = ", xq(input$condition),
    ",\n  perception = ", pr,
    ")", collapse="")
  })

  output$plot_ini <- renderPlot({
    op <- par(mar=c(0,0,0,0))
    plot(l())
    par(op)
  })
  output$plot_pop <- renderPlot({
    op <- par(mar=c(0,0,0,0))
    plot(a())
    par(op)
  })
  output$plot_spfun <- renderPlot({
    plot(dis, xy_fun()(dis), type="l", col=4,
      ylim=c(0,1), xlab="Distance", ylab="P(acceptance)")
  })
  output$plot_ani <- renderPlot({
    op <- par(mar=c(0,0,0,0))
    plot(b(), event_type=input$event)
    if (input$show_tess && !is.null(b()$tess))
      plot(b()$tess, add=TRUE, wlines="tess",
        showpoints=FALSE, cmpnt_col="grey", cmpnt_lty=1)
    par(op)
  })
  output$plot_det <- renderPlot({
    op <- par(mar=c(0,0,0,0))
    plot(o(),
      event_type=input$event,
      condition=input$condition)
    par(op)
  })
  output$plot_dfun <- renderPlot({
    plot(dis, dfun()(dis, input$tauV), type="l", col="black",
      ylim=c(0,1), xlab="Distance", ylab="P(detection)")
    lines(dis, dfun()(dis, input$tauM), col="purple")
    legend("top", horiz=TRUE, bty="n", lty=1, col=c("black","purple"),
      legend=c("vocalization", "movement"))
  })
  output$plot_tra <- renderPlot({
    op <- par(mar=c(0,0,0,0))
    plot(m())
    par(op)
  })
  output$table_rem <- renderTable({
    tab <- get_table(m())
    tab <- cbind(tab, Total=rowSums(tab))
    tab <- rbind(tab, Total=colSums(tab))
    tab
  }, rownames = TRUE, colnames = TRUE, digits = 0)
  output$plot_est <- renderPlot({
    v <- e()
    col <- c("#ffe042", "#e71989")
    op <- par(mfrow=c(1,3))
    barplot(c(True=input$phi1, Estimate=v$phi),
      col=col, main=expression(phi))
    Taus <- switch(input$event,
      "vocal"=c(True=input$tauV, Estimate=v$tau),
      "move"=c(True=input$tauM, Estimate=v$tau),
      "both"=c(TrueV=input$tauV, TrueM=input$tauM, Estimate=v$tau))
    TauCol <- switch(input$event,
      "vocal"=col[c(1,2)],
      "move"=col[c(1,2)],
      "both"=col[c(1,1,2)])
    barplot(Taus,
      col=TauCol, main=expression(tau))
    barplot(c(True=input$D, Estimate=v$D),
      col=col, main=expression(D))
    par(op)
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
