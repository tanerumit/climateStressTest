
source("global.R")

# UI-SIDE ######################################################################

ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  titlePanel("Climate Response Surface"),
  br(),
  column(width = 5,
    tabsetPanel(
      tabPanel("1.Data",
        br(),
        fileInput("datafile", "Choose CSV File", multiple = F, accept = ".csv"),
        uiOutput('xvarInput'),
        uiOutput('yvarInput'),
        uiOutput('zvarInput'),
        br(),
        uiOutput('button1')
      ),
      tabPanel("2.Plot",
        br(),
        textInput("xlab", label = "X-axis label"),
        textInput("ylab", label = "Y-axis label"),
        textInput("zlab", label = "Z-axis label"),
        textInput("plot.title", label = "Enter Plot title"),
        textInput("plot.subtitle", label = "Enter Plot subtitle"),
        br(),
        uiOutput('resolutionInput'),
        uiOutput('zthresholdInput'),
        uiOutput('color.lowInput'),
        uiOutput('color.highInput')
      ),
      tabPanel("3.Climate Information",
               br(),
               textInput("test2", label = "X-axis label")
      ),
      tabPanel("4.Fine Tuning",
               br(),
               textInput("test1", label = "X-axis label")
      )
    ) # tabsetPanel close
  ), # column close
  column(width = 7,
       br(), br(), br(),
       climateResponseSurface_mod_UI("Surface1", height = "400px", width = "500px") %>% withSpinner()
       #div(class = "square",)
  ) # column close
) # fluidPage close



################################################################################
### SERVER SIDE ----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues()

  #### Data Tab
  observe({

    req(input$datafile)

    rv$dataFile <- read.csv(input$datafile$datapath, header = TRUE, sep = ",",
      stringsAsFactors = TRUE, row.names = NULL)

    output$xvarInput = renderUI({
      selectInput("xvar", label = "X-axis variable", choices = colnames(rv$dataFile))
    })

    output$yvarInput = renderUI({
      selectInput("yvar", label = "Y-axis variable",
                  choices = colnames(rv$dataFile)[! colnames(rv$dataFile)  %in% input$xvar])
    })

    output$zvarInput = renderUI({
      selectInput("zvar", label = "Z-axis variable",
                  choices = colnames(rv$dataFile)[! colnames(rv$dataFile)  %in% c(input$xvar, input$yvar)])
    })

    output$button1 = renderUI({
      actionButton("action1", label = "Submit")
    })

    #PLOTTING TOOLS ####


    output$resolutionInput = renderUI({
      sliderInput("resolution", label = "Plot resolution", min = 100, max = 500, value = 100, step = 100)
    })

    output$zthresholdInput = renderUI({
      numericInput("zthreshold", label = "Acceptable level", value  = 50)
    })

    output$color.lowInput = renderUI({
        selectInput("color.low", label = "low perf color", choices = c("Red" = "red", "Blue" = "blue"), selected  = "red")
    })

    output$color.highInput = renderUI({
      selectInput("color.high", label = "high perf color", choices = c("Red" = "red", "Blue" = "blue"), selected = "blue")
    })


  }) # observe close

  rv$xvar <- reactive(input$xvar)
  rv$yvar <- reactive(input$yvar)
  rv$zvar <- reactive(input$zvar)
  rv$xlab <- reactive(input$xlab)
  rv$ylab <- reactive(input$ylab)
  rv$zlab <- reactive(input$zlab)
  rv$resolution <- reactive(input$resolution)
  rv$zthreshold <- reactive(input$zthreshold)
  rv$color.low  <- reactive(input$color.low)
  rv$color.high <- reactive(input$color.high)

  rv$plot.title  <- reactive(input$plot.title)
  rv$plot.subtitle <- reactive(input$plot.subtitle)

  observeEvent(input$action1, {

    climateResponseSurface_mod_Server(id = "Surface1", grid.data = rv$dataFile, plot.title = rv$plot.title, plot.subtitle = rv$plot.subtitle,
                                      variable.x = rv$xvar, label.x = rv$xlab, variable.y = rv$yvar, label.y = rv$ylab,
                                      variable.z = rv$zvar, label.z = rv$zlab, plot.resolution = rv$resolution,
                                      threshold = rv$zthreshold, color.low = rv$color.low, color.high = rv$color.high)
  })


}


shinyApp(ui, server)


