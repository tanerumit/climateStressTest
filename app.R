
source("./src/global.R")
source("./src/functions.R")
source("./src/createResponseSurface.R")

### UI-SIDE --------------------------------------------------------------------

#### Dashboard sidebar elements

sidebar = dashboardSidebar(
  width = 400,
  useShinyjs(),
  includeCSS("www/style.css"),
  sidebarMenu(
    useShinyjs(),
    br(),
    tabsetPanel(type = "tabs",
      tabPanel("1. Data",
        br(),
        uiOutput('stressTestDataUploadUI'),
        uiOutput('variable.xUI'),
        uiOutput('variable.yUI'),
        uiOutput('variable.zUI'),
        uiOutput('location.zUI'),
        br(),
        uiOutput('pthresholdUI'),
        br(),
        uiOutput("plot.colorsUI"),
        uiOutput("plot.flip.axesUI")
      ),
      tabPanel("2.GCMS",
        br(),
        uiOutput('GCMDataUI'),
        uiOutput('scenariosUI'),
        uiOutput('modelsUI'),
        uiOutput('gcm.marginalUI')
      ),
      tabPanel("3.Fine Tuning",
        br(),
        h4(HTML("Set Legend features")),
        uiOutput('variable.z.minUI'),
        uiOutput('variable.z.maxUI'),
        uiOutput('variable.z.binUI'),
        br(),
        h4(HTML("Set Labels")),
        uiOutput('plot.titleUI')

      )
    ), #tabset close
  tags$style(type = 'text/css',
             "footer{position: absolute; bottom:2%; left: 5%; padding:6px; color:gray}"),
  HTML('<footer> <a href="mailto:umit.taner@deltares.nl">Contact</a> </footer>')
  )
) #sidebar close

#### Dashboard body elements
body <- dashboardBody(
  #customOneNote,
  useShinyjs(),
  fluidPage(
  column(12, align="center",
    br(),
    plotOutput("SurfacePlotUI", height = "700px", width = "800px") %>% withSpinner(),
    uiOutput("downloadPlotUI"))

  # tabItems(
  #   tabItem(
  #     tabName = "Page1",
  #     includeHTML("www/About.html")
  #   ),
  #   tabItem(
  #     tabName = "Page2",
  #     fluidPage(
  #       column(12, align="center",
  #          #plotlyOutput("SurfacePlotUI", height = "600px", width = "750px") %>% withSpinner(),
  #          plotOutput("SurfacePlotUI", height = "600px", width = "700px") %>% withSpinner(),
  #          br(),
  #          uiOutput("downloadPlotUI")

      ) #fluidpage close
   # ) #tabitem close
  #) #tabitems close
) # body close

loadingState()

####  Define the dashboard
appUI <-  dashboardPage(
  skin = "blue",
  header  = dashboardHeader(
    title = "Stress Test Results Explorer",
    titleWidth = 360),
  sidebar = sidebar,
  body    = body
)




################################################################################


### SERVER-SIDE ----------------------------------------------------------------

appServer <- function(input, output, session) {


  RV <- reactiveValues(gcm.marginal = FALSE, GCM_DF = NULL, z_range = NULL)

  z_bin_default <- 15

  ## Stress test results upload (UI element)
  output$stressTestDataUploadUI = renderUI({
     fileInput("stressTestDataUpload",
       label = "Upload stress test datafile (csv)", multiple = F, accept = ".csv",
       width = '95%')
   })

  stressTestData <- reactive({

    if(!is.null(input$stressTestDataUpload))
        read.csv(input$stressTestDataUpload$datapath, header = T, sep = ",",
          stringsAsFactors = T, row.names = NULL)
  #  else {
  #     str_data_default
  #  }
  })



### RENDER CLIMATE RESPONSE SURFACE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$variable.xUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.x", label = "Temperature variable:",
                choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[2],
                width = '95%')

  })
  output$variable.yUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.y", label = "Precipitation variable:",
                choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[3],
                width = '95%')
  })
  output$variable.zUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.z", label = "Response variable:",
                choices = unique(stressTestData()[['statistic']]),
                selected = unique(stressTestData()[['statistic']])[[1]],
                width = '95%')
  })

  output$location.zUI  = renderUI({
    req(stressTestData())
    pickerInput("location.z", label = "Location",
                choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[4], width = '95%')
  })

   output$plot.titleUI  = renderUI({
     req(stressTestData())
     textInput("plot.title", label = "Plot title", width = '95%')
   })
  # output$variable.x.labelUI  = renderUI({
  #   req(stressTestData())
  #   textInput("variable.x.label", label = "X-label", width = '95%')
  # })
  # output$variable.y.labelUI  = renderUI({
  #   req(stressTestData())
  #   textInput("variable.y.label", label = "Y-label", width = '95%')
  # })
  # output$variable.z.labelUI  = renderUI({
  #   req(stressTestData())
  #   textInput("variable.z.label", label = "Z-label", width = '95%')
  # })

  output$pthresholdUI = renderUI({

    req(stressTestData())

    val_init <- stressTestData() %>%
      filter(statistic == input$variable.z) %>%
      filter(prcp == 0) %>%
      filter(tavg == 0) %>%
      pull(input$location.z)

    sliderInput(inputId = "pthreshold",
                label = "Performance Threshold",
                ticks = FALSE,
                step  = NULL, #this needs to be fixed
                min   = z_range()[[1]],
                max   = z_range()[[2]],
                value = val_init,
                width = '95%'
    )
  })

  # #### CHeck Boxes
  output$plot.colorsUI = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.colors", label="Flip color scale", value = FALSE)
  })

  output$plot.flip.axesUI = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.flip.axes", label="Flip axes", value = FALSE)
  })


  # output$plot.legendUI    = renderUI({
  #   req(stressTestData())
  #   awesomeCheckbox("plot.legend", label= "Hide legend", value = FALSE)
  # })
  #
  output$variable.z.minUI  = renderUI({
     req(stressTestData())
     numericInput("variable.z.min", label = "Minimum value", width = '95%',
                  value = z_range()[[1]], step = 10^floor(log10(z_range()[[1]])))

  })

  output$variable.z.maxUI  = renderUI({
     req(stressTestData())
     numericInput("variable.z.max", label = "Maximum value", width = '95%',
                  value = z_range()[[2]], step = 10^floor(log10(z_range()[[1]])))
  })

  output$variable.z.binUI  = renderUI({
     req(stressTestData())
     numericInput("variable.z.bin", label = "Number of bins", width = '95%', value = z_bin_default)
  })



################################################################################

  # Reactive values from stress test data
  z_range <- reactive({
    req(stressTestData())
    stressTestData() %>% filter(statistic == input$variable.z) %>% pull(input$location.z) %>% range()
  })

  observeEvent(input$gcm.marginal, {RV$gcm.marginal <- input$gcm.marginal})
  observeEvent(GCMDataDF(), {RV$GCM_DF <- GCMDataDF()})




  # Reactive climate surface plot
  SurfacePlot <- reactive({

    req(input$pthreshold,
        input$variable.x,
        input$variable.y,
        input$variable.z,
        input$location.z,
        stressTestData())

    #Set color scheme
    if(!is.null(input$plot.colors)) {
      if(input$plot.colors == TRUE) {
        color.high <- "#FF0000"; color.low  <- "royalblue3"
      } else {
        color.high <- "royalblue3"; color.low  <- "#FF0000"
      }
    }

    str_data <- stressTestData() %>%
      filter(statistic == input$variable.z) %>%
      select(-statistic)

    z_min <-  if(!is.null(input$variable.z.min)) input$variable.z.min else z_range()[[1]]
    z_max <-  if(!is.null(input$variable.z.max)) input$variable.z.max else z_range()[[2]]
    z_bin <-  if(!is.null(input$variable.z.bin)) input$variable.z.bin else z_bin_default

    p <- createSurfacePlot(
      str.data = str_data,
      gcm.data = RV$GCM_DF,
      variable.x = input$variable.x,
      variable.y = input$variable.y,
      variable.z = input$location.z,
      threshold.z = input$pthreshold,
      color.low  = color.low,
      color.high = color.high,
      variable.x.label = expression(Delta~"Mean Temperature " (degree*C)),
      variable.y.label = expression(Delta~"Mean Precipitation (%)") ,
      variable.z.label = NULL,
      plot.title = input$plot.title,
      scenarios_list = c("rcp26", "rcp45", "rcp60", "rcp85"),
      plot.gcm.marginal.dist = RV$gcm.marginal,
      z_min = z_min,
      z_max = z_max,
      z_bin = z_bin,
      z_min_legend = NULL,
      z_max_legend = NULL,
      z_bin_legend = 7)

    #Flip axes
    if(!is.null(input$plot.flip.axes)) {
      if(input$plot.flip.axes == FALSE) {
        p
      } else {
        p + coord_flip()
      }
    }

  })

  output$SurfacePlotUI <- renderPlot(SurfacePlot())

### GCM PROJECTIONS TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$GCMDataUI = renderUI({
    req(!is.null(stressTestData()))
    fileInput("GCMDataUpload", label = "Upload climate projections datafile (csv)",
      multiple = F, accept = ".csv", width = '95%')
  })

  GCMData <- reactive({

    if(!is.null(input$GCMDataUpload)) #{

      read.csv(input$GCMDataUpload$datapath, header = TRUE, sep = ",",
        stringsAsFactors = T, row.names = NULL) %>%
      filter(horizon == "near")

  })

  GCMDataDF <- reactive({

    req(input$GCMDataUpload)

      # Extract x, y, and z dimensions from the data matrix
      df <- GCMData() %>% select(scenario, model)
      df[[input$variable.x]] <- GCMData() %>% pull(input$variable.x)
      df[[input$variable.y]] <- GCMData() %>% pull(input$variable.y)

      df %<>% filter(scenario %in% input$scenarios) %>%
        filter(model %in% input$models)

    #}

  })

  scenarios_list <- reactive({

    req(input$GCMDataUpload)

    dat <- GCMData() %>% pull(scenario) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })


  output$scenariosUI  <- renderUI({

    req(input$GCMDataUpload)

      pickerInput(
        inputId = "scenarios",
        label = "Climate scenarios",
        choices = scenarios_list(),
        selected = scenarios_list(),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        width = '95%'
      )
  })

  models_list  <- reactive({

    req(input$GCMDataUpload)

    dat <- GCMData() %>% pull(model) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })
  output$modelsUI <- renderUI({

    req(input$GCMDataUpload)

    pickerInput(
      inputId = "models",
      label = "Global circulation models (GCMs)",
      choices = models_list(),
      selected = models_list(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      width = '95%'
    )
  })
  output$gcm.marginalUI = renderUI({
    awesomeCheckbox("gcm.marginal", label= "Display marginal distributions", value = FALSE)
  })

  # # #### Dowload response surface
  #plot_name <- reactive({ifelse(is.null(input$plot.title), "surfaceplot", input$plot.title)})

  output$downloadPlot <- downloadHandler(
    filename = function() {paste("crs_",input$location.z, "_", input$variable.z,'.png',sep='')},
    content  = function(file){
      ggsave(file, plot = SurfacePlot(), height = 8, width = 10, units = "in")
    }
  )

  output$downloadPlotUI  = renderUI({

    req(SurfacePlot())

    downloadBttn(
      outputId = "downloadPlot",
      label = "Download",
      style = "material-flat",
      color = "default",
      size = "xs"
    )
  })

  session$onSessionEnded(stopApp)
}



################################################################################


### APPLICATON -----------------------------------------------------------------

shinyApp(ui = appUI, server = appServer)
