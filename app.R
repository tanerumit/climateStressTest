
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
      tabPanel("1. Stress Test Results",
        br(),
        uiOutput('stressTestDataUploadUI'),
        uiOutput('variable.xUI'),
        uiOutput('variable.yUI'),
        uiOutput('variable.zUI'),
        br(),
        uiOutput('pthresholdUI'),
        br(),
        uiOutput("plot.colorsUI"),
        uiOutput("plot.flip.axesUI")
        #uiOutput('plot.titleUI')
      ),
      tabPanel("2.Climate Information",
        br(),
        uiOutput('GCMDataUI'),
        uiOutput('scenariosUI'),
        uiOutput('modelsUI'),
        uiOutput('gcm.marginalUI')
      )
    ), #tabset close

    # #uiOutput('demoButtonUI'),
    # #convertMenuItem(menuItem("About", tabName="Page1", selected = TRUE), tabName="Page1"),
    # convertMenuItem(menuItem("1. Create Response Surface", tabName="Page2",
    #     icon=NULL, selected = TRUE, startExpanded = TRUE,
    #   useShinyjs(),
    #   uiOutput('stressTestDataUploadUI'),
    #   #bs_modal(id = "modal1", title = "Stress test data", body = DTOutput('stressTestDataTbl')),
    #   #bs_attach_modal(uiOutput("stressTestDataTblBttnUI"), id_modal = "modal1"),
    #   uiOutput('variable.xUI'),
    #   uiOutput('variable.yUI'),
    #   uiOutput('variable.zUI'),
    #   uiOutput('pthresholdUI')
    # ),tabName="Page2"),
    # convertMenuItem(menuItem("2. Overlay Climate Information", tabName="Page2",
    #   #uiOutput('GCMDataTblBttn_DefaultUI'),
    #   uiOutput('GCMDataUI'),
    #   #bs_modal(id = "modal2", title = "GCM projections", body = DTOutput('GCMDataTbl')),
    #   #bs_attach_modal(uiOutput("GCMDataTblBttnUI"), id_modal = "modal2"),
    #   uiOutput('scenariosUI'),
    #   uiOutput('modelsUI'),
    #   uiOutput('gcm.marginalUI')
    # ),tabName="Page2") #,
    # convertMenuItem(menuItem("3. Adjust scales", tabName="Page2", icon=NULL,
    #   uiOutput('variable.z.minUI'),
    #   uiOutput('variable.z.maxUI'),
    #   uiOutput('variable.z.binUI'),
    #   uiOutput("plot.colorsUI"),
    #   uiOutput("plot.legendUI")
    #   ),tabName="Page2"),
    # convertMenuItem(menuItem("4. Labels", tabName="Page2", icon=NULL,
    #    uiOutput('plot.titleUI'),
    #    uiOutput('variable.x.labelUI'),
    #    uiOutput('variable.y.labelUI'),
    #    uiOutput('variable.z.labelUI')
    # ),tabName="Page2")
  #), # sidebarMenu close
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

#demoButtonUI  /// default data button UI
#demoButton    /// default data button
#stressTestDataUpload   ///Upload data
#stressTestDataUploadUI ///Upload Buttion
#stressTestData()  //// processed stressTest dataframe

#stressTestDataTbl()
#stressTestDataTbl_button
#stressTestDataTblBttnUI


#GCMDataFileUpload
#GCMDataFileUploadUI
#GCMDataDF()

appServer <- function(input, output, session) {

  values <- reactiveValues(gcm.marginal = FALSE)

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
    else {
       str_data_default
    }
  })

#
#   output$demoButtonUI  = renderUI({
#     actionBttn(
#       inputId = "demoButton",
#       label = "Live Demo",
#       style = "material-flat",
#       size = "xs",
#       color = "default"
#     )
#   })

  # ### Stress Test Data results viewing
  # output$stressTestDataTbl = renderDataTable({
  #
  #   if (!is.null(stressTestData())) {
  #     datatable(stressTestData(),
  #         options = list(lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-left', targets = "_all"))),
  #         class = 'cell-border stripe') %>%
  #      formatRound(columns = sapply(stressTestData(), is.numeric), digits = 2) %>%
  #      formatRound(columns = 0, digits = 0)
  #   }
  # })
  # output$stressTestDataTblBttnUI  = renderUI({
  #   req(input$stressTestDataUpload)
  #   actionBttn(
  #     inputId = "stressTestDataTblBttn",
  #     label = "View Data",
  #     style = "material-flat",
  #     size = "xs",
  #     color = "default"
  #   )
  # })


### RENDER CLIMATE RESPONSE SURFACE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$variable.xUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.x", label = "Temperature variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[1], width = '95%')

  })
  output$variable.yUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.y", label = "Precipitation variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[2], width = '95%')
  })
  output$variable.zUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.z", label = "Metric variable:", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[3], width = '95%')
  })
  # output$plot.titleUI  = renderUI({
  #   req(stressTestData())
  #   textInput("plot.title", label = "Plot title", width = '95%')
  # })
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

  output$pthresholdUI     = renderUI({
    req(stressTestData())
    sliderInput(inputId = "pthreshold",
                label = "Performance metric threshold",
                ticks = FALSE,
                step  = NULL, #this needs to be fixed
                min   = stressTestData() %>% pull(input$variable.z) %>% min()  %>% floor(),
                max   = stressTestData() %>% pull(input$variable.z) %>% max()  %>% ceiling(),
                value = 35,
                round = 0,
                width = '95%'
    )
  })

  # #### CHeck Boxes
  output$plot.colorsUI = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.colors", label="Flip colors", value = FALSE)
  })

  output$plot.flip.axesUI = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.flip.axes", label="Flip x and y axes", value = FALSE)
  })


  # output$plot.legendUI    = renderUI({
  #   req(stressTestData())
  #   awesomeCheckbox("plot.legend", label= "Hide legend", value = FALSE)
  # })
  #
  # output$variable.z.minUI  = renderUI({
  #   req(stressTestData())
  #   numericInput("variable.z.min", label = "Min. value", width = '95%',
  #                value = stressTestData() %>% pull(input$variable.z) %>% min() %>% floor())
  # })
  #
  # output$variable.z.maxUI  = renderUI({
  #   req(stressTestData())
  #   numericInput("variable.z.max", label = "Max. value", width = '95%',
  #                value = stressTestData() %>% pull(input$variable.z) %>% max() %>% ceiling())
  # })
  #
  # output$variable.z.binUI  = renderUI({
  #   req(stressTestData())
  #   numericInput("variable.z.bin", label = "Bin num.", width = '95%', value = 20)
  # })

  observeEvent(input$gcm.marginal, {values$gcm.marginal <- input$gcm.marginal})


  SurfacePlot <- reactive({

    req(input$pthreshold,
        input$variable.x,
        input$variable.y,
        input$variable.z,
        stressTestData())

    #Set color scheme
    if(!is.null(input$plot.colors)) {
      if(input$plot.colors == TRUE) {
        color.high <- "#FF0000"; color.low  <- "green3"
      } else {
        color.high <- "green3"; color.low  <- "#FF0000"
      }
    }


    p <- createSurfacePlot(
      str.data = stressTestData(),
      gcm.data = GCMDataDF(),
      variable.x = input$variable.x,
      variable.y = input$variable.y,
      variable.z = input$variable.z,
      threshold.z = input$pthreshold,
      color.low  = color.low,
      color.high = color.high,
      variable.x.label = expression(Delta~" Annual Mean Temperature " ( degree*C)),
      variable.y.label = expression(Delta~" Annual Mean Precipitation (%)") ,
      variable.z.label = input$variable.z,
      plot.title = "",
      scenarios_list = c("rcp26", "rcp45", "rcp60", "rcp85"),
      plot.gcm.marginal.dist = values$gcm.marginal,
      z_bin = 11,
      z_min_legend = 10,
      z_max_legend = NULL)

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

    if(!is.null(input$GCMDataUpload)) {

      read.csv(input$GCMDataUpload$datapath, header = TRUE, sep = ",",
        stringsAsFactors = T, row.names = NULL)

    } else {

       gcm_data_default
    #   #if(input$GCMDataTblBttn_Default == T) {
    #     read.csv("./data/interpret_climate_information.csv", header = T, sep = ",",
    #       stringsAsFactors = T, row.names = NULL)
    #   #}
    }

  })

  GCMDataDF <- reactive({

    #if(is.null(input$GCMDataUpload)) {
    #  return(NULL)
    #} else {

      # Extract x, y, and z dimensions from the data matrix
      df <- GCMData() %>% select(scenario, model)
      df[[input$variable.x]] <- GCMData() %>% pull(input$variable.x)
      df[[input$variable.y]] <- GCMData() %>% pull(input$variable.y)

      df %<>% filter(scenario %in% input$scenarios) %>%
        filter(model %in% input$models)

    #}

  })

  scenarios_list <- reactive({

    dat <- GCMData() %>% pull(scenario) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })


  output$scenariosUI  <- renderUI({

    #req(input$GCMDataUpload)

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

    dat <- GCMData() %>% pull(model) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })
  output$modelsUI <- renderUI({

    #req(input$GCMDataUpload)

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
  plot_name <- reactive({ifelse(is.null(input$plot.title), "surfaceplot", input$plot.title)})

  output$downloadPlot <- downloadHandler(
    filename = function() {paste(plot_name(),'.png',sep='')},
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
