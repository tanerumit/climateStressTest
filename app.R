
source("./src/global.R")
source("./src/functions.R")



### UI-SIDE --------------------------------------------------------------------

#### Dashboard sidebar elements

sidebar = dashboardSidebar(
  width = 400,
  useShinyjs(),
  includeCSS("www/style.css"),
  sidebarMenu(
    br(),
    #uiOutput('demoButtonUI'),
    convertMenuItem(menuItem("About", tabName="Page1", selected = TRUE), tabName="Page1"),
    convertMenuItem(menuItem("1. Create Response Surface", tabName="Page2", icon=NULL,
      useShinyjs(),
      uiOutput('stressTestDataUploadUI'),
      #bs_modal(id = "modal1", title = "Stress test data", body = DTOutput('stressTestDataTbl')),
      #bs_attach_modal(uiOutput("stressTestDataTblBttnUI"), id_modal = "modal1"),
      uiOutput('variable.xUI'),
      uiOutput('variable.yUI'),
      uiOutput('variable.zUI'),
      uiOutput('pthresholdUI')
    ),tabName="Page2"),
    convertMenuItem(menuItem("2. Overlay Climate Information", tabName="Page2",
      #uiOutput('GCMDataTblBttn_DefaultUI'),
      uiOutput('GCMDataUI'),
      #bs_modal(id = "modal2", title = "GCM projections", body = DTOutput('GCMDataTbl')),
      #bs_attach_modal(uiOutput("GCMDataTblBttnUI"), id_modal = "modal2"),
      uiOutput('scenariosUI'),
      uiOutput('modelsUI'),
      uiOutput('gcm.marginalUI')
    ),tabName="Page2"),
    convertMenuItem(menuItem("3. Adjust scales", tabName="Page2", icon=NULL,
      uiOutput('variable.z.minUI'),
      uiOutput('variable.z.maxUI'),
      uiOutput('variable.z.binUI'),
      uiOutput("plot.colorsUI"),
      uiOutput("plot.legendUI")
      ),tabName="Page2"),
    convertMenuItem(menuItem("4. Labels", tabName="Page2", icon=NULL,
       uiOutput('plot.titleUI'),
       uiOutput('variable.x.labelUI'),
       uiOutput('variable.y.labelUI'),
       uiOutput('variable.z.labelUI')
    ),tabName="Page2")
  ), # sidebarMenu close
  tags$style(type = 'text/css',
             "footer{position: absolute; bottom:2%; left: 5%; padding:6px; color:gray}"),
  HTML('<footer> <a href="mailto:umit.taner@deltares.nl">Contact</a> </footer>')

  )#sidebar close

#### Dashboard body elements
body <- dashboardBody(
  customOneNote,
  useShinyjs(),
  tabItems(
    tabItem(
      tabName = "Page1",
      includeHTML("www/About.html")
    ),
    tabItem(
      tabName = "Page2",
      fluidPage(
        br(), br(),
        column(12, align="center",
           #plotlyOutput("SurfacePlotUI", height = "600px", width = "750px") %>% withSpinner(),
           plotOutput("SurfacePlotUI", height = "600px", width = "700px") %>% withSpinner(),
           br(),
           uiOutput("downloadPlotUI")
        )
      ) #fluidpage close
    ) #tabitem close
  ) #tabitems close
) # body close

loadingState()

####  Define the dashboard
appUI <-  dashboardPage(
  header  = dashboardHeader(
    title = "Climate Surface Viz Tool",
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



  ## Stress test results upload (UI element)
  output$stressTestDataUploadUI = renderUI({
     fileInput("stressTestDataUpload",
       label = "Upload stress test datafile (csv)", multiple = F, accept = ".csv", width = '95%')
   })

  # ### Stress Test Data (user interactive)
  # stressTestData <- eventReactive(input$demoButton, {
  #     read.csv("./data/stress_test_sample_data_default.csv", header = T, sep = ",", stringsAsFactors = T, row.names = NULL)
  #   })

  stressTestData <- reactive({

    req(input$stressTestDataUpload)
      #if (!is.null(input$stressTestDataUpload)) {
        read.csv(input$stressTestDataUpload$datapath, header = T, sep = ",", stringsAsFactors = T, row.names = NULL)
      #}
  #  }
  })


  output$demoButtonUI  = renderUI({
    actionBttn(
      inputId = "demoButton",
      label = "Live Demo",
      style = "material-flat",
      size = "xs",
      color = "default"
    )
  })

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
    pickerInput("variable.x", label = "X-Axis variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[1], width = '95%')

  })
  output$variable.yUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.y", label = "Y-Axis variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[2], width = '95%')
  })
  output$variable.zUI  = renderUI({
    req(stressTestData())
    pickerInput("variable.z", label = "Z-Axis: variable", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[3], width = '95%')
  })
  output$plot.titleUI  = renderUI({
    req(stressTestData())
    textInput("plot.title", label = "Plot title", width = '95%')
  })
  output$variable.x.labelUI  = renderUI({
    req(stressTestData())
    textInput("variable.x.label", label = "X-label", width = '95%')
  })
  output$variable.y.labelUI  = renderUI({
    req(stressTestData())
    textInput("variable.y.label", label = "Y-label", width = '95%')
  })
  output$variable.z.labelUI  = renderUI({
    req(stressTestData())
    textInput("variable.z.label", label = "Z-label", width = '95%')
  })

  output$pthresholdUI     = renderUI({
    req(stressTestData())
    sliderInput(inputId = "pthreshold",
                label = "Performance Threshold",
                ticks = FALSE,
                step  = NULL, #this needs to be fixed
                min   = stressTestData() %>% pull(input$variable.z) %>% min()  %>% floor(),
                max   = stressTestData() %>% pull(input$variable.z) %>% max()  %>% ceiling(),
                value = 35,
                round = 0,
                width = '95%'
    )
  })

  #### CHeck Boxes
  output$plot.colorsUI    = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.colors", label="Flip colors", value = FALSE)
  })
  output$plot.legendUI    = renderUI({
    req(stressTestData())
    awesomeCheckbox("plot.legend", label= "Hide legend", value = FALSE)
  })

  output$variable.z.minUI  = renderUI({
    req(stressTestData())
    numericInput("variable.z.min", label = "Min. value", width = '95%',
                 value = stressTestData() %>% pull(input$variable.z) %>% min() %>% floor())
  })

  output$variable.z.maxUI  = renderUI({
    req(stressTestData())
    numericInput("variable.z.max", label = "Max. value", width = '95%',
                 value = stressTestData() %>% pull(input$variable.z) %>% max() %>% ceiling())
  })

  output$variable.z.binUI  = renderUI({
    req(stressTestData())
    numericInput("variable.z.bin", label = "Bin num.", width = '95%', value = 20)
  })


  color.low  <- col1
  color.high <- col2

  SurfacePlot <- reactive({

    req(input$pthreshold, input$variable.x, input$variable.y, input$variable.z)

    # Set color scheme
    if(!is.null(input$plot.colors)) {
      if(input$plot.colors == TRUE) {
        color.high <- col1; color.low  <- col2
      } else {
        color.high <- col2; color.low  <- col1
      }
    }

    # Extract x, y, and z dimensions from the data matrix
    x_data <- stressTestData() %>% pull(input$variable.x)
    y_data <- stressTestData() %>% pull(input$variable.y)
    z_data <- stressTestData() %>% pull(input$variable.z)

    z_mid  <- input$pthreshold
    z_min  <- min(z_data) %>% floor()
    z_max  <- max(z_data) %>% ceiling()
    z_bin  <- 20


    if(!is.null(input$variable.z.min)) {
      z_min  <- input$variable.z.min
    }
    if(!is.null(input$variable.z.max)) {
      z_max  <- input$variable.z.max
    }
    if(!is.null(input$variable.z.bin)) {
      z_bin  <- input$variable.z.bin
    }


    # Specify x, y breaks
    x_breaks  <- unique(x_data)
    y_breaks  <- unique(y_data)
    z_breaks  <- round(seq(z_min,z_max,length.out = z_bin))
    z_breaks_legend <- pretty(range(z_breaks),9)

    df = tibble(x = x_data, y = y_data, z = z_data)

    # Core climate response surface
    p <- ggplot(df, aes(x = x, y = y)) +
      # Define theme
      theme_light(base_size = 14) +
      # Place z dimension
      geom_contour_fill(aes(z=z), breaks = z_breaks) +
      # Set scales
      scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
      scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
      scale_fill_divergent(low = color.low, mid = "white", high = color.high, midpoint = z_mid,
         limits = range(z_breaks), breaks = z_breaks,
         guide = guide_coloursteps(barwidth=1.5, show.limits=TRUE, ticks = TRUE, barheight = 20, order = 1, draw.ulim = FALSE)) +
      # Set labs
      labs(x = input$variable.x.label,y = input$variable.y.label,color = NULL, fill = input$variable.z.label,title = input$plot.title) +
      # Set guides
      guides(color = guide_legend(order = 2)) +
      #guides(fill = guide_coloursteps(barwidth=1.5, show.limits, ticks = TRUE, barheight = 20, order = 1),
      #       color = guide_legend(order = 2)) +
      # Threshold line
      geom_contour(aes(z = z), breaks = z_mid, color = "black", size = 1.5)

    if(!is.null(input$plot.legend)) {
      if(input$plot.legend == TRUE) {
        p <- p + theme(legend.position = "none")
      }
    }

    # GCM Dots
    if(!is.null(GCMDataDF())) {

      # Find positions of scenarios
      scenario_ind <- which(scenarios_list() %in% unique(GCMDataDF()$scenario))
      scenario_col <- rcp_col[scenario_ind]

      #if(input$GCM.scenarios == TRUE) {
      p <- p + scale_color_manual(values = scenario_col) +
        geom_point(aes(color = scenario), data = GCMDataDF(), shape = 1, stroke = 2, size = 3, alpha = 0.75)

    }

    if(!is.null(input$gcm.marginal)) {

      if(input$gcm.marginal == TRUE) {

        p_top  <- ggplot(GCMDataDF(), aes(x))+
          scale_fill_manual(values = scenario_col) +
          scale_color_manual(values = scenario_col) +
          geom_density(aes(fill = scenario, color = scenario), alpha = 0.4, position="identity") +
          scale_x_continuous(expand = c(0, 0), limits = c(0,5)) +
          gg_blank + guides(fill=FALSE, color = FALSE) #+
          #theme(legend.position  = "none") #, panel.border = element_rect(color = "gray80", fill=NA))

        p_left  <- ggplot(GCMDataDF(), aes(y))+
          scale_fill_manual(values = scenario_col) +
          scale_color_manual(values = scenario_col) +
          geom_density(aes(fill = scenario, color = scenario), alpha = 0.4, position="identity") +
          scale_x_continuous(expand = c(0, 0), limits = c(-60,40)) +
          coord_flip() + gg_blank + guides(fill=FALSE, color = FALSE)
          #theme(legend.position  = "none") #, panel.border = element_rect(color = "gray80", fill=NA))

        p <- ggarrange(empty, p_top, p_left, p, ncol=2, nrow=2, widths=c(1, 7), heights=c(1, 7))
        #p <- subplot(empty, p_top, p_left, p, nrows=2, widths=c(0.15, 0.85), heights=c(0.15, 0.85),
        #             shareX = TRUE, shareY = TRUE)
      }
    }

    p #+ theme(plot.margin=grid::unit(c(1,1,1,1), "mm"))

  })


  #output$SurfacePlotUI <- renderPlotly(SurfacePlot())
  output$SurfacePlotUI <- renderPlot(SurfacePlot())

### GCM PROJECTIONS TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$GCMDataUI = renderUI({
    req(!is.null(stressTestData()))
    fileInput("GCMDataUpload", label = "Upload climate projections datafile (csv)", multiple = F, accept = ".csv", width = '95%')
  })


  GCMData <- reactive({

    if (!is.null(input$GCMDataUpload)) {
      read.csv(input$GCMDataUpload$datapath, header = TRUE, sep = ",", stringsAsFactors = T, row.names = NULL)
    } else {
      #if(input$GCMDataTblBttn_Default == T) {
        read.csv("./data/interpret_climate_information.csv", header = T, sep = ",", stringsAsFactors = T, row.names = NULL)
      #}
    }
  })

  GCMDataDF <- reactive({

    # Extract x, y, and z dimensions from the data matrix
    df   <- GCMData() %>% select(scenario, model)
    df$x <- GCMData() %>% pull(input$variable.x)
    df$y <- GCMData() %>% pull(input$variable.y)

    df %<>%
      filter(scenario %in% input$scenarios) %>%
      filter(model %in% input$models)

  })
  scenarios_list <- reactive({

    dat <- GCMData() %>% pull(scenario) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })
  output$scenariosUI  <- renderUI({

    req(input$GCMDataUpload)

      pickerInput(
        inputId = "scenarios",
        label = "Scenarios",
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
  output$modelsUI     <- renderUI({

    req(input$GCMDataUpload)

    pickerInput(
      inputId = "models",
      label = "Climate Models",
      choices = models_list(),
      selected = models_list(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      width = '95%'
    )
  })
  output$gcm.marginalUI = renderUI({
    req(input$GCMDataUpload)
    awesomeCheckbox("gcm.marginal", label= "Display marginal distributions", value = FALSE)
  })



  # #### Dowload response surface
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
      label = "Download plot",
      style = "material-flat",
      color = "default",
      size = "sm"
    )
  })


  session$onSessionEnded(stopApp)

}



################################################################################


### APPLICATON -----------------------------------------------------------------

shinyApp(ui = appUI, server = appServer)
