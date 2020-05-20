

################################################################################
### UI-SIDE --------------------------------------------------------------------



#### Define body
body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  fluidPage(
    tabBox(width = 5, height = "650px", selected = "2. Set Basic Layout",
           tabPanel(title = "1. About",
                    tags$div(style="margin-left:15px;",
                             h3(strong("Climate Response Surface Generator"))
                    ),
                    p("Description")
           ),
           tabPanel(title = "2. Base Plot",
                    useShinyjs(),
                    br(),
                    p("Upload the Stress test data and specify x, y, and z axes variables"),
                    fluidRow(
                    column(12, uiOutput('strTestDataUI'))
                    ),
                    fluidRow(
                      column(7, uiOutput('variable.xUI')),
                      column(5, uiOutput('variable.x.labelUI'))
                    ),
                    fluidRow(
                      column(7, uiOutput('variable.yUI')),
                      column(5, uiOutput('variable.y.labelUI'))
                    ),
                    fluidRow(
                      column(7, uiOutput('variable.zUI')),
                      column(5, uiOutput('variable.z.labelUI'))
                    ),
                    uiOutput("plot.titleUI"),
                    uiOutput('pthresholdUI'),
                    fluidRow(
                      column(6, uiOutput("plot.legendUI")),
                      column(6, uiOutput("plot.axis.intUI"))
                    ),
                    fluidRow(
                      column(6, uiOutput("plot.colorsUI")),
                      column(6, uiOutput("plot.resUI"))
                    ),



           ),
           tabPanel(title = "3. Climate Projections",
                    p("Description"),
                    uiOutput('GCMDataUI'),
                    uiOutput('scenariosUI'),
                    uiOutput('modelsUI')

           )
    ),
    box(title = "", height = "650px", width = 7, offset = 0,
        column(12, align="center",
               plotOutput("Surface1", height = "440px", width = "485px"),
               br(),
               uiOutput("downloadPlotUI")
        )

    ),
    tags$style(type = 'text/css',
               "footer{position: absolute; bottom:2%; right: 2%; padding:6px;}"),
    HTML('<footer> By Umit Taner. Free to use &copy; 2020 </footer>')
    #downloadButton('downloadPlot','Download Plot')
    #div(class = "square",)


  )
)

####  Define the dashboard
appUI <- dashboardPagePlus(
  header  = dashboardHeaderPlus(title = "Plot Climate Surface"),
  skin    = "black-light",
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body,
  title = "DashboardPage"
)

#    tags$style(type='text/css', "#stressTestDataUI {width:100%; margin-top: 25px;}"),
#    tags$style(type='text/css', "#button1  {width:100%; margin-top: 25px;}"),



################################################################################





################################################################################
### SERVER-SIDE ----------------------------------------------------------------


appServer <- function(input, output, session) {


### Stress Test Data Processing ###

  stressTestData <- reactive({

    req(input$strTestData)
    read.csv(input$strTestData$datapath, header = TRUE, sep = ",",
             stringsAsFactors = TRUE, row.names = NULL)
  })



### RENDER CLIMATE RESPONSE SURFACE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SurfacePlot <- reactive({

    # Set color scheme
    color.high <- "blue"
    color.low  <- "red"

    if(!is.null(input$plot.colors)) {

      if(input$plot.colors == TRUE) {
        color.high <- "red"; color.low  <- "blue"
      } else {
        color.high <- "blue"; color.low  <- "red"
      }
    }


    # Extract x, y, and z dimensions from the data matrix
    x_data <- stressTestData() %>% pull(input$variable.x)
    y_data <- stressTestData() %>% pull(input$variable.y)
    z_data <- stressTestData() %>% pull(input$variable.z)

    # Default bin number and mid.point
    z_bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
    z_mid  <- input$pthreshold

    # Specify x, y breaks
    x_breaks <- unique(x_data)
    y_breaks <- unique(y_data)

    #z-dimension parameters
    z_bins1 <- c(2*z_bins[1] - z_bins[2], z_bins, 2*z_bins[length(z_bins)] - z_bins[length(z_bins)-1])
    zcut  <- binCentered(z_bins1)
    variable.z.label  <- binCentered(zcut)

    # Prepare data matrix
    z_data[z_data < min(zcut)] <- zcut[1]
    z_data[z_data > max(zcut)] <- zcut[length(zcut)]

    res <- 100

    if(!is.null(input$plot.res)) {
      res <- ifelse(input$plot.res == TRUE, 300, 100)
    }

    df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = res) %>%
      mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = variable.z.label),
             z = as.numeric(as.character(z)))

    if(!is.null(input$plot.axis.int)) {

      if(input$plot.axis.int == TRUE) {
        x_breaks <- pretty(df$x, n = 7)
        y_breaks <- pretty(df$y, n = 7)
      }
    }

    # Prepare plot
    p <- ggplot(df, aes(x = x, y = y)) +

      theme_light(base_size = 12) +

      # Place z dimension
      geom_tile(aes(fill = z), color = NA) +

      # Set scales
      scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
      scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
      scale_fill_gradient2(low = color.low, mid = "white", high = color.high,
                           midpoint = z_mid, limits = range(z_bins), breaks = z_bins) +

      # Set labs
      labs(x = input$variable.x.label,
           y = input$variable.y.label,
           fill = input$variable.z.label,
           title = input$plot.title) +

      # Set guides
      guides(fill = guide_colorbar(nbin=length(z_bins), raster=F, barwidth=1,
                                   ticks = FALSE, barheight = 12)) #+

      # Equal size axes
      #coord_fixed(ratio = 1)

      if(!is.null(input$plot.legend)) {
        if(input$plot.legend == TRUE) {
          p <- p + theme(legend.position = "none")
        }
      }

      if(!is.null(input$GCMData)) {
        #if(input$GCM.scenarios == TRUE) {
          p <- p + geom_point(data = GCMDataDF())
        #}
      }

    p

  })

  output$Surface1 <- renderPlot({SurfacePlot()})

  plot_name <- reactive({ifelse(is.null(input$plot.title), "surfaceplot", input$plot.title)})

  output$downloadPlot <- downloadHandler(
    filename = function(){ paste(plot_name(),'.png',sep='')},
    content = function(file){
      ggsave(file, plot = SurfacePlot())
    }
  )

  output$downloadPlotUI  = renderUI({

    req(SurfacePlot())

    downloadBttn(
      outputId = "downloadPlot",
      label = "Download",
      style = "bordered",
      color = "success",
      size = "sm",
      block = FALSE,
      no_outline = TRUE
    )
  })



### GCM PROJECTIONS TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  scenarios_list <- c("RCP 2.6" = "rcp26", "RCP 4.5" = "rcp45", "RCP 6.0" = "rcp60", "RCP 8.5" = "rcp85")


  model_list <-  c("bcc-csm1-1", "bcc-csm1-1-m",   "CanESM2",
  "CCSM4",  "CESM1-CAM5", "CSIRO-Mk3-6-0",
  "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G",
  "GISS-E2-H",  "GISS-E2-R", "HadGEM2-AO", "HadGEM2-ES",
  "IPSL-CM5A-LR",   "IPSL-CM5A-MR", "MIROC-ESM",
  "MIROC-ESM-CHEM", "MIROC5", "MPI-ESM-LR", "MPI-ESM-MR",
  "MRI-CGCM3",  "NorESM1-M", "ACCESS1-0",
  "ACCESS1-3",  "CESM1-BGC", "CMCC-CM", "CMCC-CMS",
  "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-R-CC",
  "HadCM3",   "HadGEM2-CC", "inmcm4", "IPSL-CM5B-LR",
  "MIROC4h",  "EC-EARTH")

  output$scenariosUI = renderUI({

    req(input$GCMData)

    pickerInput(
      inputId = "scenarios",
      label = "Scenarios",
      choices = scenarios_list,
      selected = scenarios_list,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
})

  output$modelsUI = renderUI({

    req(input$GCMData)

    pickerInput(
      inputId = "models",
      label = "Climate Models",
      choices = model_list,
      selected = model_list,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })


### Climate Projections Data Processing ###

  GCMData <- reactive({

    req(input$GCMData)
    read.csv(input$GCMData$datapath, header = TRUE, sep = ",",
                     stringsAsFactors = TRUE, row.names = NULL)

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

### RENDER UI ELEMENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$strTestDataUI = renderUI({
    fileInput("strTestData", label = NULL, multiple = F, accept = ".csv", width = '90%')
  })

  output$variable.xUI  = renderUI({
    pickerInput("variable.x", label = "X-Axis variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[1], width = '100%')
  })
  output$variable.yUI  = renderUI({
    pickerInput("variable.y", label = "Y-Axis variable: ", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[2], width = '100%')
  })
  output$variable.zUI  = renderUI({
    pickerInput("variable.z", label = "Z-Axis: variable", choices = colnames(stressTestData()),
                selected = colnames(stressTestData())[3], width = '100%')
  })

  output$plot.resUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.res", label="High resolution", value = FALSE)
  })

  output$pthresholdUI  = renderUI({
    req(input$strTestData)
    sliderInput(inputId = "pthreshold",
                    label = "Threshold",
                    min   = stressTestData() %>% pull(input$variable.z) %>% min() %>% round(),
                    max   = stressTestData() %>% pull(input$variable.z) %>% max() %>% round(),
                    value = stressTestData() %>% pull(input$variable.z) %>% mean() %>% round(),
                    round = 0,
                    width = '90%'
    )
  })

  output$plot.colorsUI   = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.colors", label="Reversed color scale", value = FALSE)
  })

  output$plot.legendUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.legend", label= "Hide plot legend", value = FALSE)
  })
  output$plot.axis.intUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.axis.int", label= "Override axis intervals", value = FALSE)
  })
  output$variable.x.labelUI  = renderUI({
    req(input$strTestData)
    textInput("variable.x.label", label = "Label", width = '80%')
  })
  output$variable.y.labelUI  = renderUI({
    req(input$strTestData)
    textInput("variable.y.label", label = "Label", width = '80%')
  })
  output$variable.z.labelUI  = renderUI({
    req(input$strTestData)
    textInput("variable.z.label", label = "Label", width = '80%')
  })
  output$plot.titleUI  = renderUI({
    req(input$strTestData)
    textInput("plot.title", label = "Plot title", width = '90%')
  })

  output$GCMDataUI     = renderUI({
    req(input$strTestData)
    fileInput("GCMData", "Choose CSV File", multiple = F, accept = ".csv")
  })

}










################################################################################





shinyApp(ui = appUI, server = appServer)


# observeEvent(input$btn.finetune, {
#
#   toggle("plot.resUI", animType = "fade")
#   toggle("plot.colorsUI")
#   toggle("plot.legendUI")
#   toggle("plot.axis.intUI")
#   toggle("variable.x.labelUI")
#   toggle("variable.y.labelUI")
#   toggle("variable.z.labelUI")
#   toggle("plot.titleUI")
# })
# #### Hide Features
# output$btn.finetuneUI  = renderUI({
#   req(input$strTestData)
#   actionButton("btn.finetune", "Fine Tuning")
# })
