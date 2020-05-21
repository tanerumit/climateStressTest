
### UI-SIDE --------------------------------------------------------------------

#### Define body
body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  fluidPage(
    tabBox(width = 5, height = "625px", selected = "2. Base Plot",
           tabPanel(title = "1. About"),
           tabPanel(title = "2. Base Plot",
                    useShinyjs(),
                    fluidRow(
                      column(12,
                             strong("Upload Stress Test Data (csv format)"),
                             uiOutput('strTestDataUI'),
                             bsTooltip(id = "strTestDataUI", title = "Data uploading", placement = "right")
                      ),
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
                    fluidRow(
                      column(12, uiOutput('plot.titleUI'))
                    ),
                    fluidRow(
                      column(7, uiOutput('pthresholdUI'))
                    ),
                    fluidRow(
                      column(6, uiOutput("plot.legendUI")),
                      column(6, uiOutput("plot.axis.intUI"))
                    ),
                    fluidRow(
                      column(6, uiOutput("plot.colorsUI")),
                      column(6, uiOutput("plot.resUI"))
                    )

           ),
           tabPanel(title = "3. Climate Projections",
                    strong("Upload Climate Projections Data (csv format)"),
                    uiOutput('GCMDataUI'),
                    uiOutput('scenariosUI'),
                    uiOutput('modelsUI')

           )
    ),
    #box(title = "", height = "625px", width = 7, offset = 0,
        column(6, align="center",
               plotOutput("Surface1", height = "550px", width = "575px") %>% withSpinner(),
               br(),
               uiOutput("downloadPlotUI")
        ),

    #),
    tags$style(type = 'text/css',
               "footer{position: absolute; bottom:2%; right: 2%; padding:6px;}"),
    HTML('<footer> Free to use &copy; 2020 </footer>')
    #downloadButton('downloadPlot','Download Plot')
    #div(class = "square",)


  )
)

####  Define the dashboard
appUI <- dashboardPage(
  header  = dashboardHeader(title = "Plot Climate Surface"),
  skin    = "black",
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body,
  title = "ClimateSurface"
)



### SERVER-SIDE ----------------------------------------------------------------


appServer <- function(input, output, session) {


### Stress Test Data Processing ###

  stressTestData <- reactive({

    req(input$strTestData)
    read.csv(input$strTestData$datapath, header = TRUE, sep = ",",
             stringsAsFactors = TRUE, row.names = NULL)
  })


### File Uploaders

  output$strTestDataUI = renderUI({
    fileInput("strTestData", label = NULL, multiple = F, accept = ".csv", width = '92%')
  })

  output$GCMDataUI     = renderUI({
    req(input$strTestData)
    fileInput("GCMData", NULL, multiple = F, accept = ".csv", width = '92%')
  })




### RENDER CLIMATE RESPONSE SURFACE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  SurfacePlot <- reactive({

    rcp_col <- c("#08519c", "#abd9e9", "#fe9929", "#f03b20")

    # Set color scheme
    color.high <- "blue"
    color.low  <- "red"
    res <- 100

    if(!is.null(input$plot.colors)) {
      if(input$plot.colors == TRUE) {
        color.high <- "red"; color.low  <- "blue"
      }
    }

    # Extract x, y, and z dimensions from the data matrix
    x_data <- stressTestData() %>% pull(input$variable.x)
    y_data <- stressTestData() %>% pull(input$variable.y)
    z_data <- stressTestData() %>% pull(input$variable.z)

    # Default bin number and mid.point
    z_bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
    #z_mid  <- round(mean(z_data))

    if(!is.null(input$pthreshold)) {z_mid  <- input$pthreshold}

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

    if(!is.null(input$plot.res)) {res <- ifelse(input$plot.res == TRUE, 300, 100)}

    df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = res) %>%
      mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = variable.z.label)) %>%
      mutate(z = as.numeric(as.character(z)))

    if(!is.null(input$plot.axis.int)) {

      if(input$plot.axis.int == TRUE) {
        x_breaks <- pretty(df$x, n = 9)
        y_breaks <- pretty(df$y, n = 9)
      }
    }

    # Prepare plot
    p <- ggplot(df, aes(x = x, y = y)) +
      theme_light(base_size = 15) +

      # Place z dimension
      geom_tile(aes(fill = z), color = NA) +

      # Set scales
      scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
      scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
      scale_fill_gradient2(low = color.low, mid = "white", high = color.high,
                           midpoint = z_mid, limits = range(z_bins), breaks = z_bins) +


      # Set labs
      labs(x     = input$variable.x.label,
           y     = input$variable.y.label,
           fill  = input$variable.z.label,
           title = input$plot.title) +

      # Set guides
      guides(fill  = guide_colorbar(nbin=length(z_bins), raster=F, barwidth=1, ticks = F, barheight = 12, order = 1),
             color = guide_legend(order = 2)
      )

      # Equal size axes
      #coord_fixed(ratio = 1)

      if(!is.null(input$plot.legend)) {
        if(input$plot.legend == TRUE) {
          p <- p + theme(legend.position = "none")
        }
      }

      if(!is.null(input$GCMData)) {

        # Find positions of scenarios
        scenario_ind <- which(scenarios_list() %in% unique(GCMDataDF()$scenario))
        scenario_col <- rcp_col[scenario_ind]

        #if(input$GCM.scenarios == TRUE) {
          p <- p +
            scale_color_manual(values = scenario_col) +
            geom_point(aes(color = scenario), data = GCMDataDF(), shape = 1, stroke = 2, size = 2.5, alpha = 0.9)
        #}
      }

    p

  })

  output$Surface1 <- renderPlot({

    SurfacePlot()
  })

  plot_name <- reactive({ifelse(is.null(input$plot.title), "surfaceplot", input$plot.title)})

  output$downloadPlot <- downloadHandler(
    filename = function() {paste(plot_name(),'.png',sep='')},
    content  = function(file){
      ggsave(file, plot = SurfacePlot())
    }
  )

  output$downloadPlotUI  = renderUI({

    req(SurfacePlot())

    downloadBttn(
      outputId = "downloadPlot",
      label = "Download",
      style = "material-flat",
      color = "default",
      size = "sm",
      block = FALSE,
      no_outline = FALSE
    )
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
    textInput("plot.title", label = "Title", width = '92%')
  })

  output$pthresholdUI     = renderUI({
    req(input$strTestData)
    sliderInput(inputId = "pthreshold",
                label = "Threshold",
                ticks = FALSE,
                min   = stressTestData() %>% pull(input$variable.z) %>% min()  %>% round(),
                max   = stressTestData() %>% pull(input$variable.z) %>% max()  %>% round(),
                value = stressTestData() %>% pull(input$variable.z) %>% mean() %>% round(),
                round = 0,
                width = '92%'
    )
  })

  #### CHeck Boxes
  output$plot.resUI       = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.res", label="High Resolution", value = FALSE)
  })
  output$plot.colorsUI    = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.colors", label="Switch Colors", value = FALSE)
  })
  output$plot.legendUI    = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.legend", label= "Hide Legend", value = FALSE)
  })
  output$plot.axis.intUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.axis.int", label= "Override Intervals", value = FALSE)
  })

### GCM PROJECTIONS TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  scenarios_list <- reactive({

    dat <- GCMData() %>% pull(scenario) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })
  models_list    <- reactive({

    dat <- GCMData() %>% pull(model) %>% unique()
    names(dat) <- dat
    as.vector(dat)

  })

  output$scenariosUI <- renderUI({

    req(input$GCMData)

      pickerInput(
        inputId = "scenarios",
        label = "Scenarios",
        choices = scenarios_list(),
        selected = scenarios_list(),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
  })
  output$modelsUI    <- renderUI({

    req(input$GCMData)

    pickerInput(
      inputId = "models",
      label = "Climate Models",
      choices = models_list(),
      selected = models_list(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })



}

### APPLICATON -----------------------------------------------------------------

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
