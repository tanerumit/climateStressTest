

################################################################################
### SERVER-SIDE
################################################################################

server <- function(input, output, session) {

  ## Stress Test Data Input
  stressTestDF <- reactive({
    req(input$strTestData)
    read.csv(input$strTestData$datapath, header = TRUE, sep = ",",
             stringsAsFactors = TRUE, row.names = NULL)
  })

  ## GCM Projections Data Input
  GCMDataDF <- reactive({

    req(input$GCMData)

    data <- read.csv(input$GCMData$datapath, header = TRUE, sep = ",",
             stringsAsFactors = TRUE, row.names = NULL)

    # Extract x, y, and z dimensions from the data matrix
    df   <- data %>% select(scenario, model)
    df$x <- data %>% pull(input$variable.x)
    df$y <- data %>% pull(input$variable.y)

    df
  })


  # Render Surface plot
  SurfacePlot <- reactive({

    # Set color scheme
    color.high <- "blue"
    color.low  <- "red"

    if(!is.null(input$color.reverse)) {

      if(input$color.reverse == TRUE) {
        color.high <- "red"; color.low  <- "blue"
      } else {
        color.high <- "blue"; color.low  <- "red"
      }
    }


    # Extract x, y, and z dimensions from the data matrix
    x_data <- stressTestDF() %>% pull(input$variable.x)
    y_data <- stressTestDF() %>% pull(input$variable.y)
    z_data <- stressTestDF() %>% pull(input$variable.z)

    # Default bin number and mid.point
    z_bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
    z_mid  <- input$pthreshold

    # Specify x, y breaks
    x_breaks <- unique(x_data)
    y_breaks <- unique(y_data)

    #z-dimension parameters
    z_bins1 <- c(2*z_bins[1] - z_bins[2], z_bins, 2*z_bins[length(z_bins)] - z_bins[length(z_bins)-1])
    zcut  <- binCentered(z_bins1)
    zlab  <- binCentered(zcut)

    # Prepare data matrix
    z_data[z_data < min(zcut)] <- zcut[1]
    z_data[z_data > max(zcut)] <- zcut[length(zcut)]

    res <- 100

    if(!is.null(input$plot.resol)) {
      res <- ifelse(input$plot.resol == TRUE, 300, 100)
    }


    df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = res) %>%
      mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = zlab),
             z = as.numeric(as.character(z)))

    if(!is.null(input$plot.override.axis.int)) {

      if(input$plot.override.axis.int == TRUE) {
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
      labs(x = input$xlab, y = input$ylab, fill = input$zlab,
           title = input$plot.title) +
      # Set guides
      guides(fill = guide_colorbar(nbin=length(z_bins), raster=F, barwidth=1,
                                   ticks = FALSE, barheight = 12))

    if(!is.null(input$plot.legend)) {
      if(input$plot.legend == TRUE) {
        p <- p + theme(legend.position = "none")
      }
    }

    if(!is.null(input$GCMData)) {
      if(input$GCM.scenarios == TRUE) {
        p <- p + geom_point(data = GCMDataDF())
      }
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
    downloadBttn('downloadPlot','Download Plot', size = "sm", style = "minimal")
  })



  # Render UI elements
  output$strTestDataUI = renderUI({
    fileInput("strTestData", label = NULL, multiple = F, accept = ".csv", width = '75%')
  })

  #### Stress test data inputs

  output$variable.xUI  = renderUI({
    selectInput("variable.x", label = "X-Axis Variable: ", choices = colnames(stressTestDF()),
                selected = colnames(stressTestDF())[1], width = '75%')
  })
  output$variable.yUI  = renderUI({
    selectInput("variable.y", label = "Y-Axis Variable: ", choices = colnames(stressTestDF()),
                selected = colnames(stressTestDF())[2], width = '75%')
  })
  output$variable.zUI  = renderUI({
    selectInput("variable.z", label = "Z-Axis Variable: ", choices = colnames(stressTestDF()),
                selected = colnames(stressTestDF())[3], width = '75%')
  })


  output$plot.resolUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.resol", label="High resolution", value = FALSE)
  })

  output$pthresholdUI  = renderUI({
    req(input$strTestData)
    sliderInput("pthreshold", label = "Set the level of concern (threshold)",
                min   = stressTestDF() %>% pull(input$variable.z) %>% min() %>% round(),
                max   = stressTestDF() %>% pull(input$variable.z) %>% max() %>% round(),
                value = stressTestDF() %>% pull(input$variable.z) %>% mean() %>% round(),
                round = 0,
                width = '75%'
    )
  })
  output$color.reverseUI   = renderUI({
    req(input$strTestData)
    awesomeCheckbox("color.reverse", label="Reversed color scale", value = FALSE)
  })

  output$plot.legendUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.legend", label= "Hide plot legend", value = FALSE)
  })
  output$plot.override.axis.intUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.override.axis.int", label= "Override axis intervals", value = FALSE)
  })
  output$xlabUI  = renderUI({
    req(input$strTestData)
    textInput("xlab", label = "X-axis label")
  })
  output$ylabUI  = renderUI({
    req(input$strTestData)
    textInput("ylab", label = "Y-axis label")
  })
  output$zlabUI  = renderUI({
    req(input$strTestData)
    textInput("zlab", label = "Z-axis label")
  })
  output$plot.titleUI  = renderUI({
    req(input$strTestData)
    textInput("plot.title", label = "Plot title")
  })

  #### Hide Features
  output$btn.finetuneUI  = renderUI({
    req(input$strTestData)
    actionButton("btn.finetune", "Fine Tuning")
  })

  observeEvent(input$btn.finetune, {

    toggle("plot.resolUI", animType = "fade")
    toggle("color.reverseUI")
    toggle("plot.legendUI")
    toggle("plot.override.axis.intUI")
    toggle("xlabUI")
    toggle("ylabUI")
    toggle("zlabUI")
    toggle("plot.titleUI")
  })


  output$GCMDataUI     = renderUI({
    req(input$strTestData)
    fileInput("GCMData", "Choose CSV File", multiple = F, accept = ".csv")
  })

  output$GCM.scenariosUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("GCM.scenarios", label="Overlay projections", value = FALSE)
  })

}
