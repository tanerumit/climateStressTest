
source("./global.R")
source("./R/functions.R")

### UI-SIDE --------------------------------------------------------------------

#### Define body
body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  fluidPage(
    p(div(HTML("<h4> <strong> CLIMATE RESPONSE SURFACES VISUALIZER </strong> </h4> This app visualizes climate response functions. To learn more, click the tab: <strong> <em> About </em> </strong>.
               To begin, click the tab: <strong> <em> 1. Draw Base Plot </em> </strong> and
               upload your data. For superimposing climate projections, click: <strong> <em> 2. Overlay Climate Info </em> </strong> "), style="margin-left:15px;")),

    #p(div(HTML("This work is licensed under <a href=https://creativecommons.org/licenses/by-sa/4.0/> Creative Commons
    #           Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License </a>"), style="margin-left:15px;")),

    tabBox(width = 5, height = "580px", selected = "1. Draw Base Plot",
           tabPanel(title = "About",
                    includeHTML("./www/readme2.html"),
                    br(),
                    img(src='crs.png',style="width: 300px", align="center")
           ),
           tabPanel(title = "1. Draw Base Plot",
                    useShinyjs(),
                    fluidRow(
                      column(9,
                             strong("Upload Stress Test Data (csv file)"),
                             uiOutput('strTestDataUI')
                      ),
                      column(3,
                             br(), #br(), br(), br(),
                             bs_modal(id = "modal1", title = "Stress test data", body = DTOutput('strTestDataTbl')),
                             bs_attach_modal(uiOutput("stressTestDataTblBttnUI"), id_modal = "modal1")
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
                      column(7, uiOutput('pthresholdUI')),
                      column(5, uiOutput('plot.titleUI'))
                    ),
                    #fluidRow(
                    #  column(7, uiOutput('pthresholdUI'))
                    #),
                    #tags$em("Fine tuning"),
                    fluidRow(
                      column(6, uiOutput("plot.legendUI")),
                      column(6, uiOutput("plot.axis.intUI"))
                    ),
                    fluidRow(
                      column(6, uiOutput("plot.colorsUI")),
                      column(6, uiOutput("plot.resUI"))
                    )

           ),
           tabPanel(title = "2. Overlay Climate Info",
                    #strong("Historical climate"),
                    strong("Upload Climate Projections Data (csv format)"),
                    fluidRow(
                      column(9, uiOutput('GCMDataUI')),
                      column(3,
                             #br(),
                             bs_modal(id = "modal2", title = "GCM projections", body = DTOutput('GCMDataTbl')),
                             bs_attach_modal(uiOutput("GCMDataTblBttnUI"), id_modal = "modal2")
                      ),
                    ),
                    uiOutput('scenariosUI'),
                    uiOutput('modelsUI'),
                    uiOutput('gcm.contoursUI'),
                    uiOutput('plot.histvalueUI')

           )
    ),

        column(5, align="center",
               plotOutput("SurfacePlotUI", height = "580px", width = "600px") %>% withSpinner(),
        ),
        column(1, offset = 0,
               uiOutput("downloadPlotUI")
        ),

    #),
    tags$style(type = 'text/css',
               "footer{position: absolute; bottom:2%; right: 2%; padding:6px;}")



  )
)

####  Define the dashboard
appUI <- dashboardPage(
  header  = dashboardHeader(title = "Plot Climate Surface", disable = TRUE),
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
    fileInput("strTestData", label = NULL, multiple = F, accept = ".csv", width = '100%')
  })
  output$GCMDataUI     = renderUI({
    req(input$strTestData)
    fileInput("GCMData", NULL, multiple = F, accept = ".csv", width = '100%')
  })


### Data Tables
  output$strTestDataTbl = renderDataTable({

    datatable(stressTestData(),
        options = list(lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-left', targets = "_all"))),
        class = 'cell-border stripe') %>%
     formatRound(columns = sapply(stressTestData(), is.numeric), digits = 2) %>%
     formatRound(columns = 0, digits = 0)
  })
  output$GCMDataTbl     = renderDataTable({

    datatable(GCMData(),
              options = list(lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-left', targets = "_all"))),
              class = 'cell-border stripe') %>%
      formatRound(columns = sapply(GCMData(), is.numeric), digits = 2) %>%
      formatRound(columns = 0, digits = 0)
  })


### RENDER CLIMATE RESPONSE SURFACE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  SurfacePlot <- reactive({

    col1 <- "red2"
    col2 <- "royalblue3"

    #rcp_col <- c("#08519c", "#abd9e9", "#fe9929", "#f03b20")
    rcp_col <- c("#08519c", "cyan3", "#fe9929", "#f03b20")

    # Set color scheme
    color.high <- col2
    color.low  <- col1
    res <- 100

    if(!is.null(input$plot.colors)) {
      if(input$plot.colors == TRUE) {
        color.high <- col1; color.low  <- col2
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


    p <- ggplot(df, aes(x = x, y = y)) +

      # Define theme
      theme_light(base_size = 15) +

      # Place z dimension
      geom_tile(aes(fill = z), color = NA) +

      # Set scales
      scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
      scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
      scale_fill_gradient2(low = color.low, mid = "white", high = color.high, midpoint = z_mid, limits = range(z_bins), breaks = z_bins) +


      # Set labs
      labs(x     = input$variable.x.label,
           y     = input$variable.y.label,
           color = NULL,
           fill  = input$variable.z.label,
           title = input$plot.title) +

      # Set guides
      guides(fill  = guide_colorbar(nbin=length(z_bins), raster=F, barwidth=1.5, ticks = F, barheight = 25, order = 1),
             color = guide_legend(order = 2)
      )

      # Legend ON/OFF
      if(!is.null(input$plot.legend)) {
        if(input$plot.legend == TRUE) {
          p <- p + theme(legend.position = "none")
        }
      }

      # GCM Dots
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

      ### GCM Contours
      if(0.80 %in% input$gcm.contours) {
        p <- p + stat_ellipse(data = GCMDataDF(), size = 1, linetype = "dotdash", level = 0.80)
      }
      if(0.90 %in% input$gcm.contours) {
        p <- p + stat_ellipse(data = GCMDataDF(), size = 1, linetype = "dotdash", level = 0.90)
      }
      if(0.950 %in% input$gcm.contours) {
        p <- p + stat_ellipse(data = GCMDataDF(), size = 1, linetype = "dotdash", level = 0.95)
      }
      if(0.99 %in% input$gcm.contours) {
        p <- p + stat_ellipse(data = GCMDataDF(), size = 1, linetype = "dotdash", level = 0.99)
      }
      if(0.995 %in% input$gcm.contours) {
        p <- p + stat_ellipse(data = GCMDataDF(), size = 1, linetype = "dotdash", level = 0.995)
      }

      if(!is.null(input$plot.histvalue)) {
        if(input$plot.histvalue == TRUE) {
          p <- p + geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +  geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)
        }
      }

      p

  })
  output$SurfacePlotUI <- renderPlot({

    SurfacePlot()
  })

  plot_name <- reactive({ifelse(is.null(input$plot.title), "surfaceplot", input$plot.title)})

  output$downloadPlot <- downloadHandler(
    filename = function() {paste(plot_name(),'.png',sep='')},
    content  = function(file){
      ggsave(file, plot = SurfacePlot())
    }
  )

### Buttions
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
  output$stressTestDataTblBttnUI  = renderUI({
    req(input$strTestData)
    actionBttn(
      inputId = "stressTestDataTblBttn",
      label = "View",
      style = "material-flat",
      size = "sm",
      color = "default"
    )
  })
  output$GCMDataTblBttnUI  = renderUI({
    req(input$strTestData)
    actionBttn(
      inputId = "GCMDataTblBttn",
      label = "View",
      style = "material-flat",
      size = "sm",
      color = "default"
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
    textInput("variable.x.label", label = "Label", width = '85%')
  })
  output$variable.y.labelUI  = renderUI({
    req(input$strTestData)
    textInput("variable.y.label", label = "Label", width = '85%')
  })
  output$variable.z.labelUI  = renderUI({
    req(input$strTestData)
    textInput("variable.z.label", label = "Label", width = '85%')
  })
  output$plot.titleUI  = renderUI({
    req(input$strTestData)
    textInput("plot.title", label = "Plot title", width = '85%')
  })

  output$pthresholdUI     = renderUI({
    req(input$strTestData)
    sliderInput(inputId = "pthreshold",
                label = "Performance Threshold",
                ticks = FALSE,
                step  = NULL, #this needs to be fixed
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
    awesomeCheckbox("plot.res", label="Increase plot resolution", value = FALSE)
  })
  output$plot.colorsUI    = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.colors", label="Flip fill colors", value = FALSE)
  })
  output$plot.legendUI    = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.legend", label= "Hide plot legend", value = FALSE)
  })
  output$plot.axis.intUI  = renderUI({
    req(input$strTestData)
    awesomeCheckbox("plot.axis.int", label= "Override axis intervals", value = FALSE)
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

  models_list    <- reactive({

    dat <- GCMData() %>% pull(model) %>% unique()
    names(dat) <- dat
    as.vector(dat)

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

  output$gcm.contoursUI = renderUI({
    req(input$GCMData)
    checkboxGroupButtons(inputId = "gcm.contours",
                    label   = "Confidence Intervals",
                    selected = NULL,
                    status = "primary",
                    choiceNames  = c("80%", "90%", "95%", "99%", "99.5%"),
                    choiceValues = c(0.80, 0.90, 0.95, 0.99, 0.995),
                    checkIcon = list(
                      yes = icon("ok",
                                 lib = "glyphicon"),
                      no = icon("remove",
                                lib = "glyphicon"))
    )

  })

  output$plot.histvalueUI = renderUI({
    req(input$GCMData)
    awesomeCheckbox("plot.histvalue", label= "Hist. performance", value = FALSE)
  })


}

### APPLICATON -----------------------------------------------------------------

shinyApp(ui = appUI, server = appServer)
