

################################################################################
### UI-SIDE
################################################################################


jsc <- '
$(document).ready(function () {
  $(".sidebar-menu").children("li").on("click", function() {
    $("#mult, #single").toggle();
  });
});
'

#### Define side-bar
sidebar <- dashboardSidebar(disable = TRUE)


#### Define body
body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  fluidPage(
    tabBox(width = 5, height = "750px", selected = "2. Set Basic Layout",
         tabPanel(title = "1. About",
                  tags$div(style="margin-left:15px;",
                           h3(strong("Climate Response Surface Generator"))
                  ),
                  p("Description")
         ),
         tabPanel(title = "2. Define Plot",
                  useShinyjs(),
                  br(),
                  p("Upload the Stress test data and specify x, y, and z axes variables"),
                  uiOutput('strTestDataUI'),
                  uiOutput('variable.xUI'),
                  uiOutput('variable.yUI'),
                  uiOutput('variable.zUI'),
                  br(),
                  uiOutput('pthresholdUI')
        ),
        tabPanel(title = "3. Fine Tune",
                 br(),
                 uiOutput("plot.titleUI"),
                 uiOutput("xlabUI"),
                 uiOutput("ylabUI"),
                 uiOutput("zlabUI"),
                 splitLayout(cellWidths = c("45%", "45%"),
                             uiOutput("plot.legendUI"),
                             uiOutput("plot.override.axis.intUI")
                 ),
                 splitLayout(cellWidths = c("45%", "45%"),
                             uiOutput("plot.resolUI"),
                             uiOutput("color.reverseUI")
                 )
        ),
        tabPanel(title = "3. Overlay GCMs",
                 p("Description"),
                 splitLayout(cellWidths = "90%", uiOutput('GCMDataUI')),
                 splitLayout(cellWidths = "90%", uiOutput('GCM.scenariosUI'))
        )
      ),
      box(title = "Climate Response Surface", height = "750px", width = 5, offset = 2,
            p("Describe what it is"),
            column(12, align="center",
                    plotOutput("Surface1", height = "500px", width = "600px"),
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
dashboardPage(
  skin    = "yellow",
  header  = dashboardHeader(title = "Climate Response Surface"),
  sidebar = sidebar,
  body    = body
)

#    tags$style(type='text/css', "#stressTestDataUI {width:100%; margin-top: 25px;}"),
#    tags$style(type='text/css', "#button1  {width:100%; margin-top: 25px;}"),



