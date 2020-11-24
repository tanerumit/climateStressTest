# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('menu1', tabName = 'menu1')
    , menuItem('menu2', tabName = 'menu2')
    , menuItem('menu3', tabName = 'menu3')
  )
)

# Body board
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'menu1',
      textInput('i_test', 'Test')
    ),
    tabItem(
      tabName = 'menu2',
      textInput('i_test', 'utp')
    ),
    tabItem(
      tabName = 'menu3',
      textInput('i_test', 'xxxx')
    )
  )
)

# Shiny UI
ui <- dashboardPage(
  title = 'test',
  dashboardHeader(),
  sidebar,
  body
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
