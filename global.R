
#devtools::install_github('daattali/shinycssloaders')
#devtools::install_github("rstudio/shiny")
#devtools::install_github("rstudio/htmltools")

# R Dependencies
library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(tidyselect)
library(ggplot2)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(akima)
library(shinyjs)
library(shinyBS)
library(htmlwidgets)
library(reactlog)
library(shinydashboardPlus)
library(shinymaterial)
library(shinyWidgets)

#Shiny debugging
options(shiny.reactlog = TRUE)

# Misc R settings
options(stringsAsFactors = FALSE)

# Misc Shiny app settings
options(shiny.autoreload = TRUE)

options(scipen=999)

source("./global.R")
source("./R/functions.R")
