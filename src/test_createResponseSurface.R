

source("./src/createResponseSurface.R")
source("./src/functions.R")
source("./src/global.R")


str_data <- str_data_init %>% filter(statistic == "mean") %>% select(-statistic)
gcm_data_init <- read_csv("./data/2024/gcm_means.csv") 
  
  
createSurfacePlot(
    str.data = str_data,
    gcm.data = gcm_data_init,
    variable.x = "tavg",
    variable.y = "prcp",
    variable.z = "Q_1006",
    threshold.z = NULL,
    color.low = "red2",
    color.high = "royalblue3",
    variable.x.label = "Change in Temperature",
    variable.y.label = "Change in Precipitation",
    variable.z.label = "Metric",
    plot.title = "response surface",
    scenarios_list = c("ssp126", "ssp245", "ssp370", "ssp585"),
    plot.gcm.marginal.dist = FALSE,
    z_bin = 20,
    z_min_legend = NULL,
    z_max_legend = NULL,
    z_bin_legend = 5)

