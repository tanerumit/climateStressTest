
source("./src/global.R")
source("./src/functions.R")

df <- readr::read_csv(file = "./data/stress_test_sample_data.csv") %>%
  rename(x = tavg, y = prcp, z = flow)

gcm_df <- read.csv("./data/gcm_delta_change_2050_default.csv", header = T, sep = ",",
                   stringsAsFactors = T, row.names = NULL) %>%
  rename(x = tavg, y = prcp)




col1 <- "red2"; col2 <- "royalblue3"
rcp_col <- c("#08519c", "cyan3", "#fe9929", "#f03b20")

# Set color scheme
color.high <- col1; color.low  <- col2

# Extract x, y, and z dimensions from the data matrix
x_data <- df$x #x ranges from 0 to 5
y_data <- df$y #y ranges from -60 to 40
z_data <- df$z #z ranges from 13.29277 49.37604

z_mid <- 30

# Specify x, y breaks
x_breaks  <- unique(x_data)
y_breaks  <- unique(y_data)
z_breaks  <- MakeBreaks(bins = 20)(z_data, binwidth = NULL)
z_breaks_legend <- pretty(range(z_breaks),5)

# Core climate response surface
p <- ggplot(df, aes(x = x, y = y)) +
  # Define theme
  theme_light(base_size = 15) +
  # Place z dimension
  geom_contour_fill(aes(z=z), breaks = z_breaks) +
  # Set scales
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
  scale_fill_divergent(low = color.low, mid = "white", high = color.high, midpoint = z_mid, limits = range(z_breaks_legend), breaks = z_breaks_legend) +
  # Set labs
  labs(x = "xvar",y = "yvar",color = NULL, fill = "zvar",title = "surface") +
  # Set guides
  guides(fill = guide_colorbar(raster=F, barwidth=1.5, ticks = TRUE, barheight = 20, order = 1), color = guide_legend(order = 2)) +
  # Threshold line
  geom_contour(aes(z = z), breaks = z_mid, color = "black", size = 1.5)

  p <- p +
    scale_color_manual(values = rcp_col) +
    geom_point(aes(color = scenario), data = gcm_df, shape = 1, stroke = 2, size = 3, alpha = 0.75)

p


library(plotly)
px <- ggplotly(p) + theme(legend.position = "none")



if (!require("processx")) install.packages("processx")
fig <- plot_ly(z = ~volcano) %>% add_surface()
orca(px, "surface-plot.svg")

p <- plot_ly(z = ~volcano) %>% add_surface()
orca(p, "surface-plot.svg")
###################################################################

library(highcharter)

scatter_data <- data_frame(
  x = c(1.1, 0.3, 3.3,4.1, 2.6), y = c(1.2, 6.31, 7.5, 8.2, 9.9) )

heatmap_data <- data_frame(
  x = c(rep(0,6),rep(1,6),rep(2,6),rep(3,6),rep(4,6), rep(5,6)),
  y = rep(6:11,6),
  value = runif(36,70,90)
)

heatmap_data$value <- round(heatmap_data$value, 2)

hc <- highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = "Heatmap with Scatter Plot") %>%
  hc_add_series(name = "value", data = heatmap_data,
                dataLabels = list(enabled = TRUE, format = "{point.value}"))
hc



hc_colorAxis(hc, minColor = "#FFFFFF", maxColor = "#434348") %>%
  hc_add_series(data = scatter_data, type = "scatter")


df2 <- df %>% mutate(x = factor(x), y = factor(y))

hc <- df2 %>%
  hchart(type = "heatmap", hcaes(x = x, y = y, value = z)) %>%
  hc_legend(layout = "vertical", verticalAlign = "top", align = "right")


hc_colorAxis(hc, minColor = "#FFFFFF", maxColor = "#434348") %>%
  hc_add_series(data = scatter_data, type = "scatter")


hchart(df,
       "heatmap",
       hcaes(x = x, y = y, value = z),
       name = "metic")


library(dplyr)
library(highcharter)

dfdiam <- diamonds %>%
  group_by(cut, clarity) %>%
  summarize(price = median(price))
