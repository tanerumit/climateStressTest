
source("./src/global.R")
source("./src/functions.R")

df <- readr::read_csv(file = "./data/stress_test_sample_data.csv") %>%
  rename(x = tavg, y = prcp, z = flow)

col1 <- "red2"; col2 <- "royalblue3"
rcp_col <- c("#08519c", "cyan3", "#fe9929", "#f03b20")

# Set color scheme
color.high <- col1; color.low  <- col2

# Extract x, y, and z dimensions from the data matrix
x_data <- df$x #x ranges from 0 to 5
y_data <- df$y #y ranges from -60 to 40
z_data <- df$z #z ranges from 13.29277 49.37604

z_mid <- 45

# Specify x, y breaks
x_breaks  <- unique(x_data)
y_breaks  <- unique(y_data)
z_breaks  <- MakeBreaks(bins = 20)(z_data, binwidth = NULL)
z_breaks_legend <- pretty(range(z_breaks),5)

p <- ggplot(df, aes(x = x, y = y)) +
  theme_light(base_size = 15) +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
  geom_contour_fill(aes(z=z), breaks = z_breaks) +
  scale_fill_divergent(midpoint = z_mid, limits = range(z_breaks_legend), breaks = z_breaks_legend) +
  guides(fill = guide_colorbar(
                               draw.ulim = FALSE, draw.llim = FALSE,
                               raster=F, barwidth=1.5, ticks = T, barheight = 20, order = 1),
         color = guide_legend(order = 2)) +
  labs(x=NULL, y=NULL, fill=NULL)
p
