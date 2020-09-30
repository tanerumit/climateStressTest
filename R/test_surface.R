
source("./global.R")
source("./R/functions.R")

library(readr)

df <- read_csv(file = "./data/stress_test_sample_data.csv") %>%
  rename(x = tavg, y = prcp, z = flow)

col1 <- "red2"; col2 <- "royalblue3"
rcp_col <- c("#08519c", "cyan3", "#fe9929", "#f03b20")

# Set color scheme
color.high <- col1; color.low  <- col2


# Extract x, y, and z dimensions from the data matrix
x_data <- df$x
y_data <- df$y
z_data <- df$z

# Default bin number and mid.point
z_bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
z_mid  <- mean(z_data)

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

res <- 300

df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = res) %>%
  mutate(zint = z) %>%
  mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = variable.z.label)) %>%
  mutate(z = as.numeric(as.character(z))) %>%
  group_by(x) %>%
  mutate(zthold = abs(zint - z_mid)) %>%
  mutate(tline = ifelse(zthold == min(zthold), 1, 0))

p <- ggplot(df, aes(x = x, y = y)) +
  theme_light(base_size = 15) +
  geom_tile(aes(fill = z), color = NA) +
  scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
  scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
  scale_fill_gradient2(low = color.low, mid = "white", high = color.high,
                       midpoint = z_mid, limits = range(z_bins), breaks = z_bins) +
  guides(fill  = guide_colorbar(nbin=length(z_bins), raster=F,
                                barwidth=1.5, ticks = F,
                                barheight = 20, order = 1),
         color = guide_legend(order = 2)) +
  geom_line(data = filter(df, tline>0), mapping = aes(x, y), size = 1)
p
