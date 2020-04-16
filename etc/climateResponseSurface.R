
data <- readr::read_csv("./data/stress_test_sample_data.csv")

pars.x.axis = list()
pars.y.axis = list()
pars.z.axis = list()
pars.other  = list()

data <- data
pars.x.axis$name         <-  "tavg"
pars.x.axis$label        <-  expression("Cambio de temperatura (" * degree * C *")")
pars.y.axis$name         <-  "prcp"
pars.y.axis$label        <-  "Cambio de precipitacion (%)"
pars.z.axis$name         <-  "flow"
pars.z.axis$label        <-  "m3/s"
pars.z.axis$threshold    <-  35
pars.z.axis$bins         <-  NULL
pars.other$col1          <-  "red"
pars.other$col2          <-  "white"
pars.other$col3          <-  "blue"
pars.other$res           <-  100


gridInterpolate <- function(x, y, z = NULL, res, ...) {

  # Interpolation for three-dimensional array
  if (is.null(z)) {z <- rep(0, length(x))}

  z <- data.frame(z)

  df1 <- lapply(seq_len(ncol(z)), function(i) akima::interp(x, y, z[, i],
                                                            xo = seq(min(x), max(x), length = res),
                                                            yo = seq(min(y), max(y), length = res)), ...)

  df2 <- do.call("cbind", lapply(df1, function(x) c(x$z)))
  df3 <- as_tibble(cbind(expand.grid(x = df1[[1]]$x, y = df1[[1]]$y), z = df2))

}

binCentered <- function(x) {return(x[-length(x)] + (x[2] - x[1])/2)}

climateResponseMap <- function(data = NULL,
  pars.x.axis = list(name = NULL, label = NULL),
  pars.y.axis = list(name = NULL, label = NULL),
  pars.z.axis = list(name = NULL, label = NULL, threshold = NULL, bins = NULL),
  pars.other  = list(col1 = NULL, col2 = NULL, col3 = NULL, res = NULL))
{

  #Libraries required
  require(ggplot2)
  require(dplyr)


  # Extract x, y, and z dimensions from the data matrix
  x_data <- data[[pars.x.axis$name]]
  y_data <- data[[pars.y.axis$name]]
  z_data <- data[[pars.z.axis$name]]

  # Default bin number and mid.point
  if (is.null(pars.z.axis$bins))
    pars.z.axis$bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
  if (is.null(pars.z.axis$threshold))
    pars.z.axis$threshold  <- mean(z_data)

  # Specify x, y breaks
  x_breaks <- unique(x_data)
  y_breaks <- unique(y_data)

  #z-dimension parameters
  z_bins1 <- c(2*z.bins[1] - z.bins[2], z.bins, 2*z.bins[length(z.bins)] - z.bins[length(z.bins)-1])
  zcut  <- binCentered(z_bins1)
  zlab  <- binCentered(zcut)

  # Prepare data matrix
  z_data[z_data < min(zcut)] <- zcut[1]
  z_data[z_data > max(zcut)] <- zcut[length(zcut)]

  df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = pars.other$res) %>%
    mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = zlab),
           z = as.numeric(as.character(z)))

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = z), color = NA) +
    scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
    scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
    labs(x = x.specs$label, y = y.specs$label, fill = z.specs$label) +
    scale_fill_gradient2(low = color.low, mid = color.mid,
                         high = color.high, midpoint = z.mid, limits = range(z.bins), breaks = z.bins) +
    guides(fill = guide_colorbar(nbin=length(z.bins), raster=F, barwidth=1,
                                 frame.colour="black", ticks = FALSE, barheight = 12))









    return(p)

}

p <- climateResponseMap(data = data,
  pars.x.axis = list(name = "tavg", label = pars.x.axis$label ),
  pars.y.axis = list(name = "prcp", label = pars.y.axis$label ),
  pars.z.axis = list(name = "flow", label = "m3/s", threshold = 35, bins = NULL),
  pars.other  = list(col1 = "red", col2 = "white", col3 = "blue", res = 300))

p




