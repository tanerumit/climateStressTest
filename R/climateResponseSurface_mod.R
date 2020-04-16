


# UI Function
climateResponseSurface_mod_UI <- function(id, ...) {

  ns = NS(id)

  plotOutput(ns("surface"), ...)

}

# Server-side function
climateResponseSurface_mod_Server <- function(id,
      plot.title, plot.subtitle,
      grid.data, variable.x, label.x, variable.y, label.y, variable.z, label.z,
      threshold, plot.resolution, color.low, color.high) {


  moduleServer(id,

    function(input, output, session) {

      p <- reactive({

          # Extract x, y, and z dimensions from the data matrix
          x_data <- grid.data %>% pull(variable.x())
          y_data <- grid.data %>% pull(variable.y())
          z_data <- grid.data %>% pull(variable.z())

          # Default bin number and mid.point
          z_bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
          z_mid  <- threshold()

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

          df <- gridInterpolate(x = x_data , y = y_data, z = z_data, res = plot.resolution()) %>%
            mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, right = T, labels = zlab),
                   z = as.numeric(as.character(z)))


          # Prepare plot
          ggplot(df, aes(x = x, y = y)) +
            # Place z dimension
            geom_tile(aes(fill = z), color = NA) +
            # Set scales
            scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
            scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
            scale_fill_gradient2(low = as.character(color.low()), mid = "white", high = as.character(color.high()),
                                 midpoint = z_mid, limits = range(z_bins), breaks = z_bins) +
            # Set labs
            labs(x = label.x(), y = label.y(), fill = label.z(), title = plot.title(), subtitle = plot.subtitle()) +
            # Set guides
            guides(fill = guide_colorbar(nbin=length(z_bins), raster=F, barwidth=1, ticks = FALSE, barheight = 12))

      }) # reactive close

      #Render the ggplot object
      output$surface <- renderPlot(p())


    } # function close
  ) # moduleServer close
}




