
#' Climate Response Surface Generator
#'
#' @param str.data
#' @param gcm.data
#' @param variable.x
#' @param variable.y
#' @param variable.z
#' @param threshold.z
#' @param color.low
#' @param color.high
#' @param variable.x.label
#' @param variable.y.label
#' @param variable.z.label
#' @param plot.title
#' @param scenarios_list
#' @param plot.gcm.marginal.dist
#'
#' @return
#' @export
#'
#' @examples
createSurfacePlot <- function(
    str.data = NULL,
    gcm.data = NULL,
    variable.x = NULL,
    variable.y = NULL,
    variable.z = NULL,
    threshold.z = NULL,
    color.low = "red2",
    color.high = "royalblue3",
    variable.x.label = "change in temperature",
    variable.y.label = "change in precipitation",
    variable.z.label = "change in variable",
    plot.title = "response surface",
    scenarios_list = c("rcp26", "rcp45", "rcp60", "rcp85"),
    plot.gcm.marginal.dist = TRUE,
    z_min = NULL,
    z_max = NULL,
    z_bin = 15,
    z_min_legend = NULL,
    z_max_legend = NULL,
    z_bin_legend = NULL)

  {

    require(patchwork)
    require(ggplot2)
    require(dplyr)
    require(metR)

    gg_theme_blank <- theme(axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_blank(),
          axis.title.x     = element_blank(),
          axis.title.y     = element_blank())

    gg_theme_surface <- function(size = 18) {
      theme_minimal() %+replace%
      theme(
        plot.title = element_text(size = size, hjust = 0),
        axis.title = element_text(size = size),
        legend.title = element_text(size = size),
        axis.text = element_text(size = size-2),
        legend.text = element_text(size = size-2)
      )
    }


    empty <- ggplot() + geom_point(aes(1,1), colour="white") + gg_theme_blank

    # Specify x, y breaks
    x_breaks  <- unique(str.data[[variable.x]])
    y_breaks  <- unique(str.data[[variable.y]])

    # Z-range
    if(is.null(z_min)) z_min <- min(str.data[[variable.z]])
    if(is.null(z_max)) z_max <- max(str.data[[variable.z]])
    z_breaks <- seq(z_min, z_max, length.out = z_bin)

    # Z-range legend
    if(is.null(z_min_legend)) z_min_legend  <- z_min
    if(is.null(z_max_legend)) z_max_legend  <- z_max
    if(is.null(z_bin_legend)) z_bin_legend  <- z_bin

    z_breaks_legend <- pretty(c(z_min_legend, z_max_legend), z_bin_legend)

    if(is.null(threshold.z)) threshold.z  <- mean(str.data[[variable.z]])


    # Core climate response surface
    p <- ggplot(str.data, aes(x = .data[[variable.x]], y = .data[[variable.y]])) +

      # Define theme
      gg_theme_surface() +

      # Place z dimension
      geom_contour_filled(aes(z = .data[[variable.z]],
                              fill = stat(level_mid)),
                              breaks = z_breaks) +
      geom_contour(aes(z = .data[[variable.z]]),
                   breaks = threshold.z,
                   color = "black", size = 1.5) +

      # Set scales
      scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
      scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
      scale_fill_divergent(low = color.low, mid = "white",
         high = color.high, midpoint = threshold.z,
         limits = range(z_breaks_legend), breaks = z_breaks_legend,
         guide = guide_colorbar(barwidth=2, show.limits=TRUE, ticks = TRUE,
           ticks.colour = "black", barheight = 20, order = 1, draw.ulim = FALSE)) +
      guides(color = guide_legend(order = 2)) +

      # Set labs
      labs(x = variable.x.label,y = variable.y.label,
        color = "\nGCM\nScenario", fill = variable.z.label, title = plot.title)

    scenario_col <- c("ssp126" = "#003466", "ssp245"	="#f69320",
                      "ssp370"	="#df0000", "ssp585"	="#980002")


      ######## GCM Dots
      if(!is.null(gcm.data)) {

        #Find positions of scenarios
        #gcm.data2 <- gcm.data %>% filter(scenario %in% scenarios_list)
        #sind <- which(names(scenario_col) %in% scenarios_list)

        p <- p +
          #scale_color_manual(values = scenario_col) +
          geom_point(mapping = aes(x = .data[[variable.x]], y = .data[[variable.y]],
            color = scenario), data = gcm.data, shape = 1,
            stroke = 2, size = 2.5, alpha = 0.85) +
          scale_color_manual(values = scenario_col)
      }

      if(plot.gcm.marginal.dist == TRUE & !is.null(gcm.data)) {

          p_top  <- ggplot(gcm.data, aes(x = .data[[variable.x]]))+
            scale_fill_manual(values = scenario_col) +
            scale_color_manual(values = scenario_col) +
            geom_density(aes(fill = scenario, color = scenario), alpha = 0.4, position="identity") +
            scale_x_continuous(expand = c(0, 0), limits = range(x_breaks)) +
            gg_theme_blank + guides(fill="none", color = "none") #+
            #theme(legend.position  = "none") #, panel.border = element_rect(color = "gray80", fill=NA))

          p_left  <- ggplot(gcm.data, aes(y = .data[[variable.y]]))+
            scale_fill_manual(values = scenario_col) +
            scale_color_manual(values = scenario_col) +
            geom_density(aes(fill = scenario, color = scenario), alpha = 0.4, position="identity") +
            scale_y_continuous(expand = c(0, 0), limits = range(y_breaks)) +
            gg_theme_blank + guides(fill="none", color = "none") +
            scale_x_reverse()
            #theme(legend.position  = "none") #, panel.border = element_rect(color = "gray80", fill=NA))

          p <- empty + p_top + p_left + p + plot_layout(ncol = 2, widths = c(1, 7), heights = c(1,7))

        }

    return(p)

  }



