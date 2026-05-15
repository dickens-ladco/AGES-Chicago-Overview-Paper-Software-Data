# GMAP_final_dataset.R must be loaded before running this script

# Load necessary libraries
library(ggmap)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)
library(patchwork)

# Register Google Maps API key
register_google(key = "...")

# Function to filter data by date, time of day (AM/PM), and facility boundaries
filter_data_by_facility <- function(data, date, facility, am_pm = NULL) {
  boundaries <- list(
    "Bedford Park" = list(max_lat = 41.76988, min_lat = 41.74738, max_long = -87.70458, min_long = -87.79708),
    "Corwith" = list(max_lat = 41.83626, min_lat = 41.80875, max_long = -87.70538, min_long = -87.72288),
    "Global Two" = list(max_lat = 41.90787, min_lat = 41.89035, max_long = -87.88609, min_long = -87.93859),
    "Hare1" = list(max_lat = 42.02007, min_lat = 41.97117, max_long = -87.93815, min_long = -87.96064),
    "Hare2" = list(max_lat = 41.96867, min_lat = 41.90617, max_long = -87.82065, min_long = -87.88814)
  )
  
  bounds <- boundaries[[facility]]
  
  if (!is.null(am_pm)) {
    data <- data %>%
      filter(Date == date & circuit == am_pm &
               Latitude >= bounds$min_lat & Latitude <= bounds$max_lat &
               Longitude >= bounds$min_long & Longitude <= bounds$max_long)
  } else {
    data <- data %>%
      filter(Date == date &
               Latitude >= bounds$min_lat & Latitude <= bounds$max_lat &
               Longitude >= bounds$min_long & Longitude <= bounds$max_long)
  }
  
  return(data)
}

# Function to plot NO2 concentrations with wind data and size variation on a satellite map
plot_NO2_wind_size_facility <- function(data, date, facility, max_NO2, min_NO2, am_pm = NULL, show_legend = FALSE) {
  # Filter data
  data_filtered <- filter_data_by_facility(data, date, facility, am_pm)
  
  if (nrow(data_filtered) == 0) { #for troubleshooting the bounds
    print("No data available for the specified filter criteria.")
    return(NULL)
  }
  
  # Define zoom levels for each facility
  zoom_levels <- list(
    "Bedford Park" = 13,
    "Corwith" = 14,
    "UIC" = 17,
    "Global Two" = 13,
    "Hare1" = 13,
    "Hare2" = 13
  )
  
  # Get the bounding box for the filtered data
  bbox <- make_bbox(lon = data_filtered$Longitude, lat = data_filtered$Latitude, f = 0.1)
  
  # Get the map
  zoom_level <- zoom_levels[[facility]]
  satellite_map <- get_map(location = bbox, source = "google", maptype = "satellite", zoom = zoom_level, crop = TRUE)
  
  # Define maximum NO2 concentration for color scale (from histograms)
  max_NO2_color <- switch(facility,
                          "Bedford Park" = 70,
                          "Corwith" = 87.5,
                          "Global Two" = 40,
                          "O’Hare" = 87.5)
  
  # Choose color scale, gradient up until a certain point
  color_scale_function <- scale_color_gradientn(colors = matlab.like(32), name = "NO2 Conc (ppb)", limits = c(min_NO2, max_NO2_color), oob = scales::rescale_none, na.value = "gray20")
  
  # Preparing for arrows
  data_filtered <- data_filtered %>%
    mutate(xend = Longitude + Wind_Speed * sin((Wind_Direction + 180) * pi / 180) / 1000,
           yend = Latitude + Wind_Speed * cos((Wind_Direction + 180) * pi / 180) / 1000)
  
  # Plot wind arrows and size variation
  p <- ggmap(satellite_map) +
    geom_point(data = data_filtered, aes(x = Longitude, y = Latitude, color = NO2.Conc, size = NO2.Conc), alpha = 0.7) +
    scale_size_continuous(range = c(1, 10), name = "NO2 Conc", limits = c(min(data_filtered$NO2.Conc, na.rm = TRUE), max(data_filtered$NO2.Conc, na.rm = TRUE))) +
    geom_segment(data = data_filtered, 
                 aes(x = Longitude, y = Latitude, 
                     xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black", size = 0.5) +
    color_scale_function +
    labs(title = paste(date, am_pm), x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(legend.position = ifelse(show_legend, "bottom", "none"))
  
  return(p)
}

# Function to generate and save plots for each facility
generate_plots_for_facility <- function(data, facility, dates, am_pm_labels = c("AM", "PM")) {
  # Calculate the global NO2 concentration range for the facility data
  max_NO2 <- max(data$NO2.Conc, na.rm = TRUE)
  min_NO2 <- min(data$NO2.Conc, na.rm = TRUE)
  
  # Generate plots for all combinations of dates and AM/PM labels, complicated because only want to show one cohesive legend
  plots <- list()
  for (i in seq_along(dates)) {
    if (facility %in% c("Global Two", "O’Hare")) {
      plot <- plot_NO2_wind_size_facility(data, dates[i], facility, max_NO2, min_NO2, show_legend = (i == length(dates)))
      if (!is.null(plot)) {
        plots <- append(plots, list(plot))
      }
    } else {
      for (j in seq_along(am_pm_labels)) {
        show_legend <- (i == length(dates) && j == length(am_pm_labels))
        plot <- plot_NO2_wind_size_facility(data, dates[i], facility, max_NO2, min_NO2, am_pm = am_pm_labels[j], show_legend = show_legend)
        if (!is.null(plot)) {
          plots <- append(plots, list(plot))
        }
      }
    }
  }
  
  # Combine all plots using patchwork
  if (length(plots) > 0) {
    combined_plot <- wrap_plots(plots, ncol = 4) + plot_layout(guides = "collect")
    file_name <- paste0("~/Desktop/GMAP/Code/Facility Plots Final/combined_plot_", gsub(" ", "_", tolower(facility)), ".png")
    ggsave(filename = file_name, plot = combined_plot, width = 20, height = 16)
    print(paste("Plot saved as", file_name))
  } else {
    print("No plots to display.") #troubleshooting purposes
  }
}

# List of dates for each facility
dates_bedford <- c("Aug 01", "Aug 02", "Aug 08", "Aug 12")
dates_corwith <- c("Aug 01", "Aug 02", "Aug 08", "Aug 12")
dates_global_two <- c("Aug 01", "Aug 02")
dates_hare1 <- c("Aug 08") # Sample dates for Hare1
dates_hare2 <- c("Aug 08") # Sample dates for Hare2

# Generate plots for each facility
generate_plots_for_facility(GMAP.final.avg, "Bedford Park", dates_bedford)
generate_plots_for_facility(GMAP.final.avg, "Corwith", dates_corwith)
generate_plots_for_facility(GMAP.final.avg, "Global Two", dates_global_two)
generate_plots_for_facility(GMAP.final.avg, "Hare1", dates_hare1)
generate_plots_for_facility(GMAP.final.avg, "Hare2", dates_hare2)
