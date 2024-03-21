library(tidyverse)
library(readr)
library(tidyr)
library(broom)
library(jsonlite)
library(usmap)
library(dplyr)
library(tigris)
library(tidycensus)
library(patchwork)
library(sf)
library(sp)
library(viridis)
library(RColorBrewer)
library(ggpubr)

options(tigris_use_cache = FALSE)

folder = dirname(rstudioapi::getSourceEditorContext()$path)
data_directory = file.path(folder, '..', 'data')
setwd(data_directory)

# FERC shape files
nerc_geojson <- st_read("ferc_boundaries.geojson")

# US State shapefiles
state_shp <-
  tigris::states(cb = TRUE,
                 year = 2020,
                 resolution = "20m")

state_shp <- tigris::shift_geometry(state_shp)
state_shp <- rename(state_shp, STABBR = STUSPS)

# Exclude non contigous US States
non_cont_fips_codes <-
  c('02', '15', '72', '66', '60', '69', '78', '78')
state_shp_filtered <- state_shp %>%
  filter(!STATEFP %in% non_cont_fips_codes)

# Set crs
nerc_geojson <- st_transform(nerc_geojson, 'ESRI:102003')
state_shp_filtered <-
  st_transform(state_shp_filtered, 'ESRI:102003')

## General functions and params ##

# List of columns to be grouped
cols_to_group <- c("X.1.96SD", "X.1SD", "Mean", "X1SD", "X1.96SD")

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Read the general grid regions data
grid_csv <- read.csv("grid_data.csv")

#  Create variable groups and labels in intervals 8
create_group_labels <- function(fill_variable) {
  num_groups <- 8
  
  max_value <-
    max(grid_csv[[fill_variable]], na.rm = TRUE)
  min_value <- min(grid_csv[[fill_variable]], na.rm = TRUE)
  
  max_value_adjusted <- ceiling(max_value)
  min_value_adjusted <- floor(min_value)
  
  range_value <- max_value_adjusted - min_value_adjusted
  interval_size <- range_value / num_groups
  
  first_group <- min_value_adjusted
  last_group <- max_value_adjusted
  
  # Generate the remaining groups, ensuring they are ceiled at each step, excluding the adjusted max_value
  remaining_groups <- seq(first_group + interval_size, max_value_adjusted - interval_size, by = interval_size)
  remaining_groups <- ceiling(remaining_groups)
  
  # Combine the first group with the remaining groups
  groups <- c(first_group, remaining_groups, last_group)
  
  # Create group labels
  group_labels <- paste0(groups[-length(groups)], "-", groups[-1] + 1 - 1)
  
  return(list(groups = groups, labels = group_labels))
}

# GDP groups
gdp_label_list <- create_group_labels("DailyGDP") # Groups and labels
gdp_groups <- gdp_label_list$groups # Groups
group_gdp_labels <-  gdp_label_list$labels # Labels

# Pop groups
pop_label_list <- create_group_labels("DailyGDP") # Groups and labels
pop_groups <- pop_label_list$groups # Groups
group_pop_labels <-  pop_label_list$labels # Labels

# Est groups
est_label_list <- create_group_labels("DailyGDP") # Groups and labels
est_groups <- est_label_list$groups # Groups
group_est_labels <-  est_label_list$labels # Labels

# Function to apply cut and create grouped columns
create_grouped_cols_v2 <- function(data, cols, breaks, labels) {
  for (col in cols) {
    group_col_name <- paste(col, "Group", sep = "_")
    data[[group_col_name]] <-
      cut(
        data[[col]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = labels
      )
  }
  return(data)
}

# Create a faceted sf plots
create_sf_faceted_plot <-
  function(data,
           data_2,
           legend_title,
           plot_title,
           plot_subtitle,
           group_labels) {
    
    # Order and label the 'Group_Category' factor
    data$Group_Category <- factor(
      data$Group_Category,
      levels = c(
        "X.1.96SD_Group",
        "X.1SD_Group",
        "Mean_Group",
        "X1SD_Group",
        "X1.96SD_Group"
      ),
      labels = c("2.5%", "16%", "Mean", "84%", "97.5%")
    )
    # Unique groups in the pivoted df
    num_groups <-
      length(group_labels)
    viridis_colors <-
      viridis::viridis_pal(option = "D", direction = -1)(num_groups)
    
    plot <- ggplot(data) +
      geom_sf(
        data = data_2,
        fill = NA,
        color = "dodgerblue",
        linewidth = 0.1,
        alpha = 1
      ) +
      geom_sf(
        aes(fill = factor(Group_Value)),
        linewidth = 0.34,
        alpha = 0.8,
        color = "white"
      ) +
      scale_fill_manual(values = viridis_colors, name = legend_title, breaks = group_labels, limits = group_labels) +
      facet_wrap( ~ Group_Category, ncol = length(unique(data$Group_Category))) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      theme_void() +
      theme(
        plot.title = element_text(
          size = 11,
          hjust = 0.01,
          margin = margin(
            b = 0.2,
            t = 0,
            l = 2,
            unit = "cm"
          )
        ),
        plot.subtitle = element_text(hjust = 0.01, 
                                     size = 9,
                                     margin = margin(
                                       b = 0.3,
                                       t = 0,
                                       l = 2,
                                       unit = "cm"
                                     )
                                     ),
        legend.position = "right",
        legend.justification = "right",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Reduces margin around the legend
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Reduces space between legend and plot
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(0.01, "npc"),
        panel.spacing = unit(0, "lines")  # Reduce spacing between panels
      ) +
      theme(text = element_text(family = "Times New Roman"))
    
    return(plot)
  }

### GDP Event 2 ###

# File paths and data directory
event_2_data_dir = file.path(data_directory, 'events', '2')
event_2_pop_df <- read.csv(file.path(event_2_data_dir, 'event_2_pop_affected.csv')) # Read pop data
event_2_est_df <- read.csv(file.path(event_2_data_dir, 'event_2_est_by_regions.csv')) # Read establishment data
event_2_gdp_df <- read.csv(file.path(event_2_data_dir, 'event_2_gdp_by_regions.csv')) # Read GDP data

# Apply the function to your dataframes
event_2_pop_df <- replace_zeros_with_NA(event_2_pop_df, cols_to_group)
event_2_est_df <- replace_zeros_with_NA(event_2_est_df, cols_to_group)
event_2_gdp_df <- replace_zeros_with_NA(event_2_gdp_df, cols_to_group)

# Apply the updated function to event_2_gdp_df
event_2_gdp_df <-
  create_grouped_cols_v2(event_2_gdp_df, cols_to_group, gdp_groups, group_gdp_labels)

#  Merge with FERC GeoJSON
event_2_gdp_geo <- nerc_geojson %>%
  inner_join(event_2_gdp_df, by = c("REGIONS"))

# Prepare facet data
event_2_gdp_pivoted <- event_2_gdp_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_2_gdp_plot = create_sf_faceted_plot(
  data = event_2_gdp_pivoted,
  state_shp_filtered,
  legend_title = "GDP\n(US $Bn)",
  plot_title = "(C) Direct economic impact",
  plot_subtitle = "Distribution of GDP shock in events leading to blackout in two grid regions",
  group_labels = group_gdp_labels
)

event_2_gdp_plot

### Population Event 2 ###

# Apply the updated function to event_2_gdp_df
event_2_pop_df <-
  create_grouped_cols_v2(event_2_pop_df, cols_to_group, pop_groups, group_pop_labels)

#  Merge with FERC GeoJSON
event_2_pop_geo <- nerc_geojson %>%
  inner_join(event_2_pop_df, by = c("REGIONS"))

# Prepare facet data
event_2_pop_pivoted <- event_2_pop_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_2_pop_plot = create_sf_faceted_plot(
  data = event_2_pop_pivoted,
  state_shp_filtered,
  legend_title = "Population\n(Mn)",
  plot_title = "(A) Population affected in two grid failure scenarios",
  plot_subtitle = "Heterogeneous distribution of population affected in the selected percentiles across grid regions",
  group_labels = group_pop_labels
)

event_2_pop_plot

### Establishment Event 2 ###

# Apply the updated function to event_2_gdp_df
event_2_est_df <-
  create_grouped_cols_v2(event_2_est_df, cols_to_group, est_groups, group_est_labels)

#  Merge with FERC GeoJSON
event_2_est_geo <- nerc_geojson %>%
  inner_join(event_2_est_df, by = c("REGIONS"))

# Prepare facet data
event_2_est_pivoted <- event_2_est_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")

event_2_est_plot = create_sf_faceted_plot(
  data = event_2_est_pivoted,
  state_shp_filtered,
  legend_title = "Count\n(1,000)",
  plot_title = "(B) Business establishments",
  plot_subtitle = "Scenarios that may affect businesses within one standard deviation, two standard deviations, and the mean in the distribution",
  group_labels = group_est_labels
)

event_2_est_plot

combined_event_2_plot <- ggarrange(
  event_2_pop_plot, 
  event_2_est_plot, 
  event_2_gdp_plot,
  ncol = 1, 
  nrow = 3
)

print(combined_event_2_plot)

file_path = file.path(folder,"figures", "stacked_event_2_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_event_2_plot, dpi = 300, width=8, height=4.5, bg="#f2f2f2")  # Adjust dpi for image quality


### GDP Event 4 ###

# File paths and data directory
event_4_data_dir = file.path(data_directory, 'events', '4')
event_4_pop_df <- read.csv(file.path(event_4_data_dir, 'event_4_pop_affected.csv')) # Read pop data
event_4_est_df <- read.csv(file.path(event_4_data_dir, 'event_4_est_by_regions.csv')) # Read establishment data
event_4_gdp_df <- read.csv(file.path(event_4_data_dir, 'event_4_gdp_by_regions.csv')) # Read GDP data

# Apply the function to your dataframes
event_4_pop_df <- replace_zeros_with_NA(event_4_pop_df, cols_to_group)
event_4_est_df <- replace_zeros_with_NA(event_4_est_df, cols_to_group)
event_4_gdp_df <- replace_zeros_with_NA(event_4_gdp_df, cols_to_group)

# Apply the updated function to event_4_gdp_df
event_4_gdp_df <-
  create_grouped_cols_v2(event_4_gdp_df, cols_to_group, gdp_groups, group_gdp_labels)

#  Merge with FERC GeoJSON
event_4_gdp_geo <- nerc_geojson %>%
  inner_join(event_4_gdp_df, by = c("REGIONS"))

# Prepare facet data
event_4_gdp_pivoted <- event_4_gdp_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_4_gdp_plot = create_sf_faceted_plot(
  data = event_4_gdp_pivoted,
  state_shp_filtered,
  legend_title = "GDP\n(US $Bn)",
  plot_title = "(C) Direct economic impact",
  plot_subtitle = "Distribution of GDP shock in events leading to blackout in four grid regions",
  group_labels = group_gdp_labels
)

event_4_gdp_plot

### Population Event 4 ###

# Apply the updated function to event_4_gdp_df
event_4_pop_df <-
  create_grouped_cols_v2(event_4_pop_df, cols_to_group, pop_groups, group_pop_labels)

#  Merge with FERC GeoJSON
event_4_pop_geo <- nerc_geojson %>%
  inner_join(event_4_pop_df, by = c("REGIONS"))

# Prepare facet data
event_4_pop_pivoted <- event_4_pop_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_4_pop_plot = create_sf_faceted_plot(
  data = event_4_pop_pivoted,
  state_shp_filtered,
  legend_title = "Population\n(Mn)",
  plot_title = "(A) Population affected in four grid failure scenarios",
  plot_subtitle = "Heterogeneous distribution of population affected in the selected percentiles across grid regions",
  group_labels = group_pop_labels
)

event_4_pop_plot

### Establishment Event 4 ###

# Apply the updated function to event_4_gdp_df
event_4_est_df <-
  create_grouped_cols_v2(event_4_est_df, cols_to_group, est_groups, group_est_labels)

#  Merge with FERC GeoJSON
event_4_est_geo <- nerc_geojson %>%
  inner_join(event_4_est_df, by = c("REGIONS"))

# Prepare facet data
event_4_est_pivoted <- event_4_est_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")

event_4_est_plot = create_sf_faceted_plot(
  data = event_4_est_pivoted,
  state_shp_filtered,
  legend_title = "Count\n(1,000)",
  plot_title = "(B) Business establishments",
  plot_subtitle = "Scenarios that may affect businesses within one standard deviation, two standard deviations, and the mean in the distribution",
  group_labels = group_est_labels
)

event_4_est_plot

combined_event_4_plot <- ggarrange(
  event_4_pop_plot, 
  event_4_est_plot, 
  event_4_gdp_plot,
  ncol = 1, 
  nrow = 3
)

print(combined_event_4_plot)

file_path = file.path(folder,"figures", "stacked_event_4_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_event_4_plot, dpi = 300, width=8, height=4.5, bg="#f2f2f2")  # Adjust dpi for image quality

### GDP Event 6 ###

# File paths and data directory
event_6_data_dir = file.path(data_directory, 'events', '6')
event_6_pop_df <- read.csv(file.path(event_6_data_dir, 'event_6_pop_affected.csv')) # Read pop data
event_6_est_df <- read.csv(file.path(event_6_data_dir, 'event_6_est_by_regions.csv')) # Read establishment data
event_6_gdp_df <- read.csv(file.path(event_6_data_dir, 'event_6_gdp_by_regions.csv')) # Read GDP data

# Apply the function to your dataframes
event_6_pop_df <- replace_zeros_with_NA(event_6_pop_df, cols_to_group)
event_6_est_df <- replace_zeros_with_NA(event_6_est_df, cols_to_group)
event_6_gdp_df <- replace_zeros_with_NA(event_6_gdp_df, cols_to_group)

# Apply the updated function to event_6_gdp_df
event_6_gdp_df <-
  create_grouped_cols_v2(event_6_gdp_df, cols_to_group, gdp_groups, group_gdp_labels)

#  Merge with FERC GeoJSON
event_6_gdp_geo <- nerc_geojson %>%
  inner_join(event_6_gdp_df, by = c("REGIONS"))

# Prepare facet data
event_6_gdp_pivoted <- event_6_gdp_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_6_gdp_plot = create_sf_faceted_plot(
  data = event_6_gdp_pivoted,
  state_shp_filtered,
  legend_title = "GDP\n(US $Bn)",
  plot_title = "(C) Direct economic impact",
  plot_subtitle = "Distribution of GDP shock in events leading to blackout in six grid regions",
  group_labels = group_gdp_labels
)

event_6_gdp_plot

### Population Event 6 ###

# Apply the updated function to event_6_gdp_df
event_6_pop_df <-
  create_grouped_cols_v2(event_6_pop_df, cols_to_group, pop_groups, group_pop_labels)

#  Merge with FERC GeoJSON
event_6_pop_geo <- nerc_geojson %>%
  inner_join(event_6_pop_df, by = c("REGIONS"))

# Prepare facet data
event_6_pop_pivoted <- event_6_pop_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_6_pop_plot = create_sf_faceted_plot(
  data = event_6_pop_pivoted,
  state_shp_filtered,
  legend_title = "Population\n(Mn)",
  plot_title = "(A) Population affected in six grid failure scenarios",
  plot_subtitle = "Heterogeneous distribution of population affected in the selected percentiles across grid regions",
  group_labels = group_pop_labels
)

event_6_pop_plot

### Establishment Event 6 ###

# Apply the updated function to event_6_gdp_df
event_6_est_df <-
  create_grouped_cols_v2(event_6_est_df, cols_to_group, est_groups, group_est_labels)

#  Merge with FERC GeoJSON
event_6_est_geo <- nerc_geojson %>%
  inner_join(event_6_est_df, by = c("REGIONS"))

# Prepare facet data
event_6_est_pivoted <- event_6_est_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")

event_6_est_plot = create_sf_faceted_plot(
  data = event_6_est_pivoted,
  state_shp_filtered,
  legend_title = "Count\n(1,000)",
  plot_title = "(B) Business establishments",
  plot_subtitle = "Scenarios that may affect businesses within one standard deviation, two standard deviations, and the mean in the distribution",
  group_labels = group_est_labels
)

event_6_est_plot

combined_event_6_plot <- ggarrange(
  event_6_pop_plot, 
  event_6_est_plot, 
  event_6_gdp_plot,
  ncol = 1, 
  nrow = 3
)

print(combined_event_6_plot)

file_path = file.path(folder,"figures", "stacked_event_6_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_event_6_plot, dpi = 300, width=8, height=4.5, bg="#f2f2f2")  # Adjust dpi for image quality


### GDP Event 8 ###

# File paths and data directory
event_8_data_dir = file.path(data_directory, 'events', '8')
event_8_pop_df <- read.csv(file.path(event_8_data_dir, 'event_8_pop_affected.csv')) # Read pop data
event_8_est_df <- read.csv(file.path(event_8_data_dir, 'event_8_est_by_regions.csv')) # Read establishment data
event_8_gdp_df <- read.csv(file.path(event_8_data_dir, 'event_8_gdp_by_regions.csv')) # Read GDP data

# Apply the function to your dataframes
event_8_pop_df <- replace_zeros_with_NA(event_8_pop_df, cols_to_group)
event_8_est_df <- replace_zeros_with_NA(event_8_est_df, cols_to_group)
event_8_gdp_df <- replace_zeros_with_NA(event_8_gdp_df, cols_to_group)

# Apply the updated function to event_8_gdp_df
event_8_gdp_df <-
  create_grouped_cols_v2(event_8_gdp_df, cols_to_group, gdp_groups, group_gdp_labels)

#  Merge with FERC GeoJSON
event_8_gdp_geo <- nerc_geojson %>%
  inner_join(event_8_gdp_df, by = c("REGIONS"))

# Prepare facet data
event_8_gdp_pivoted <- event_8_gdp_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_8_gdp_plot = create_sf_faceted_plot(
  data = event_8_gdp_pivoted,
  state_shp_filtered,
  legend_title = "GDP\n(US $Bn)",
  plot_title = "(C) Direct economic impact",
  plot_subtitle = "Distribution of GDP shock in events leading to blackout in eight grid regions",
  group_labels = group_gdp_labels
)

event_8_gdp_plot

### Population Event 8 ###

# Apply the updated function to event_8_gdp_df
event_8_pop_df <-
  create_grouped_cols_v2(event_8_pop_df, cols_to_group, pop_groups, group_pop_labels)

#  Merge with FERC GeoJSON
event_8_pop_geo <- nerc_geojson %>%
  inner_join(event_8_pop_df, by = c("REGIONS"))

# Prepare facet data
event_8_pop_pivoted <- event_8_pop_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")


event_8_pop_plot = create_sf_faceted_plot(
  data = event_8_pop_pivoted,
  state_shp_filtered,
  legend_title = "Population\n(Mn)",
  plot_title = "(A) Population affected in eight grid failure scenarios",
  plot_subtitle = "Heterogeneous distribution of population affected in the selected percentiles across grid regions",
  group_labels = group_pop_labels
)

event_8_pop_plot

### Establishment Event 8 ###

# Apply the updated function to event_8_gdp_df
event_8_est_df <-
  create_grouped_cols_v2(event_8_est_df, cols_to_group, est_groups, group_est_labels)

#  Merge with FERC GeoJSON
event_8_est_geo <- nerc_geojson %>%
  inner_join(event_8_est_df, by = c("REGIONS"))

# Prepare facet data
event_8_est_pivoted <- event_8_est_geo %>%
  pivot_longer(cols = ends_with("_Group"),
               names_to = "Group_Category",
               values_to = "Group_Value")

event_8_est_plot = create_sf_faceted_plot(
  data = event_8_est_pivoted,
  state_shp_filtered,
  legend_title = "Count\n(1,000)",
  plot_title = "(B) Business establishments",
  plot_subtitle = "Scenarios that may affect businesses within one standard deviation, two standard deviations, and the mean in the distribution",
  group_labels = group_est_labels
)

event_8_est_plot

combined_event_8_plot <- ggarrange(
  event_8_pop_plot, 
  event_8_est_plot, 
  event_8_gdp_plot,
  ncol = 1, 
  nrow = 3
)

print(combined_event_8_plot)

file_path = file.path(folder,"figures", "stacked_event_8_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_event_8_plot, dpi = 300, width=8, height=4.5, bg="#f2f2f2")  # Adjust dpi for image quality

dev.off()

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     