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
      labels = c("97.5%", "16%", "Mean", "84%", "2.5%")
    )
    # Unique groups in the pivoted df
    num_groups <- length(unique(group_labels))
    viridis_colors <- viridis::viridis(n = num_groups, direction = -1)
    
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
      scale_fill_manual(values = viridis_colors, name = legend_title, breaks=group_labels) +
      facet_wrap( ~ Group_Category, ncol = length(unique(data$Group_Category))) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#f2f2f2", color = NA), # Set super light gray background
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        plot.title = element_text(
          size = 10,
          hjust = 0.01,
          face='bold',
          margin = margin(
            b = -0.1,
            t = 0.4,
            l = 2,
            unit = "cm"
          )
        ),
        plot.subtitle = element_text(hjust = 0.01, 
                                     size = 8
                                     ),
        legend.position = "right",
        legend.justification = "center",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(0.01, "npc"),
      ) +
      theme(text = element_text(family = "Times New Roman"))
    
    return(plot)
  }

## General data properties ##

cols_to_group <- c("X.1.96SD", "X.1SD", "Mean", "X1SD", "X1.96SD") # List of columns to be grouped

# Categorize GDP  Data
gdp_groups <-
  c(0, 2, 4, 6, 8, 10, 12, 15, Inf) # Updated group boundaries
group_gdp_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-15", ">15") # Labels for each group

# Make establishments as factors
est_groups <- c(0, 150, 300, 450, 600, 750, 900, 1050, 1200, 1350, 1500, Inf) # Group boundaries
group_est_labels <- c("0-150", "150-300", "300-450", "450-600", "600-750", "750-900", "900-1050", "1050-1200", "1200-1350", "1350-1500", ">1500") # Labels for each group

# Make population as factors
pop_groups <-
  c(0, 5, 10, 15, 20, 25, 30, 35, 45, 50, 60, 70, Inf) # Updated group boundaries
group_pop_labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-45", "45-50", "50-60", "60-70", ">70") # Labels for each group


##### EVENTS 2 #####

## Read the events 2 data ##
event_2_data_dir = file.path(data_directory, 'events', '2') # 2 power grid failures data dir
event_2_pop_df <- read.csv(file.path(event_2_data_dir, 'event_2_pop_affected.csv')) # Read pop data
event_2_est_df <- read.csv(file.path(event_2_data_dir, 'event_2_est_by_regions.csv')) # Read establishment data
event_2_gdp_df <- read.csv(file.path(event_2_data_dir, 'event_2_gdp_by_regions.csv')) # Read GDP data

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Apply the function to est, pop, and gdp dfs
event_2_pop_df <- replace_zeros_with_NA(event_2_pop_df, cols_to_group)
event_2_est_df <- replace_zeros_with_NA(event_2_est_df, cols_to_group)
event_2_gdp_df <- replace_zeros_with_NA(event_2_gdp_df, cols_to_group)

### GDP EVENT 2 ###

# Categorize the vals
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
  legend_title = "GDP (US $Bn)",
  plot_title = "(C) Regional GDP",
  plot_subtitle = "Distribution of GDP across grid regions under investigated ranges",
  group_labels = group_gdp_labels
)

event_2_gdp_plot

### Population Event 2 ###

# Make the vals in the columns as factors
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
  legend_title = "Population (Mn)",
  plot_title = "(A) Regional population",
  plot_subtitle = "The subplots indicate heterogeneous distribution of population across grid regions under investigated percentiles",
  group_labels = group_pop_labels
)

event_2_pop_plot

### Establishment Event 2 ###

# Categorize the vals in the columns
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
  legend_title = "Establishment (1,000)",
  plot_title = "(B) Count of business establishments",
  plot_subtitle = "The subplots depict the count of businesses under various scenario percentiles",
  group_labels = group_est_labels
)

event_2_est_plot

# Combine the event 2 plots
combined_plot_2 <- event_2_pop_plot/ event_2_est_plot / event_2_gdp_plot
# Adjust the layout
combined_plot_2 <- combined_plot_2 + plot_layout(ncol = 1, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_plot_2 <- combined_plot_2 + plot_annotation(
  title = "Impact of space weather events leading to the failure of two grid regions",
  theme = theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.01, vjust = 0.2,
                              family = "Times New Roman")
  )
)

print(combined_plot_2)

# File path
file_path = file.path(folder,"figures", "stacked_event_2_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_plot_2, dpi = 300, width=8, height=5)  # Adjust dpi for image quality

##########################################################################
##### EVENT 4 #####

## Read the events 4 data ##
event_4_data_dir = file.path(data_directory, 'events', '4') # 4 power grid failures data dir
event_4_pop_df <- read.csv(file.path(event_4_data_dir, 'event_4_pop_affected.csv')) # Read pop data
event_4_est_df <- read.csv(file.path(event_4_data_dir, 'event_4_est_by_regions.csv')) # Read establishment data
event_4_gdp_df <- read.csv(file.path(event_4_data_dir, 'event_4_gdp_by_regions.csv')) # Read GDP data

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Apply the function to est, pop, and gdp dfs
event_4_pop_df <- replace_zeros_with_NA(event_4_pop_df, cols_to_group)
event_4_est_df <- replace_zeros_with_NA(event_4_est_df, cols_to_group)
event_4_gdp_df <- replace_zeros_with_NA(event_4_gdp_df, cols_to_group)

### GDP EVENT 4 ###

# Categorize the vals
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
  legend_title = "GDP (US $Bn)",
  plot_title = "(C) Regional GDP",
  plot_subtitle = "Distribution of GDP across grid regions under investigated ranges",
  group_labels = group_gdp_labels
)

event_4_gdp_plot

### Population EVENT 4 ###

# Make the vals in the columns as factors
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
  legend_title = "Population (Mn)",
  plot_title = "(A) Regional population",
  plot_subtitle = "The subplots indicate heterogeneous distribution of population across grid regions under investigated percentiles",
  group_labels = group_pop_labels
)

event_4_pop_plot


### Establishment EVENT 4 ###

# Categorize the vals in the columns
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
  legend_title = "Establishment (1,000)",
  plot_title = "(B) Count of business establishments",
  plot_subtitle = "The subplots depict the count of businesses under various scenario percentiles",
  group_labels = group_est_labels
)

event_4_est_plot

# Combine the EVENT 4 plots
combined_plot_event_4 <- event_4_pop_plot/ event_4_est_plot / event_4_gdp_plot
# Adjust the layout
combined_plot_event_4 <- combined_plot_event_4 + plot_layout(ncol = 1, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_plot_event_4 <- combined_plot_event_4 + plot_annotation(
  title = "Impact of space weather events leading to the failure of four grid regions",
  theme = theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.01, vjust = 0.2,
                              family = "Times New Roman")
  )
)

print(combined_plot_event_4)

# File path
file_path_event_4 = file.path(folder,"figures", "stacked_event_4_metrics.png")

# Save the stacked plots
ggsave(file_path_event_4, combined_plot_event_4, dpi = 300, width=8, height=5)  # Adjust dpi for image quality

##########################################################################
##### EVENTS 6 #####

## Read the events 6 data ##
event_6_data_dir = file.path(data_directory, 'events', '6') # 6 power grid failures data dir
event_6_pop_df <- read.csv(file.path(event_6_data_dir, 'event_6_pop_affected.csv')) # Read pop data
event_6_est_df <- read.csv(file.path(event_6_data_dir, 'event_6_est_by_regions.csv')) # Read establishment data
event_6_gdp_df <- read.csv(file.path(event_6_data_dir, 'event_6_gdp_by_regions.csv')) # Read GDP data

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Apply the function to est, pop, and gdp dfs
event_6_pop_df <- replace_zeros_with_NA(event_6_pop_df, cols_to_group)
event_6_est_df <- replace_zeros_with_NA(event_6_est_df, cols_to_group)
event_6_gdp_df <- replace_zeros_with_NA(event_6_gdp_df, cols_to_group)

### GDP EVENTS 6 ###

# Categorize the vals
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
  legend_title = "GDP (US $Bn)",
  plot_title = "(C) Regional GDP",
  plot_subtitle = "Distribution of GDP across grid regions under investigated ranges",
  group_labels = group_gdp_labels
)

event_6_gdp_plot

### Population EVENTS 6 ###

# Make the vals in the columns as factors
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
  legend_title = "Population (Mn)",
  plot_title = "(A) Regional population",
  plot_subtitle = "The subplots indicate heterogeneous distribution of population across grid regions under investigated percentiles",
  group_labels = group_pop_labels
)

event_6_pop_plot

### Establishment EVENTS 6 ###

# Categorize the vals in the columns
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
  legend_title = "Establishment (1,000)",
  plot_title = "(B) Count of business establishments",
  plot_subtitle = "The subplots depict the count of businesses under various scenario percentiles",
  group_labels = group_est_labels
)

event_6_est_plot

# Combine the EVENTS 6 plots
combined_plot_event_6 <- event_6_pop_plot/ event_6_est_plot / event_6_gdp_plot
# Adjust the layout
combined_plot_event_6 <- combined_plot_event_6 + plot_layout(ncol = 1, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_plot_event_6 <- combined_plot_event_6 + plot_annotation(
  title = "Impact of space weather events leading to the failure of six grid regions",
  theme = theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.01, vjust = 0.2,
                              family = "Times New Roman")
  )
)

print(combined_plot_event_6)

# File path
file_path_event_6 = file.path(folder,"figures", "stacked_event_6_metrics.png")

# Save the stacked plots
ggsave(file_path_event_6, combined_plot_event_6, dpi = 300, width=8, height=5)  # Adjust dpi for image quality


##########################################################################
##### EVENTS 8 #####

## Read the EVENTS 8 data ##
event_8_data_dir = file.path(data_directory, 'events', '8') # 8 power grid failures data dir
event_8_pop_df <- read.csv(file.path(event_8_data_dir, 'event_8_pop_affected.csv')) # Read pop data
event_8_est_df <- read.csv(file.path(event_8_data_dir, 'event_8_est_by_regions.csv')) # Read establishment data
event_8_gdp_df <- read.csv(file.path(event_8_data_dir, 'event_8_gdp_by_regions.csv')) # Read GDP data

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Apply the function to est, pop, and gdp dfs
event_8_pop_df <- replace_zeros_with_NA(event_8_pop_df, cols_to_group)
event_8_est_df <- replace_zeros_with_NA(event_8_est_df, cols_to_group)
event_8_gdp_df <- replace_zeros_with_NA(event_8_gdp_df, cols_to_group)

### GDP EVENTS 8 ###

# Categorize the vals
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
  legend_title = "GDP (US $Bn)",
  plot_title = "(C) Regional GDP",
  plot_subtitle = "Distribution of GDP across grid regions under investigated ranges",
  group_labels = group_gdp_labels
)

event_8_gdp_plot

### Population EVENTS 8 ###

# Make the vals in the columns as factors
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
  legend_title = "Population (Mn)",
  plot_title = "(A) Regional population",
  plot_subtitle = "The subplots indicate heterogeneous distribution of population across grid regions under investigated percentiles",
  group_labels = group_pop_labels
)

event_8_pop_plot

### Establishment EVENTS 8 ###

# Categorize the vals in the columns
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
  legend_title = "Establishment (1,000)",
  plot_title = "(B) Count of business establishments",
  plot_subtitle = "The subplots depict the count of businesses under various scenario percentiles",
  group_labels = group_est_labels
)

event_8_est_plot

# Combine the EVENTS 8 plots
combined_plot_event_8 <- event_8_pop_plot/ event_8_est_plot / event_8_gdp_plot
# Adjust the layout
combined_plot_event_8 <- combined_plot_event_8 + plot_layout(ncol = 1, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_plot_event_8 <- combined_plot_event_8 + plot_annotation(
  title = "Impact of space weather events leading to the failure of eight grid regions",
  theme = theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.01, vjust = 0.2,
                              family = "Times New Roman")
  )
)

print(combined_plot_event_8)

# File path
file_path_event_8 = file.path(folder,"figures", "stacked_event_8_metrics.png")

# Save the stacked plots
ggsave(file_path_event_8, combined_plot_event_8, dpi = 300, width=8, height=5)  # Adjust dpi for image quality

dev.off()








