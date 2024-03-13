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

## Read the events 2 data ##

# File paths and data directory
event_2_data_dir = file.path(data_directory, 'events', '2')
event_2_pop_path = file.path(event_2_data_dir, 'event_2_pop_affected.csv')
event_2_est_path = file.path(event_2_data_dir, 'event_2_est_by_regions.csv')
event_2_gdp_path = file.path(event_2_data_dir, 'event_2_gdp_by_regions.csv')

# Read pop data
event_2_pop_df <- read.csv(event_2_pop_path)

# Read establishment data
event_2_est_df <- read.csv(event_2_est_path)

# Read GDP data
event_2_gdp_df <- read.csv(event_2_gdp_path)

# List of columns to be grouped
cols_to_group <- c("X.1.96SD", "X.1SD", "Mean", "X1SD", "X1.96SD")

# Function to replace zeros with NA in specified columns
replace_zeros_with_NA <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == 0] <- NA
  }
  return(df)
}

# Apply the function to your dataframes
event_2_pop_df <- replace_zeros_with_NA(event_2_pop_df, cols_to_group)
event_2_est_df <- replace_zeros_with_NA(event_2_est_df, cols_to_group)
event_2_gdp_df <- replace_zeros_with_NA(event_2_gdp_df, cols_to_group)

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
      labels = c("-2SD", "-1SD", "Mean", "1SD", "2SD")
    )
    # Unique groups in the pivoted df
    num_groups <-
      length(unique(data$Group_Value))
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
      scale_fill_manual(values = viridis_colors, name = legend_title) +
      facet_wrap( ~ Group_Category, ncol = length(unique(data$Group_Category))) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      theme_void() +
      theme(
        plot.title = element_text(
          size = 10,
          hjust = 0.01,
          margin = margin(
            b = -0.1,
            t = 0.4,
            l = 2,
            unit = "cm"
          )
        ),
        plot.subtitle = element_text(hjust = 0.01, 
                                     size = 9
                                     ),
        legend.position = "right",
        legend.justification = "center",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 8)
      ) +
      theme(text = element_text(family = "Times New Roman"))
    
    return(plot)
  }

### GDP Event 2 ###

# Make the continuous vars as factos by grouping
gdp_groups <-
  c(0, 3, 6, 9, 15, Inf) # Updated group boundaries
group_labels <- c("0-3", "3-6", "6-9", "9-15", ">15") # Labels for each group

# Apply the updated function to event_2_gdp_df
event_2_gdp_df <-
  create_grouped_cols_v2(event_2_gdp_df, cols_to_group, gdp_groups, group_labels)

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
  plot_title = "(B) Regional GDP",
  plot_subtitle = "Distribution of GDP across grid regions under investigated ranges",
  group_labels = group_labels
)

event_2_gdp_plot

### Population Event 2 ###

# Pop groups
pop_groups <-
  c(0, 10, 20, 30, 45, 60, 70, Inf) # Updated group boundaries
group_labels <- c("0-10", "10-20", "20-30", "30-45", "45-60", "60-70", ">70") # Labels for each group

# Apply the updated function to event_2_gdp_df
event_2_pop_df <-
  create_grouped_cols_v2(event_2_pop_df, cols_to_group, pop_groups, group_labels)

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
  plot_title = "(B) Regional population",
  plot_subtitle = "The subplots indicate heterogeneous distribution of population across grid regions under investigated percentiles",
  group_labels = group_labels
)

event_2_pop_plot

### Establishment Event 2 ###

# Pop groups
est_groups <-
  c(0, 300, 600, 900, 1200, 1500, Inf) # Updated group boundaries
group_labels <- c("0-300", "300-600", "600-900", "900-1200", "1200-1500", ">1500") # Labels for each group

# Apply the updated function to event_2_gdp_df
event_2_est_df <-
  create_grouped_cols_v2(event_2_est_df, cols_to_group, est_groups, group_labels)

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
  group_labels = group_labels
)

event_2_est_plot

combined_plot <- event_2_gdp_plot / event_2_est_plot / event_2_pop_plot
# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 1, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(combined_plot)

file_path = file.path(folder,"figures", "stacked_event_2_metrics.png")

# Save the stacked plots
ggsave(file_path, combined_plot, dpi = 300, width=8, height=4)  # Adjust dpi for image quality














# ## Using combined plots ##
# create_sf_plot <-
#   function(data,
#            data_2,
#            fill_variable,
#            # This should be one of the grouped category columns, e.g., "Mean_Group"
#            legend_title,
#            plot_title) {
#     # Assuming fill_variable is a grouped category column created earlier with values 0 to 5
#     
#     # Create a Viridis color palette for the groups, excluding the 0 group for which grey will be used
#     num_groups <- 5 # Groups 1 to 5
#     viridis_colors <-
#       viridis::viridis_pal(option = "D", direction = -1)(num_groups)
#     
#     # Combine grey for group 0 with the Viridis colors for groups 1 to 5
#     colors <- c("grey", viridis_colors)
#     
#     # Convert fill_variable to factor if it's not already
#     data[[fill_variable]] <- factor(data[[fill_variable]])
#     
#     # Map colors to levels, ensuring that "0" is mapped to grey
#     levels(data[[fill_variable]]) <- c("0", "1", "2", "3", "4", "5")
#     
#     plot <- ggplot(data) +
#       geom_sf(
#         data = data_2,
#         fill = NA,
#         color = "dodgerblue",
#         linewidth = 0.1,
#         alpha = 1
#       ) +
#       geom_sf(
#         aes(fill = .data[[fill_variable]]),
#         linewidth = 0.34,
#         alpha = 0.8,
#         color = "white"
#       ) +
#       theme_void() +
#       scale_fill_manual(values = colors, name = legend_title) +
#       labs(title = plot_title) +
#       theme(
#         text = element_text(color = "#22211d"),
#         plot.margin = margin(0, 0, 0, 0, "cm"),
#         plot.background = element_rect(fill = "#f5f5f2", color = NA),
#         panel.background = element_rect(fill = "#f5f5f2", color = NA),
#         legend.background = element_rect(fill = "#f5f5f2", color = NA),
#         plot.title = element_text(
#           size = 10,
#           hjust = 0.01,
#           margin = margin(
#             b = -0.1,
#             t = 0.4,
#             l = 2,
#             unit = "cm"
#           )
#         ),
#         legend.position = c(0.3, 0.1),
#         legend.justification = c(0.5, 0.5),
#         legend.text = element_text(size = 9),
#         legend.key.height = unit(4, "mm"),
#         legend.key.width = unit(0.05, "npc"),
#         legend.title = element_text(size = 8)  # Adjusting legend title size
#       ) +
#       # guides(fill = guide_colourbar(title.position = 'top', direction = "horizontal")) +
#       coord_sf() +
#       theme(text = element_text(family = "Times New Roman"))
#     
#     return(plot)
#   }
# 
# plot1 = create_sf_plot(
#   data = event_2_gdp_geo,
#   state_shp_filtered,
#   fill_variable = "Mean_Group",
#   legend_title = "GDP",
#   plot_title = "(A) Regional GDP"
# ) + theme(legend.position = "none")
# 
# plot2 = create_sf_plot(
#   data = event_2_gdp_geo,
#   state_shp_filtered,
#   fill_variable = "X.1SD_Group",
#   legend_title = "GDP",
#   plot_title = "(B) Regional GDP"
# ) + theme(legend.position = "none")
# 
# combined_plot <- (plot1 | plot2) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right")
