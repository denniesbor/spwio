library(tidyverse)
library(readr)
library(tidyr)
library(broom)
library(jsonlite)
library(usmap)
library(dplyr)
library(reshape2)
library(tigris)
library(scales)
library(tidycensus)
library(patchwork)
library(sf)
library(sp)
library(viridis)
library(RColorBrewer)
library(ggpubr)

options(tigris_use_cache = TRUE)

folder = dirname(rstudioapi::getSourceEditorContext()$path)
data_directory = file.path(folder, '..', 'data')
setwd(data_directory)

# FERC GDF
nerc_geojson <- st_read("nerc_gdf.geojson")

# business data at ZCTA levels
df_zcta_stats <- read.csv("df_zcta.csv")

# state gdp, and business establishments at state_levels
df_rto_stats <-
  read.csv("df_rto_stats.csv")

# US State shapefiles
state_shp <-
  tigris::states(cb = TRUE,
                 year = 2020,
                 resolution = "20m")

state_shp <- tigris::shift_geometry(state_shp)
state_shp <- rename(state_shp, STABBR = STUSPS)

# exclude non contigous US States
non_cont_fips_codes <-
  c('02', '15', '72', '66', '60', '69', '78', '78')
state_shp_filtered <- state_shp %>%
  filter(!STATEFP %in% non_cont_fips_codes)

# Set crs
nerc_geojson <- st_transform(nerc_geojson, 'ESRI:102003')
state_shp_filtered <-
  st_transform(state_shp_filtered, 'ESRI:102003')

### Merge the NERC JSON with business data from ZCTA regions ###

merged_nerc_bg <- nerc_geojson %>%
  inner_join(df_rto_stats, by = c("REGION_ID", "REGIONS"))

# groupby the regions of NERC RTOs, and sum  the population, number of business, etc

summarised_zcta_df <- df_zcta_stats %>%
  group_by(REGIONS, REGION_ID) %>%
  summarise(
    Total_EMP = round(sum(EMP, na.rm = TRUE) / 1e6, digits = 4),
    Total_AP = round(sum(AP, na.rm = TRUE) / 1e6, digits = 4),
    Total_EST = round(sum(EST, na.rm = TRUE) / 1e3, digits = 4),
    Total_POP20 = round(sum(POP20, na.rm = TRUE) / 1e6, digits = 4),
    Total_ZCTAGDP = round(sum(ZCTAGDP, na.rm = TRUE) / 365e3, digits = 4)
  )

merged_nerc_zcta <- nerc_geojson %>%
  inner_join(summarised_zcta_df, by = c("REGION_ID", "REGIONS"))

summarised_rto_df <- merged_nerc_zcta %>%
  group_by(REGIONS) %>%
  summarise(
    Total_EMP = sum(Total_EMP, na.rm = TRUE),
    Total_AP = sum(Total_AP, na.rm = TRUE),
    Total_EST = sum(Total_EST, na.rm = TRUE),
    Total_POP20 = sum(Total_POP20, na.rm = TRUE),
    Total_ZCTAGDP = sum(Total_ZCTAGDP, na.rm = TRUE)
  )

print(summarised_rto_df)

# Function to apply cut and create grouped columns
create_grouped_cols_v2 <- function(data, col, breaks, labels) {
  group_col_name <- paste(col, "Group", sep = "_")
  data[[group_col_name]] <-
    cut(
      data[[col]],
      breaks = breaks,
      include.lowest = TRUE,
      labels = labels
    )
  return(data)
}

create_group_labels <- function(fill_variable, ceil_num) {
  num_groups <- 10
  
  max_value <-
    ceiling(max(summarised_rto_df[[fill_variable]], na.rm = TRUE))
  min_value <- min(summarised_rto_df[[fill_variable]], na.rm = TRUE)
  min_value <- floor(min_value / ceil_num) * ceil_num
  # Calculate the interval size, rounding to the nearest multiple of ceil_num
  interval_size <-
    ceiling((max_value - min_value) / num_groups / ceil_num) * ceil_num
  
  print(max_value - min_value)
  
  # Generate the groups
  groups <- seq(min_value, max_value, by = interval_size)
  
  print(groups)
  
  # Adjust the last group to ensure it includes the max_value
  # Adjust the last group if it does not reach max_value
  if (max(groups) < max_value) {
    groups <- c(groups[-length(groups)], max_value)
  }
  
  # Create labels for these groups
  group_labels <-
    paste0(groups[-length(groups)], "-", groups[-length(groups)] + interval_size - 1)
  
  # Ensure the last label correctly reflects the range up to max_value
  group_labels <-
    c(group_labels[-length(group_labels)], paste0(groups[length(groups) - 1], "-", max_value))
  
  return(list(groups = groups, labels = group_labels))
}


#### Visualizations ####
#### 1. Analysis of business data at zcta resolution ####

# Regional Transmission Operators

# color palette
num_groups <- length(unique(summarised_rto_df$REGIONS))
viridis_colors <- viridis::viridis(n = num_groups, direction = -1)

plot1 <- ggplot(data = summarised_rto_df) +
  geom_sf(
    data = state_shp_filtered,
    fill = NA,
    color = "black",
    size = 0.25,
    linewidth = 0.15
  ) +
  geom_sf(
    aes(fill = as.factor(REGIONS)),
    color = "white",
    linewidth = 0.5,
    size = 0.25,
    alpha = 0.9
  ) +
  theme_void() +
  scale_fill_manual(
    values = viridis_colors,
    name = "Transmission Regions",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0,
      label.hjust = .0,
      keywidth = 1.2,
      keyheight = 0.7,
      direction = "vertical"
    )
  ) +
  labs(title = "FERC Order No.1000 Transmission Planning Regions") +
  theme(
    text = element_text(color = "#22211d"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    legend.background = element_rect(
      fill = "#f5f5f2",
      color = "black",
      linetype = "solid"
    ),
    legend.position = c(0.99, 0.03),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.margin = margin(3, 3, 3, 3),
    plot.title = element_text(
      size = 12,
      hjust = 0.01,
      margin = margin(
        b = -0.1,
        t = 0.4,
        l = 2,
        unit = "cm"
      )
    )
  ) +
  coord_sf() +
  theme(text = element_text(family = "Times New Roman"))

plot1

# save plot 1
path_out = file.path(folder, 'figures', 'regional_planning.png')
ggsave(path_out,
       plot1,
       dpi = 300,
       width = 10,
       height = 8)

# Continous variables plots
create_sf_plot <-
  function(data,
           data_2,
           fill_variable,
           legend_title,
           plot_title,
           sub_title = "",
           group_labels) {
    # Create a Brewer color palette
    num_colors <- length(group_labels)
    viridis_colors <-
      viridis::viridis(n = num_colors, direction = -1)
    
    plot <- ggplot(data) +
      geom_sf(
        data = data_2,
        fill = NA,
        color = "dodgerblue",
        linewidth = 0.1,
        alpha = 1
      ) +
      geom_sf(
        aes(fill = .data[[fill_variable]]),
        linewidth = 0.34,
        alpha = 0.8,
        color = "white"
      ) +
      theme_void() +
      scale_fill_manual(
        values = viridis_colors,
        name = legend_title,
        breaks = group_labels,
        limits = group_labels
      ) +
      labs(title = plot_title, subtitle = sub_title) +
      theme(
        text = element_text(color = "#22211d"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        legend.background = element_rect(fill = "#f2f2f2", color = NA),
        plot.title = element_text(
          size = 11,
          hjust = 0.03,
          margin = margin(
            b = 0.2,
            t = 0,
            l = 2,
            unit = "cm"
          )
        ),
        plot.subtitle = element_text(
          hjust = 0.03,
          size = 9,
          margin = margin(
            b = 0.2,
            t = 0,
            l = 2,
            unit = "cm"
          )
        ),
        legend.position = "right",
        legend.direction = "vertical",
        legend.justification = c(0.5, 0.5),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(0.02, "npc"),
      ) +
      coord_sf() +
      theme(text = element_text(family = "Times New Roman"))
    
    return(plot)
  }

# Employees
group_info_Total_EMP = create_group_labels("Total_EMP", ceil_num = 3)
group_Total_EMP <- group_info_Total_EMP$groups
labels_Total_EMP <- group_info_Total_EMP$labels

Total_EMP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                              "Total_EMP",
                                              group_Total_EMP,
                                              labels_Total_EMP)

# Plot 2 Employees
plot2 <- create_sf_plot(
  data = Total_EMP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_EMP_Group",
  legend_title = "Employment (Mn)",
  plot_title = "(C) Count of employees",
  sub_title = "Total number of employed persons within the grid regions",
  group_labels = labels_Total_EMP
)

print(plot2)

# Annual employees compensation
group_info_Total_AP = create_group_labels("Total_AP", ceil_num = 200)
group_Total_AP <- group_info_Total_AP$groups
labels_Total_AP <- group_info_Total_AP$labels

Total_AP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                             "Total_AP",
                                             group_Total_AP,
                                             labels_Total_AP)

# Plot 3 Employee renumeration
plot3 <- create_sf_plot(
  data = Total_AP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_AP_Group",
  legend_title = "Payroll ($Bn)",
  plot_title = "(B) Annual employee compensation",
  sub_title = "Total payments made to employees at electricity grid regions",
  group_labels = labels_Total_AP
)

print(plot3)

# Plot 4: Business Establishments in the Region
group_info_Total_EST = create_group_labels("Total_EST", ceil_num = 100)
group_Total_EST <- group_info_Total_EST$groups
labels_Total_EST <- group_info_Total_EST$labels

Total_EST_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                              "Total_EST",
                                              group_Total_EST,
                                              labels_Total_EST)

# Business establishements
plot4 <- create_sf_plot(
  data = Total_EST_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_EST_Group",
  legend_title = "Establishment (1,000)",
  plot_title = "(B) Total business establishments",
  sub_title = "Business count aggregated by ZCTA within electricity grid regions",
  group_labels = labels_Total_EST
)

print(plot4)

# Plot 5: Population in the Regions
group_info_Total_POP20 = create_group_labels("Total_POP20", ceil_num = 6)
group_Total_POP20 <- group_info_Total_POP20$groups
labels_Total_POP20 <- group_info_Total_POP20$labels

Total_POP20_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                                "Total_POP20",
                                                group_Total_POP20,
                                                labels_Total_POP20)

# Plot 5 Population
plot5 <- create_sf_plot(
  data = Total_POP20_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_POP20_Group",
  legend_title = "Population (Mn)",
  plot_title = "(A) Population in the regions",
  sub_title = " 2020 decencial census population aggregated by ZCTA",
  group_labels = labels_Total_POP20
)

print(plot5)

# Plot 6: Regional Aggregation of the State GDP Apportionment by Population
group_info_Total_ZCTAGDP = create_group_labels("Total_ZCTAGDP", ceil_num =
                                                 1)
group_Total_ZCTAGDP <- group_info_Total_ZCTAGDP$groups
labels_Total_ZCTAGDP <- group_info_Total_ZCTAGDP$labels

Total_ZCTAGDP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                                  "Total_ZCTAGDP",
                                                  group_Total_ZCTAGDP,
                                                  labels_Total_ZCTAGDP)

# Plot 6
plot6 <- create_sf_plot(
  data = Total_ZCTAGDP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_ZCTAGDP_Group",
  legend_title = "GDP ($Bn)",
  plot_title = "(D) Regional gross-value added GDP",
  sub_title = "GDP estimates based on the business count categorized by NAICS",
  group_labels = labels_Total_ZCTAGDP
  
)
plot6

# Combine the plots
combined_plot <- ggarrange(plot5, plot4, plot2, plot6,
                           ncol = 2,
                           nrow = 2)

combined_plot

path_out = file.path(folder, 'figures', 'grid_key_metrics.png')
ggsave(
  path_out,
  combined_plot,
  dpi = 300,
  width = 8,
  height = 5,
  bg = "#f2f2f2"
)

### Scenarios Boxplot ###

scenarios_df = read.csv("scenarios_df.csv")

# Filter dataframe with 2, 4, 6, and 8 events
filtered_df <- scenarios_df[scenarios_df$Event %in% c(2, 4, 6, 8),]

# Calculate means and standard deviations for GDPSum by Event
summary_df <- filtered_df %>%
  group_by(Event) %>%
  summarise(
    mean = mean(GDPSum),
    sd = sd(GDPSum),
    low = mean - 1.96 * sd,
    high = mean + 1.96 * sd
  )

# Box plot
plot6 <-
  ggplot(filtered_df, aes(
    x = factor(Event),
    y = GDPSum,
    fill = factor(Event)
  )) +
  geom_boxplot(outlier.shape = NA) +  # Create boxplot
  stat_boxplot(geom = "errorbar", width = 0.25) +  # Add horizontal lines at the whiskers
  scale_fill_brewer(palette = "Set1", name = "Quantity of Failed\nFERC Regions") +  # Choose a palette
  labs(
    title = "(B) Direct economic impact per scenario",
    subtitle = "Events indicate quanity of failed grid regions",
    y = "Daily GDP (US $Bn)",
    x = "Simulation Event"
  ) +
  theme_minimal() +  # Use minimal theme
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"),
    # Apply Times New Roman font
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(hjust = 1),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    legend.title = element_text(size = 9, face = "bold")
  )

plot6

# Line plot

# Get cumulative percentage of each df
ranked_df <- filtered_df %>%
  group_by(Event) %>%
  mutate(Rank = rank(desc(GDPSum)),  # Rank GDPSum within each group
         Fi = Rank / (n() + 1))

# Ensure 'Event' is a factor
ranked_df <- ranked_df %>%
  mutate(Event = as.factor(Event))

plot7 <-
  ggplot(ranked_df, aes(
    x = GDPSum,
    y = Fi,
    group = Event,
    color = Event
  )) +
  geom_line() +
  scale_color_brewer(palette = "Set1", name = "Quantity of Failed\nFERC Regions") +
  # scale_y_log10(labels = label_log(digits = 3)) +
  labs(
    x = "Total daily GDP impact ($US Bn)",
    y = "Frequency number",
    title = "(A) Distribution of direct impact",
    subtitle = "F-N curves for heterogeneous grid failures"
  ) +
  theme(
    plot.title = element_text(size = 11, face = 'bold'),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    panel.background = element_rect(fill = "#f2f2f2"),
    plot.background = element_rect(fill = "#f2f2f2"),
    legend.background = element_rect(fill = "#f2f2f2"),
    text = element_text(family = "Times New Roman")
  )

plot7

combined_plot_2 <-
  ggarrange(
    plot7,
    plot6,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )

combined_plot_2

path_out = file.path(folder, 'figures', 'scenarios_box.png')
ggsave(
  path_out,
  combined_plot_2,
  dpi = 300,
  width = 8,
  height = 4,
  bg = "white"
)

### Heat Maps of Grid Regions ###

# Load the data
grid_csv <- read.csv("grid_data.csv")

# Rank the grid regions by population density and normalize the metrics
grid_csv <- grid_csv %>%
  arrange(desc(PopDensity)) %>%
  mutate(rank = row_number()) %>%
  select(REGIONS, PopDensity, GDPHead, EST, DailyGDP, POP20) %>%
  mutate(across(-REGIONS, scales::rescale))

# Reshape the data for plotting
grid_long <- grid_csv %>%
  arrange(desc(PopDensity)) %>%
  pivot_longer(cols = -REGIONS, names_to = 'metric', values_to = 'value') %>%
  mutate(REGIONS = factor(REGIONS, levels = unique(REGIONS)))

# Rename metrics for the plot
metric_labels <- c(PopDensity = "Pop density", GDPHead = "GDP per capita", 
                   EST = "Establishment", DailyGDP = "Daily GDP", POP20 = "Population")
grid_long$metric <- factor(grid_long$metric, levels = names(metric_labels), labels = metric_labels)

# Create the ggplot heatmap
heatmap_plot <- ggplot(grid_long, aes(x = REGIONS, y = metric, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(direction=-1) +  # Using viridis color scale
  labs(x = "FER grid regions", y = "",
       title = "Heatmap of electricity grid regions by economic metrics",
       subtitle = "The regions are ranked by population density") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = 'bold'),
    plot.background = element_rect(fill = "#f2f2f2"),
    text = element_text(family = "Times New Roman"), 
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size=9))

heatmap_plot

path_out = file.path(folder, 'figures', 'heat_map.png')
ggsave(
  path_out,
  heatmap_plot,
  dpi = 300,
  width = 8,
  height = 4,
  bg = "white"
)


dev.off()
