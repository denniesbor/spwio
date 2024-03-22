library(tidyverse)
library(readr)
library(tidyr)
library(broom)
library(jsonlite)
library(usmap)
library(dplyr)
library(reshape2)
library(tigris)
library(ggforce)
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

#  Group the variabvkes of interest into 10
create_group_labels <- function(fill_variable) {
  num_groups <- 8
  
  max_value <-
    max(summarised_rto_df[[fill_variable]], na.rm = TRUE)
  min_value <- min(summarised_rto_df[[fill_variable]], na.rm = TRUE)
  
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


#### Visualizations ####
#### 1. Analysis of business data at zcta resolution ####
# color palette
num_groups <- length(unique(summarised_rto_df$REGIONS))
viridis_colors <- viridis::viridis(n = num_groups, direction = -1)

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
          face = "bold",
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
group_info_Total_EMP = create_group_labels("Total_EMP")
group_Total_EMP <- group_info_Total_EMP$groups
labels_Total_EMP <- group_info_Total_EMP$labels

Total_EMP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                              "Total_EMP",
                                              group_Total_EMP,
                                              labels_Total_EMP)

# Plot 1 Employees
plot1 <- create_sf_plot(
  data = Total_EMP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_EMP_Group",
  legend_title = "Count\n(Mn)",
  plot_title = "(C) Count of employees",
  sub_title = "Quanity of employed persons aggregated at Zip code areas",
  group_labels = labels_Total_EMP
)

print(plot1)

# Annual employees compensation
group_info_Total_AP = create_group_labels("Total_AP")
group_Total_AP <- group_info_Total_AP$groups
labels_Total_AP <- group_info_Total_AP$labels

Total_AP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                             "Total_AP",
                                             group_Total_AP,
                                             labels_Total_AP)

# Plot 2 Employee renumeration
plot2 <- create_sf_plot(
  data = Total_AP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_AP_Group",
  legend_title = "Payroll\n($Bn)",
  plot_title = "(B) Annual employee compensation",
  sub_title = " Total remuneration for labor provided by employees",
  group_labels = labels_Total_AP
)

print(plot2)

# Plot 3: Business Establishments in the Region
group_info_Total_EST = create_group_labels("Total_EST")
group_Total_EST <- group_info_Total_EST$groups
labels_Total_EST <- group_info_Total_EST$labels

Total_EST_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                              "Total_EST",
                                              group_Total_EST,
                                              labels_Total_EST)

# Business establishements
plot3 <- create_sf_plot(
  data = Total_EST_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_EST_Group",
  legend_title = "Count\n(1,000)",
  plot_title = "(B) Business establishments",
  sub_title = "Count of establishments at Zip Code Tabulation Areas",
  group_labels = labels_Total_EST
)

print(plot3)

# Plot 4: Population in the Regions
group_info_Total_POP20 = create_group_labels("Total_POP20")
group_Total_POP20 <- group_info_Total_POP20$groups
labels_Total_POP20 <- group_info_Total_POP20$labels

Total_POP20_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                                "Total_POP20",
                                                group_Total_POP20,
                                                labels_Total_POP20)

# Plot 4 Population
plot4 <- create_sf_plot(
  data = Total_POP20_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_POP20_Group",
  legend_title = "Population\n(Mn)",
  plot_title = "(A) 2020 decencial census population",
  sub_title = "Population count aggregated at Zip code tabulation areas in the regions",
  group_labels = labels_Total_POP20
)

print(plot4)

# Plot 5: Regional Aggregation of the State GDP Apportionment by Population
group_info_Total_ZCTAGDP = create_group_labels("Total_ZCTAGDP")
group_Total_ZCTAGDP <- group_info_Total_ZCTAGDP$groups
labels_Total_ZCTAGDP <- group_info_Total_ZCTAGDP$labels

Total_ZCTAGDP_grouped_df = create_grouped_cols_v2(summarised_rto_df,
                                                  "Total_ZCTAGDP",
                                                  group_Total_ZCTAGDP,
                                                  labels_Total_ZCTAGDP)

# Plot 5
plot5 <- create_sf_plot(
  data = Total_ZCTAGDP_grouped_df,
  state_shp_filtered,
  fill_variable = "Total_ZCTAGDP_Group",
  legend_title = "GDP\n($Bn)",
  plot_title = "(D) Regional gross-value added GDP",
  sub_title = "Estimates of GDP contribution by businesses categorized using NAICS codes",
  group_labels = labels_Total_ZCTAGDP
  
)
plot5

# Combine the plots
combined_plot <- ggarrange(plot4, plot3, plot1, plot5,
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
plot6 <- ggplot(filtered_df, aes(x = factor(Event), y = GDPSum, color = factor(Event))) +
  geom_sina(position = position_nudge(x = 0), size = 0.05, alpha = 0.5)+  # Add jittered points in a violin shape
  geom_boxplot(fill = NA, outlier.shape = NA, color = "#000000", size = 0.3, fatten=0.9, alpha = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.1, color="#000000", alpha = 0.8) +
  scale_color_brewer(palette = "Set1", name = "Quantity of Failed\nFERC Regions") +  # Coloring points
  labs(
    title = "(B) Direct economic impact per event scenario",
    subtitle = "Violin jitters indicate distribution of the impacts per event",
    y = "Daily GDP (US $Bn)",
    x = "Simulation Event"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(hjust = 1),
    panel.grid = element_line(color = "white"),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.4),
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
    panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.4),
    panel.background = element_rect(fill = "#f2f2f2"),
    plot.background = element_rect(fill = "#f2f2f2", color=NA),
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
  bg = "#f2f2f2"
)

### Heat Maps of Grid Regions ###

# Load the data with population density and GDP per capita
grid_csv <- read.csv("grid_data.csv")

# Merge datasets on 'REGIONS'
merged_df <- merge(grid_csv, summarised_rto_df, by = "REGIONS")

# Select only the columns of interest
final_df <- merged_df[c("REGIONS", "Total_EMP", "Total_AP", "Total_EST", "Total_POP20", "PopDensity", "GDPHead")]

# Rank the grid regions by population density
final_df <- final_df %>%
  arrange(desc(PopDensity)) %>%
  mutate(rank = row_number()) %>%
  select(REGIONS, PopDensity, GDPHead, Total_EMP, Total_AP, Total_EST, Total_POP20, rank) %>%
  mutate(across(c(Total_EMP, Total_AP, Total_EST, Total_POP20, PopDensity, GDPHead), scales::rescale))

# Reshape the data for plotting
grid_long <- final_df %>%
  pivot_longer(cols = -c(REGIONS, rank), names_to = 'metric', values_to = 'value') %>%
  mutate(REGIONS = factor(REGIONS, levels = rev(unique(final_df$REGIONS))))

# Rename metrics for the plot
metric_labels <- c(PopDensity = "Population\ndensity (sq.Km)", GDPHead = "GDP\nper capita",
                   Total_EMP = "Employee\ncount", Total_AP = "Employees\nTotal pay ($US)",
                   Total_EST = "Business\ncount", Total_POP20 = "Population\n(2020)")

# Grid regions heatmap data
grid_long$metric <- factor(grid_long$metric, levels = names(metric_labels), labels = metric_labels)


# Grid regions ploy from summarized_rto_df
grid_regions_plot <- ggplot(data = summarised_rto_df) +
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
  labs(title = "(A) Transmission Planning Regions", subtitle="The regions are digitized based on FERC Order No.1000 ") +
  theme(
    text = element_text(color = "#22211d"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    legend.position = "right",
    legend.justification = c(0.99, 0.1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face="bold"),
    legend.key.height = unit(0.2, "lines"),
    legend.key.width = unit(1.2, "lines"),
    plot.title = element_text(
      size = 12,
      hjust = 0.05,
      face = "bold",
    ),
    plot.subtitle = element_text(
      hjust = 0.056,
      size = 10,
    ),
  ) +
  coord_sf() +
  theme(text = element_text(family = "Times New Roman"))

grid_regions_plot

# Create the ggplot heatmap
heatmap_plot <- ggplot(grid_long, aes(x = metric, y = REGIONS, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(direction=-1) +
  labs(x = "", y = "",
       title = "(B) Heatmap of electricity grid regions by economic metrics",
       subtitle = "The regions are ranked by population density") +
  theme_minimal() +
  geom_text(aes(label = ifelse(metric == "Population\ndensity (sq.Km)", as.character(rank), "")), color = "#ffffff", size = 2.5) +
  scale_x_discrete(labels = metric_labels) +
  theme(
    plot.title = element_text(size = 12, face = 'bold', hjust = -0.5),
    plot.subtitle = element_text(size = 10, hjust = -0.28),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.x = element_line(color = "#696969", linewidth=0.1),
    axis.text.x = element_text( hjust = 0.5, size=8),
    axis.text.y= element_text(size=8))

heatmap_plot

# Combine the grid regions and heatmap plot
combined_plot_3 <- ggarrange(
    grid_regions_plot,
    heatmap_plot,
    ncol = 1,
    nrow = 2
  )

combined_plot_3

path_out = file.path(folder, 'figures', 'heatmap_and_rtomap.png')
ggsave(
  path_out,
  combined_plot_3,
  dpi = 300,
  width = 8,
  height = 8,
  bg = "#f2f2f2"
)



dev.off()

