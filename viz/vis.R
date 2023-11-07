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

# NERC GDF
nerc_geojson <- st_read("nerc_gdf.geojson")

# business data at ZCTA levels
df_zcta_stats <- read.csv("df_zcta.csv")

# state gdp, and business establishments at state_levels
df_rto_stats <-
  read.csv("df_rto_stats.csv")

# US State Data From Tigris
# Get the congressional districts
state_shp <-
  tigris::states(cb = TRUE,
                 year = 2020,
                 resolution = "20m")

state_shp <- tigris::shift_geometry(state_shp)
state_shp <- rename(state_shp, STABBR = STUSPS)

# exclude non contigous US States
non_cont_fips_codes <- c('02', '15', '72', '66', '60', '69', '78', '78')
state_shp_filtered <- state_shp %>% 
  filter(!STATEFP %in% non_cont_fips_codes)

# st_write(state_shp_filtered, 'state_shp_filtered.geojson', driver = 'GeoJSON')

### Merge the NERC JSON with business data from ZCTA regions ###

merged_nerc_zcta <- nerc_geojson %>%
  inner_join(df_zcta_stats, by = c("REGION_ID", "REGIONS"))

merged_nerc_bg <- nerc_geojson %>%
  inner_join(df_rto_stats, by = c("REGION_ID", "REGIONS"))

# groupby the regions of NERC RTOs, and sum  the population, number of business, etc

summarised_rto_df <- merged_nerc_zcta %>%
  group_by(REGIONS) %>%
  summarise(
    Total_EMP = round(sum(EMP, na.rm = TRUE) / 1e6, digits = 0),
    Total_AP = round(sum(AP, na.rm = TRUE) / 1e6, digits = 0),
    Total_EST = round(sum(EST, na.rm = TRUE) / 1e4, digits = 0),
    Total_POP20 = round(sum(POP20, na.rm = TRUE) / 1e6, digits = 0),
    Total_ZCTAGDP = round(sum(ZCTAGDP, na.rm = TRUE) / 1e3, digits = 0)
  )

print(summarised_rto_df)

#### Visualizations ####
#### 1. Analysis of business data at zcta resolution ####

# Regional Transmission Operators
# color palette
colors <-
  colorRampPalette(brewer.pal(9, "Set1"))(length(unique(summarised_rto_df$REGIONS)))

plot1 <- ggplot(data = summarised_rto_df) +
  geom_sf(data = state_shp_filtered, fill = NA, color = "black", size = 0.25, linewidth=0.1) +
  geom_sf(
    aes(fill = as.factor(REGIONS)),
    color = "white",
    linewidth = 0.5,
    size = 0.25,
    alpha = 0.9
  ) +
  theme_void() +
  scale_fill_manual(
    values = colors,
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
  labs(title = "(A) FERC Order No.1000 Transmission Planning Regions") +
  theme(
    text = element_text(color = "#22211d"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(
      fill = "#f5f5f2",
      color = "black",
      linetype = "solid"
    ),
    legend.position = c(0.95,-0.1),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.margin = margin(3, 3, 3, 3),
    plot.title = element_text(
      size = 10,
      hjust = 0.01,
      margin = margin(
        b = -0.1,
        t = 0.4,
        l = 2,
        unit = "cm"
      )
    )
  ) +
  coord_sf()
print(plot1)

# Continous variables plots

# color ramp
brewer_color_ramp <- colorRampPalette(brewer.pal(9, "YlOrRd"))
num_colors <- length(unique(summarised_rto_df$REGIONS))


create_sf_plot <-
  function(data,
           data_2,
           fill_variable,
           legend_title,
           plot_title) {
    plot <- ggplot(data) +
      geom_sf(data = data_2, fill = NA, color = "black", size = 0.25, linewidth=0.1) +
      geom_sf(aes(fill = .data[[fill_variable]]),
              linewidth = 0.5,
              alpha=0.9,
              color = "white") +
      theme_void() +
      scale_fill_gradientn(colors = brewer_color_ramp(num_colors),
                           name = legend_title) +
      labs(title = plot_title) +
      theme(
        text = element_text(color = "#22211d"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
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
        legend.position = c(0.3, 0.1),
        legend.justification = c(0.5, 0.5),
        legend.text = element_text(size = 9),
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(0.05, "npc"),
        legend.title = element_text(size = 8)  # Adjusting legend title size
      ) +
      guides(fill = guide_colourbar(title.position = 'top', direction = "horizontal")) +
      coord_sf()
    
    return(plot)
  }

# Plot 1 Employees
plot2 <- create_sf_plot(
  data = summarised_rto_df,
  state_shp_filtered,
  fill_variable = "Total_EMP",
  legend_title = "Employment (Millions)",
  plot_title = "(A) Total Number of Employees in the Regions"
)

print(plot2)

# Plot 2: Choropleth Total Annual Payroll Per Region
plot3 <- create_sf_plot(
  data = summarised_rto_df,
  fill_variable = "Total_AP",
  legend_title = "Payroll ($B)",
  plot_title = "(B) Annual Employee Payroll"
)

print(plot3)

# Plot 3: Business Establishments in the Region
plot4 <- create_sf_plot(
  data = summarised_rto_df,
  fill_variable = "Total_EST",
  legend_title = "Establishment (10000)",
  plot_title = "(C) Total Number of Business Establishments"
)

print(plot4)

# Plot 4: Population in the Regions
plot5 <- create_sf_plot(
  data = summarised_rto_df,
  fill_variable = "Total_POP20",
  legend_title = "Consumers (Millions)",
  plot_title = "(D) Total Number of Consumers in the Regions"
)

print(plot5)

# Plot 4: Regional Aggregation of the State GDP Apportionment by Population
plot6 <- create_sf_plot(
  data = summarised_rto_df,
  fill_variable = "Total_ZCTAGDP",
  legend_title = "GDP ($B)",
  plot_title = "(A) Regional Aggregation of Apportionment of STATE GDP by Population"
)

print(plot6)

# save plot 1
path_out = file.path(folder, 'figures', 'regional_planning.png')
ggsave(path_out,
       plot1,
       width = 10,
       height = 8)

# save the plots
combined_plot <- plot2 + plot3 + plot4 + plot5
# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 2, nrow = 2) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(combined_plot)
path_out = file.path(folder, 'figures', 'emp_est_ap.png')
ggsave(path_out,
       combined_plot,
       width = 10.5,
       height = 7.5)

# save plot 6
path_out = file.path(folder, "figures", 'population_based_gdp.png')
ggsave(path_out,
       plot6,
       width = 10,
       height = 8)


#### Visualizations at state level ####

# Plot 6 State Employees
plot6 <- create_sf_plot_st(
  data = grouped_df,
  fill_variable = "Total_EMP",
  legend_title = "Employment (Millions)",
  plot_title = "(A) Total Number of Employees in the Regions"
)

precision <- min(st_precision(state_shp_filtered), st_precision(nerc_geojson))
state_shp_filtered <- st_set_precision(state_shp_filtered, precision)
nerc_geojson <- st_set_precision(nerc_geojson, precision)

# Step 3: make geometries valid
state_shp_filtered <- st_make_valid(state_shp_filtered)
nerc_geojson <- st_make_valid(nerc_geojson)

# Step 4: Simplify geometries with the same tolerance (if necessary)
tolerance <- 0.01 # Choose an appropriate tolerance for your data
state_shp_filtered <- st_simplify(state_shp_filtered, dTolerance = tolerance)
nerc_geojson <- st_simplify(nerc_geojson, dTolerance = tolerance)

test_plot <- ggplot() +
  geom_sf(data = state_shp_filtered, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = nerc_geojson, fill = 'lightblue', color = "black", size = 0.25)
print(test_plot)
