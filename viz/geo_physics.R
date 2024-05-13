library(tidyverse)
library(readr)
library(tidyr)
library(broom)
library(jsonlite)
library(usmap)
library(dplyr)
library(tigris)
library(ggmap)
library(osmdata)
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
geophysics_dir = file.path(data_directory, "geophysics")
setwd(data_directory)

# FERC shape files
nerc_geojson <- st_read("ferc_boundaries.geojson")

# Transmission line shapefiles
tl_geojson <- st_read(file.path(geophysics_dir, "merged_line_gdf.geojson"))

# Transformer nodes shapefiles
ss_geojson <- st_read(file.path(geophysics_dir, "merged_node_gdf.geojson"))

# Set crs
nerc_geojson <- st_transform(nerc_geojson, 'EPSG:4326')
ss_geojson <- st_transform(ss_geojson, 'EPSG:4326')
tl_geojson <- st_transform(tl_geojson, 'EPSG:4326')
pjm_geojson <- filter(nerc_geojson, REGIONS == 'PJM')

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

state_shp_filtered <- st_transform(state_shp_filtered, 'EPSG:4326')
bounds <- st_bbox(pjm_geojson, crs = st_crs(state_shp_filtered))

subset_data <- st_intersection(state_shp_filtered, st_as_sfc(bounds))

# View the result to confirm the subset operation
plot(st_geometry(pjm_geojson))
# Plot the subsetted data on top in red
plot(st_geometry(subset_data), add = TRUE)


register_google(key = "")

# Define the bounding box
bbox <- c(left = -91, bottom = 36, right = -72, top = 43)

# Fetch the map
map <- get_map(location = bbox, source = "google", maptype = "terrain")

# Plot the map
ggmap_plot <- ggmap(map) +
  geom_sf(data = pjm_geojson, color = 'red', size = 1, inherit.aes = FALSE) +
  ggtitle("PJM GeoJSON Overlay")

# Print the plot
print(ggmap_plot)

# Create a faceted sf plots
create_sf_plot <-
  function(data,
           boundary_data,
           overlay_data,
           legend_title,
           plot_title,
           plot_subtitle,
           factor_col,
           labels, levels, sizing_vals) {
    
    num_groups <- length(unique(labels))
    viridis_colors <- viridis::viridis(n = num_groups, direction = 1, option = "plasma")
    
    plot2 <- ggplot(data = data) +
      geom_sf(data = boundary_data, color = "gray", size = 0.2, fill = NA, alpha=0.8) +
      geom_sf(data = overlay_data, color = "blue", linewidth=0.4, fill = NA) +
      geom_sf(aes(size = !!sym(factor_col), color = !!sym(factor_col))) +
      scale_size_manual(values = sizing_vals, name = legend_title, breaks = labels, labels = labels) +
      scale_color_manual(values = viridis_colors, name = legend_title, breaks=labels, labels = labels) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        plot.title = element_text(size = 10, hjust = 0.01, face = 'bold', margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.01, size = 8),
        legend.position = "right",
        legend.justification = "center",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(0.01, "npc"),
        text = element_text(family = "Times New Roman")
      )
    
    return(plot2)
  }

# Define failure probability levels
failure_levels <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
failure_labels <- c("00-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                    "50-60%", "60-80%", "80-100%")

sizing_vals = setNames((1:9), failure_labels)

ss_geojson$factor_probability_failure <- cut(ss_geojson$probability_failure, 
                                             breaks = failure_levels,
                                             labels = failure_labels,
                                             include.lowest = TRUE)

failure_probability_plot <- create_sf_plot(ss_geojson,
                                           subset_data,
                                           pjm_geojson,
                                           "Probability",
                                           "(C) Substation Failure Probabilities",
                                           "The probability of the Substation Failure during Peak Halloween storm",
                                           "factor_probability_failure",
                                           failure_labels, failure_levels, sizing_vals)


print(failure_probability_plot)

# Define transformer count levels and labels
transformer_count <- c(0, 2, 3, 4, 5, 6, 7, 8, 10)
transformer_count_lbls <- c("0-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-10")

# Define sizing values from 2 to 12 in steps of 2
sizing_vals <- setNames((1:9), transformer_count_lbls)  # Multiplying each index by 2 to create steps

# Convert transformer counts in your data to factors with defined levels and labels
ss_geojson$factor_transformer_count <- cut(ss_geojson$tranformer_count,
                                              breaks = transformer_count,
                                              labels = transformer_count_lbls,
                                              include.lowest = TRUE)

# Assuming create_sf_plot is already defined and correctly set up to handle these parameters
transformer_count_plot <- create_sf_plot(ss_geojson,
                                         subset_data,
                                         pjm_geojson,
                                         "Transformer Count",
                                         "(A) Quantity of Transformers",
                                         "The number of Transformers in the Substation",
                                         "factor_transformer_count",
                                         transformer_count_lbls, transformer_count, sizing_vals)

# Print the plot
print(transformer_count_plot)


# Define GIC vals flowing in the transformer
gic_levels <- c(0, 2, 4, 10, 50, 100, 200, 300, 450)
gic_labels <- c("0-2", "2-4", "4-10", "10-50", "50-100", 
                    "100-200", "200-300", "300-450")

sizing_vals <- setNames(1*(1:9), gic_labels)

ss_geojson$factor_gic_approx<- cut(ss_geojson$GIC_Approx, 
                                             breaks = gic_levels,
                                             labels = gic_labels,
                                             include.lowest = TRUE)

gic_plot <- create_sf_plot(ss_geojson,
                                           subset_data,
                                           pjm_geojson,
                                           "GIC (A)",
                                           "(B) Quantity of GIC ",
                                           "Peak GIC Flowing During 2003 Halloween Storm",
                                           "factor_gic_approx",
                           gic_labels, gic_levels, sizing_vals)


print(gic_plot)


# Plot EST, GDP, and Substations
# Population levels and labels for 8 categories
pop_levels <- seq(0, 2409253, length.out = 9)  # Creates 8 equal intervals
pop_labels <- c("0-301K", "301-602K", "602-903K", "903-1.2M", 
                "1.2-1.5M", "1.5-1.8M", "1.8-2.1M", "2.1-2.4M")



# Establishments (from 0 to 57893)
est_levels <- seq(0, 57893, length.out = 9)  # Creates 8 equal intervals
est_labels <- c("0-7.2K", "7.2-14.5K", "14.5-21.7K", "21.7-28.9K",
                "28.9-36.1K", "36.1-43.4K", "43.4-50.6K", "50.6-57.9K")


# GDP levels and labels for 8 categories, max ia 145199
gdp_levels <- seq(0, 145200, length.out = 9)  # Creates 8 equal intervals
gdp_labels <- c("0-18.1K", "18.1-36.3K", "36.3-54.4K", "54.4-72.6K", 
                "72.6-90.7K", "90.7-108.9K", "108.9-127K", "127-145.2K")


# Applying cut function to each variable in your dataset
ss_geojson$factor_pop20 <- cut(ss_geojson$POP20, 
                               breaks = pop_levels, 
                               labels = pop_labels, 
                               include.lowest = TRUE)

ss_geojson$factor_establishment <- cut(ss_geojson$EST, 
                                       breaks = est_levels, 
                                       labels = est_labels, 
                                       include.lowest = TRUE)

ss_geojson$factor_gdp2022 <- cut(ss_geojson$GDP2022, 
                                 breaks = gdp_levels, 
                                 labels = gdp_labels, 
                                 include.lowest = TRUE)

# Population
sizing_vals <- setNames(1*(1:9), pop_labels)
pop20_plot <- create_sf_plot(ss_geojson,
                             subset_data,
                             pjm_geojson,
                             "Population",
                             "(D) Population Served",
                             "Number of Customers Served by the Substation",
                             "factor_pop20",
                             pop_labels, pop_levels, sizing_vals)

print(pop20_plot)

# Count of Est
sizing_vals <- setNames(1*(1:9), est_labels)
est_plot <- create_sf_plot(ss_geojson,
                           subset_data,
                           pjm_geojson,
                           "Count",
                           "(E) Businesses Served",
                           "Count of Business Establishments Served by the Power Substation",
                           "factor_establishment",
                           est_labels, est_levels, sizing_vals)

print(est_plot)

# GDP Plot
sizing_vals <- setNames(1*(1:9), gdp_labels)
gdp_plot <- create_sf_plot(ss_geojson,
                           subset_data,
                           pjm_geojson,
                           "GDP ($M)",
                           "(F) GDP estimates of the businesses served",
                           "Total GDP of the establishments by NAICs served by the substation",
                           "factor_gdp2022",
                           gdp_labels, gdp_levels, sizing_vals)

print(gdp_plot)

# Combine the 6 Plots
combined_plot_1 <- transformer_count_plot/ gic_plot / failure_probability_plot / pop20_plot / est_plot / gdp_plot
# Adjust the layout
combined_plot_1 <- combined_plot_1 + plot_layout(ncol = 2, nrow = 3) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
print(combined_plot_1)
# File path
file_path_1 = file.path(folder,"figures", "geophysical_1.png")
# Save the stacked plots
ggsave(file_path_1, combined_plot_1, dpi = 300, width=8, height=8)

### Plot Transmission Lines Data ###
create_tl_plot <-
  function(data,
           boundary_data,
           overlay_data,
           legend_title,
           plot_title,
           plot_subtitle,
           factor_col,
           labels, levels, sizing_vals) {
    
    num_groups <- length(unique(labels))
    viridis_colors <- viridis::viridis(n = num_groups, direction = -1, option = "plasma")
    
    plot2 <- ggplot(data = data) +
      geom_sf(data = boundary_data, color = "gray", size = 0.2, fill = NA, alpha=0.8) +
      geom_sf(data = overlay_data, color = "blue", linewidth=0.4, fill = NA) +
      geom_sf(aes(linetype = "solid", color = !!sym(factor_col))) +
      scale_color_manual(values = viridis_colors, name = legend_title, breaks=labels, labels = labels) +
      labs(title = plot_title, subtitle = plot_subtitle) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        plot.title = element_text(size = 10, hjust = 0.01, face = 'bold', margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.01, size = 8),
        legend.position = "right",
        legend.justification = "center",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(0.01, "npc"),
        text = element_text(family = "Times New Roman")
      )
    
    return(plot2)
  }


# CREATE LABELS AND LEVELS

# E_field levels and labels (from 0 to 5.2)
e_field_levels <- seq(0, 5.2, length.out = 9)  # Creates 8 equal intervals
e_field_labels <- paste(round(head(e_field_levels, -1), 1), round(tail(e_field_levels, -1), 1), sep="-")

# Voltage levels and labels (from 0 to 199)
voltage_levels <- seq(0, 199, length.out = 9)  # Creates 8 equal intervals
voltage_labels <- paste(round(head(voltage_levels, -1)), round(tail(voltage_levels, -1)), sep="-")

# Current sources assuming ohmic r of 0.05 Ohms/km
i_levels <- seq(0, 103, length.out = 9)  # Creates 8 equal intervals
i_labels <- paste(round(head(i_levels, -1)), round(tail(i_levels, -1)), sep="-")

# Create factors
tl_geojson$factor_e_field <- cut(tl_geojson$E_field,
                                 breaks = e_field_levels,
                                 labels = e_field_labels,
                                 include.lowest = TRUE)

tl_geojson$factor_voltage <- cut(tl_geojson$voltage,
                                 breaks = voltage_levels,
                                 labels = voltage_labels,
                                 include.lowest = TRUE)

tl_geojson$factor_i <- cut(tl_geojson$current_sources,
                           breaks = i_levels,
                           labels = i_labels,
                           include.lowest = TRUE)

unique_voltages <- sort(unique(tl_geojson$VOLTAGE))  # Retrieve and sort unique voltage values

# Create labels based on these unique values
voltage_class_labels <- paste(unique_voltages, "kV")
voltage_class_levels <- c(unique_voltages, max(unique_voltages) + 1)

# Categorize voltage in tl_geojson using the cut function
tl_geojson$factor_voltage_class <- cut(tl_geojson$VOLTAGE,
                                 breaks = voltage_class_levels,
                                 labels = voltage_class_labels,
                                 include.lowest = TRUE, right = FALSE)

# Plot 1: Unique Voltages
sizing_vals <- setNames(1*(1:9), voltage_class_labels)
v_class_plot <- create_sf_plot(tl_geojson,
                               subset_data,
                               pjm_geojson,
                               "Class (kV)",
                               "(A) Voltage Category",
                               "Transmission Line Voltage Class",
                               "factor_voltage_class",
                               voltage_class_labels, voltage_class_levels, sizing_vals)

print(v_class_plot)

# Plot 2: E Fields
sizing_vals <- setNames(1*(1:9), e_field_labels)
e_field_plot <- create_sf_plot(tl_geojson,
                               subset_data,
                               pjm_geojson,
                               "E (V/kM)",
                               "(B) Geoelectric Field",
                               "Cumulatively Induced Geoelectric Field",
                               "factor_e_field",
                               e_field_labels, e_field_levels, sizing_vals)

print(e_field_plot)

# Plot 3: Line Voltages
sizing_vals <- setNames(1*(1:9), voltage_labels)
voltage_plot <- create_sf_plot(tl_geojson,
                               subset_data,
                               pjm_geojson,
                               "Voltages (V)",
                               "(C) Interpolated Voltage Induced",
                               "Line Voltages Induced During Peak Halloween Storm",
                               "factor_voltage",
                               voltage_labels, voltage_levels, sizing_vals)

print(voltage_plot)

# Plot 4: Line Current Sources
sizing_vals <- setNames(1*(1:9), i_labels)
i_plot <- create_sf_plot(tl_geojson,
                         subset_data,
                         pjm_geojson,
                         "Current (A)",
                         "(D) Currents Due to Line Voltages and Wire Resistances",
                         "Line Current Sources using ACSR Ohmic Resistance of 0.05 Ohms/km",
                         "factor_i",
                         i_labels, i_levels, sizing_vals)

print(i_plot)

# Combine the 4 Plots
combined_plot_2 <- v_class_plot/ e_field_plot /voltage_plot / i_plot
# Adjust the layout
combined_plot_2 <- combined_plot_2 + plot_layout(ncol = 2, nrow = 2) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
print(combined_plot_2)
# File path
file_path_2 = file.path(folder,"figures", "geophysical_2.png")
# Save the stacked plots
ggsave(file_path_2, combined_plot_2, dpi = 300, width=8, height=5)

dev.off()








