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

# US State shapefiles
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

### Merge the NERC JSON with business data from ZCTA regions ###

merged_nerc_bg <- nerc_geojson %>%
  inner_join(df_rto_stats, by = c("REGION_ID", "REGIONS"))

# groupby the regions of NERC RTOs, and sum  the population, number of business, etc

summarised_zcta_df <- df_zcta_stats %>%
  group_by(REGIONS, REGION_ID) %>%
  summarise(
    Total_EMP = round(sum(EMP, na.rm = TRUE) / 1e6, digits = 4),
    Total_AP = round(sum(AP, na.rm = TRUE) / 1e6, digits = 4),
    Total_EST = round(sum(EST, na.rm = TRUE) / 1e4, digits = 4),
    Total_POP20 = round(sum(POP20, na.rm = TRUE) / 1e6, digits = 4),
    Total_ZCTAGDP = round(sum(ZCTAGDP, na.rm = TRUE) / 1e3, digits = 4)
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

#### Visualizations ####
#### 1. Analysis of business data at zcta resolution ####

# Regional Transmission Operators
# color palette
colors <-
  colorRampPalette(brewer.pal(9, "Set1"))(length(unique(state_shp_filtered$STABBR)))


plot1 <- ggplot(data = summarised_rto_df) +
  geom_sf(data = state_shp_filtered, fill = NA, color = "black", size = 0.25, linewidth=0.15) +
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
  coord_sf() +
  theme(text = element_text(family = "Times New Roman"))

print(plot1)

# Continous variables plots
# color ramp
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(state_shp_filtered$STABBR))

create_sf_plot <-
  function(data,
           data_2,
           fill_variable,
           legend_title,
           plot_title) {
    
    # Get unique values
    unique_values <- unique(data[[fill_variable]])
    
    # Create a Brewer color palette
    num_colors <- length(unique_values) - 1
    colors <- brewer_color_ramp(num_colors)
    
    # Create a color gradient, including grey for zero
    gradient_colors <- c("grey", colors)
    gradient_breaks <- c(0, sort(unique_values[unique_values != 0]))
    
    plot <- ggplot(data) +
      geom_sf(data = data_2, fill = NA, color = "dodgerblue", linewidth=0.1, alpha=1) +
      geom_sf(aes(fill = .data[[fill_variable]]),
              linewidth = 0.34,
              alpha=0.8,
              color = "white") +
      theme_void() +
      scale_fill_gradientn(colors = gradient_colors, 
                           values = scales::rescale(gradient_breaks),
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
      coord_sf() +
      theme(text = element_text(family = "Times New Roman"))
    
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
  state_shp_filtered,
  fill_variable = "Total_AP",
  legend_title = "Payroll ($B)",
  plot_title = "(B) Annual Employee Payroll"
)

print(plot3)

# Plot 3: Business Establishments in the Region
plot4 <- create_sf_plot(
  data = summarised_rto_df,
  state_shp_filtered,
  fill_variable = "Total_EST",
  legend_title = "Establishment (10000)",
  plot_title = "(C) Total Number of Business Establishments"
)

print(plot4)

# Plot 4: Population in the Regions
plot5 <- create_sf_plot(
  data = summarised_rto_df,
  state_shp_filtered,
  fill_variable = "Total_POP20",
  legend_title = "Consumers (Millions)",
  plot_title = "(D) Total Number of Consumers in the Regions"
)

print(plot5)

# Plot 4: Regional Aggregation of the State GDP Apportionment by Population
plot6 <- create_sf_plot(
  data = summarised_rto_df,
  state_shp_filtered,
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

grouped_df <- df_rto_stats %>%
  distinct(STABBR, FIRM, ESTB, EMPL, PAYR, X2022REALGDP, STPOP, .keep_all = TRUE)


grouped_df <- df_rto_stats %>%
  distinct(STABBR, FIRM, ESTB, EMPL, PAYR, X2022REALGDP, STPOP, .keep_all = TRUE) %>%
  mutate(
    PAYR = PAYR / 1e6,
    X2022REALGDP = X2022REALGDP / 1e3,
    EMPL = EMPL / 1e6,
    STPOP = STPOP / 1e6
    
  )

merged_df <- merge(state_shp_filtered, grouped_df, by="STABBR")

# Plot 6 State Employees
plot6 <- create_sf_plot(
  data = merged_df,
  nerc_geojson,
  fill_variable = "EMPL",
  legend_title = "Employment (Millions)",
  plot_title = "(A) State Level Employment"
)

plot(plot6)

# Plot 7 State Businesses
plot7 <- create_sf_plot(
  data = merged_df,
  nerc_geojson,
  fill_variable = "ESTB",
  legend_title = "Establishments",
  plot_title = "(B) The Number of Establishments in the US States"
)

plot(plot7)

# Plot 8 Real GDP 2022
plot8 <- create_sf_plot(
  data = merged_df,
  nerc_geojson,
  fill_variable = "X2022REALGDP",
  legend_title = "GDP ($B)",
  plot_title = "(C) 2022 Real GDP Per State"
)

plot(plot8)

# Plot 9 Annual Payroll
plot9 <- create_sf_plot(
  data = merged_df,
  nerc_geojson,
  fill_variable = "PAYR",
  legend_title = "Payroll ($B)",
  plot_title = "(D) Annual Employee Compensation Per State"
)

plot(plot9)

# save the plots
combined_plot <- plot6 + plot7 + plot8 + plot9
# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 2, nrow = 2) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(combined_plot)
path_out = file.path(folder, 'figures', 'state_emp_bus_count.png')
ggsave(path_out,
       combined_plot,
       width = 10.5,
       height = 7.5)

#### Visualize US Transmission Lines and Magnetotelluric E fields at MT Stations ####

# Greg's MT
mt_data <- st_read("mt_gdf.geojson")

# Transmission lines
tl_data <- st_read("tl_cont.geojson")

# remove not available voltage class
tl_data_clean <- tl_data %>%
  filter(VOLT_CLASS != "NOT AVAILABLE") %>%
  filter(!is.na(VOLT_CLASS))

## US Electricity Lines
# color ramp
colors <-
  colorRampPalette(brewer.pal(9, "Set1"))(length(unique(tl_data_clean$VOLT_CLASS)))

plot10 <- ggplot(tl_data_clean) +
  geom_sf(data = state_shp_filtered, fill = NA, color = "black", linewidth=0.1) +
  geom_sf(aes(color = VOLT_CLASS),
          linewidth = 0.5,
          alpha=1) +
  theme_void() +
  scale_color_manual(
    values = colors,
    name = "Line Voltage (kV)",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0,
      label.hjust = .0,
      keywidth = 1.2,
      keyheight = 0.7,
      direction = "vertical"
    )
  ) +
  labs(title = "(A) US Mainland Electricity Transmission Lines") +
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
    legend.position = c(1,-0.1),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(3, "cm"),
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
  coord_sf() +
  theme(text = element_text(family = "Times New Roman"))

print(plot10)

## MT DATA Viz ##

# color ramp
brewer_pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))

plot11 <- ggplot(mt_data) +
  geom_sf(data = state_shp_filtered, fill = NA, color = "black", linewidth=0.1) +
  geom_sf(aes(color = E.100.year),
          linewidth = 0.5,
          size=1.5) +
  theme_void() +
  scale_color_gradientn(
    colours = brewer_pal(100),  # Use 100 to get a smooth gradient
    name = "E (100-year)",
    trans = "sqrt") +
  labs(title = "(B) Ground Induced Currents Estimates During a 100-Year Event") +
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
    legend.key.width = unit(0.04, "npc"),
    legend.title = element_text(size = 8)  # Adjusting legend title size
  ) +
  guides(color = guide_colourbar(title.position = 'top', direction = "horizontal")) +
  coord_sf() +
  theme(text = element_text(family = "Times New Roman"))

print(plot11)

# save the plots
combined_plot <- plot10 + plot11
# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 2, nrow = 1) &
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(combined_plot)
path_out = file.path(folder, 'figures', 'MT_TL.png')
ggsave(path_out,
       combined_plot,
       width = 10.5,
       height = 7.5)


## Plot direct impact, business establishments, and population affected ##

### 1) SUPPLY SIDE SCENARIOS ###

# Load the number of establishments affected in each scenario
df_est <- read.csv("est_regions.csv")

# Merge with the grid regions df
merged_nerc_est <- nerc_geojson %>%
  inner_join(df_est, by = c("REGIONS"))

# Load the population size affected in each scenario
df_pop <-
  read.csv("regions_pop_est_loss_scenarios.csv")

# Merge with the grid regions df
merged_nerc_pop <- nerc_geojson %>%
  inner_join(df_pop, by = c("REGIONS"))

# Load the GDP shock
df_gdp_shock <- read.csv("regions_gdp_loss_est_scenarios.csv")

# Merge with the grid regions df
merged_nerc_gdp <- nerc_geojson %>%
  inner_join(df_gdp_shock, by = c("REGIONS"))

create_plot <-
  function(data,
           data_2,
           fill_variable,
           legend_title,
           plot_title) {
    
    # Get unique values
    unique_values <- unique(data[[fill_variable]])
    
    # Create a Brewer color palette
    num_colors <- length(unique_values) - 1
    colors <- brewer_color_ramp(num_colors)
    
    # Create a color gradient, including grey for zero
    gradient_colors <- c("grey", colors)
    gradient_breaks <- c(0, sort(unique_values[unique_values != 0]))
    
    plot <- ggplot(data) +
      geom_sf(data = data_2, fill = NA, color = "dodgerblue", linewidth=0.1, alpha=1) +
      geom_sf(aes(fill = .data[[fill_variable]]),
              linewidth = 0.34,
              alpha=0.8,
              color = "white") +
      theme_void() +
      scale_fill_gradientn(colors = gradient_colors, 
                           values = scales::rescale(gradient_breaks),
                           name = legend_title) +
      labs(title = plot_title) +
      theme(
        text = element_text(color = "#22211d"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(
          size = 9,
          hjust = 0.03,
          margin = margin(
            b = -0.1,
            t = 0.4,
            l = 2,
            unit = "cm"
          )
        ),
        legend.position = c(0.3, 0),
        legend.justification = c(0.5, 0.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(0.03, "npc"),
        legend.title = element_text(size = 8)  # Adjusting legend title size
      ) +
      guides(fill = guide_colourbar(title.position = 'top', direction = "horizontal")) +
      coord_sf() +
      theme(text = element_text(family = "Times New Roman"))
    
    
    return(plot)
  }

# Function to create a plot for a given percentage column
create_perc_plot <- function(data, shape, perc_col, legend_title_base, plot_title_base, include_percentage = TRUE) {
  percentage <- gsub("perc", "", perc_col)  # Extract the percentage value
  legend_title <- sprintf("%s", legend_title_base)
  
  if (include_percentage) {
    plot_title <- sprintf("%s (%s%%)", plot_title_base, percentage)
  } else {
    plot_title <- plot_title_base
  }
  
  plot <- create_plot(
    data = data,
    shape,
    fill_variable = perc_col,
    legend_title = legend_title,
    plot_title = plot_title
  )
  
  return(plot)
}

# List of all columns starting with 'perc'
perc_columns <- grep("^perc[1-9]", names(merged_nerc_pop), value = TRUE)

plot12 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[1], "Establishments (Millions)", "(A) Establishments without Power")
plot13 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[2], "Establishments (Millions)", "(D) Establishments without Power")
plot14 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[3], "Establishments (Millions)", "(G) Establishments without Power")
plot15 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[4], "Establishments (Millions)", "(J) Establishments without Power")
plot16 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[1], "Population (Millions)", "(B) Population without Power", FALSE)
plot17 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[2], "Population (Millions)", "(E) Population without Power", FALSE)
plot18 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[3], "Population (Millions)", "(H) Population without Power", FALSE)
plot19 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[4], "Population (Millions)", "(K) Population without Power", FALSE)
plot20 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[1], "GDP Shock ($Bn)", "(C) Daily GDP Loss", FALSE)
plot21 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[2], "GDP Shock ($Bn)", "(F) Daily GDP Loss", FALSE)
plot22 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[3], "GDP Shock ($Bn)", "(I) Daily GDP Loss", FALSE)
plot23 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[4], "GDP Shock ($Bn)", "(L) Daily GDP Loss", FALSE)

combined_plot <- plot12 + plot16 + plot20 + plot13 + plot17 + plot21 + plot14 + plot18 + plot22 + plot15 + plot19 + plot23

# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 3, nrow = 4) &
  theme(plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"))

# Print the combined plot
print(combined_plot)

path_out <- file.path(folder, 'figures', 'combined_plot_est.png')
ggsave(path_out, combined_plot, width = 8, height = 12)

### 1) DEMAND SIDE SCENARIOS ###

# Load the population size affected in each scenario
df_pop <-
  read.csv("pop_regions.csv")

# Merge with the grid regions df
merged_nerc_pop <- nerc_geojson %>%
  inner_join(df_pop, by = c("REGIONS"))

# Load the number of establishments affected in each scenario
df_est <- read.csv("regions_est_pop_scenarios.csv")

# Merge with the grid regions df
merged_nerc_est <- nerc_geojson %>%
  inner_join(df_est, by = c("REGIONS"))

# Load the GDP shock
df_gdp_shock <- read.csv("regions_gdp_loss_pop_scenarios.csv")

# Merge with the grid regions df
merged_nerc_gdp <- nerc_geojson %>%
  inner_join(df_gdp_shock, by = c("REGIONS"))

# Make the plots
plot24 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[1], "Establishments (Millions)", "(B) Establishments without Power", FALSE)
plot25 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[2], "Establishments (Millions)", "(E) Establishments without Power", FALSE)
plot26 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[3], "Establishments (Millions)", "(H) Establishments without Power", FALSE)
plot27 <- create_perc_plot(merged_nerc_est, state_shp_filtered, perc_columns[4], "Establishments (Millions)", "(K) Establishments without Power", FALSE)
plot28 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[1], "Population (Millions)", "(A) Population without Power", TRUE)
plot29 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[2], "Population (Millions)", "(D) Population without Power", TRUE)
plot30 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[3], "Population (Millions)", "(G) Population without Power", TRUE)
plot31 <- create_perc_plot(merged_nerc_pop, state_shp_filtered, perc_columns[4], "Population (Millions)", "(J) Population without Power", TRUE)
plot32 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[1], "GDP Shock ($Bn)", "(C) Daily GDP Loss", FALSE)
plot33 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[2], "GDP Shock ($Bn)", "(F) Daily GDP Loss", FALSE)
plot34 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[3], "GDP Shock ($Bn)", "(I) Daily GDP Loss", FALSE)
plot35 <- create_perc_plot(merged_nerc_gdp, state_shp_filtered, perc_columns[4], "GDP Shock ($Bn)", "(L) Daily GDP Loss", FALSE)

combined_plot <- plot28 + plot24 + plot32 + plot29 + plot25 + plot33 + plot30 + plot26 + plot34 + plot31 + plot27 + plot35
# Adjust the layout
combined_plot <- combined_plot + plot_layout(ncol = 3, nrow = 4) &
  theme(plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"))

# Print the combined plot
print(combined_plot)

path_out <- file.path(folder, 'figures', 'combined_plot_pop.png')
ggsave(path_out, combined_plot, width = 8, height = 12)

dev.off()
