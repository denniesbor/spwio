library(gdata)
library(tidyverse)
library(ggpubr)
library(jsonlite)
library(rlist)
library(cowplot)
library(gridExtra)
library(dplyr)
library(tidyr)
library(flextable)

folder = dirname(rstudioapi::getSourceEditorContext()$path)
data_directory = file.path(folder, '..', 'data', 'results', "supply")
setwd(data_directory)

# set the working directory
# input - output modeling
TechnicalCoefficients <- read.csv("IndByComs.csv")
grossOutput2022 <- read.csv("output_2022.csv")[, 2]
grossOutput2022 <- grossOutput2022 / 1000
consumption <- read.csv("consumption.csv")[, 3] / (365 * 1.2) # Get daily consumption in 2017 dollars

# grossOutput <- read.csv("Gross.csv")[, 10]
Industries <- read.csv("Gross.csv")[, 1]
Industries[69] = "State Government Enterprises"

L <-
  data.matrix(TechnicalCoefficients) #We create our Leontief matrix
Identity <- diag(1, 69) #We create an Identity Matrix
A <- Identity - solve(L)
GrossDiag <-
  diag(grossOutput2022) #We diagonalize a matrix of the Industry Gross Outputs.

GrossDiagInv <-
  solve(GrossDiag) #We find the Inverse of that matrix
BMatrix <-
  GrossDiagInv %*% (A) %*% GrossDiag #We find the matrix B in accordance with the method described in Section 3.2

Ghosh = solve(Identity - BMatrix)# G = (I-B)^-1, as described in Section 3.2

GDPShocks <- read.csv("GDP_shocks_est.csv")

# population data
pop_scenario <- read.csv("pop_est.csv")

# Read the entire grid data -  calculate total population and daily gross output
grid_data <- read.csv("grid_data.csv")
total_pop <- sum(grid_data$POP20) / 1e6 # total population

daily_gdp <- sum(grossOutput2022) / 365

# NAICs Industries
NAICSIndustries <- c(
  "Agriculture, Forestry, Fishing and Hunting",
  "Mining, Quarrying, and Oil and Gas Extraction",
  "Utilities",
  "Construction",
  "Manufacturing",
  "Wholesale Trade",
  "Retail Trade",
  "Transportation and Warehousing",
  "Information",
  "Finance and Insurance",
  "Real Estate and Rental and Leasing",
  "Professional, Scientific, and Technical Services",
  "Management of Companies and Enterprises",
  "Administrative and Support and Waste ...",
  "Educational Services",
  "Health Care and Social Assistance",
  "Arts, Entertainment, and Recreation",
  "Accommodation and Food Services",
  "Other Services (except Public Administration)",
  "Unclassified"
)

GDPShocks$NAICSIndustries <- NAICSIndustries

NAICsCode = c(
  "11",
  "21",
  "22",
  "23",
  "31-33",
  "42",
  "44-45",
  "48-49",
  "51",
  "52",
  "53",
  "54",
  "55",
  "56",
  "61",
  "62",
  "71",
  "72",
  "81",
  "99"
)

legend_data <- data.frame(
  NAICSCode = NAICsCode,
  Description = NAICSIndustries
)

GraphList <- list()
LeonList <- list()
CombinedList <- list()

aggregated_frame <- data.frame()
table_frame <- data.frame()

# Condensed agg fram
condensed_aggregated_frame <- data.frame()

percentile <- 15

for (i in seq(from = 1, to = 4, by = 1)) {
  # Estimate change in personal consumption due to population disruption
  perc_column <- paste0("perc", i * percentile) # Column name
  scenario_pop <- sum(pop_scenario[perc_column])
  pop_percentage <- scenario_pop / total_pop
  consumption_loss <- (consumption * pop_percentage) / 1000 # In billion dollars
  
  # Estimate direct supply impacts
  VA <- vector(mode = 'numeric', length = 69)

  #NAICS 11 - Farms are 88.7% of Sector Mark
  VA[1] = GDPShocks[, i + 2][1] * .887
  VA[2] = GDPShocks[, i + 2][1] * .113
  
  #NAICS 21 -> "Oil and Gas are 72.5%
  VA[3] = GDPShocks[, i + 2][2] * .725
  VA[4] = GDPShocks[, i + 2][2] * .173
  VA[5] = GDPShocks[, i + 2][2] * .102
  
  #NAICS 22
  VA[6] = GDPShocks[, i + 2][3]
  
  #NAICS 23
  VA[7] = GDPShocks[, i + 2][4]
  
  #NAICS 31
  VA[8] = GDPShocks[, i + 2][5] * .0238
  VA[9] = GDPShocks[, i + 2][5] * .0232
  VA[10] = GDPShocks[, i + 2][5] * .0415
  VA[11] = GDPShocks[, i + 2][5] * .0623
  VA[12] = GDPShocks[, i + 2][5] * .0656
  VA[13] = GDPShocks[, i + 2][5] * .0637
  VA[14] = GDPShocks[, i + 2][5] * .0228
  VA[15] = GDPShocks[, i + 2][5] * .1160
  VA[16] = GDPShocks[, i + 2][5] * .0487
  VA[17] = GDPShocks[, i + 2][5] * .0126
  VA[18] = GDPShocks[, i + 2][5] * .0289
  VA[19] = GDPShocks[, i + 2][5] * .1667
  VA[20] = GDPShocks[, i + 2][5] * .0083
  VA[21] = GDPShocks[, i + 2][5] * .0036
  VA[22] = GDPShocks[, i + 2][5] * .0304
  VA[23] = GDPShocks[, i + 2][5] * .0125
  VA[24] = GDPShocks[, i + 2][5] * .0962
  VA[25] = GDPShocks[, i + 2][5] * .1341
  VA[26] = GDPShocks[, i + 2][5] * .0391
  
  #NAICS 42
  VA[27] = GDPShocks[, i + 2][6]
  
  #NAICS 44
  VA[28] = GDPShocks[, i + 2][7] * .1570
  VA[29] = GDPShocks[, i + 2][7] * .1270
  VA[30] = GDPShocks[, i + 2][7] * .1205
  VA[31] = GDPShocks[, i + 2][7] * .5956
  
  #NAICS 48
  VA[32] = GDPShocks[, i + 2][8] * .1488
  VA[33] = GDPShocks[, i + 2][8] * .0549
  VA[34] = GDPShocks[, i + 2][8] * .0350
  VA[35] = GDPShocks[, i + 2][8] * .3224
  VA[36] = GDPShocks[, i + 2][8] * .0484
  VA[37] = GDPShocks[, i + 2][8] * .0403
  VA[38] = GDPShocks[, i + 2][8] * .2289
  VA[39] = GDPShocks[, i + 2][8] * .1213
  
  #NAICS 51
  VA[40] = GDPShocks[, i + 2][9] * .2211
  VA[41] = GDPShocks[, i + 2][9] * .0757
  VA[42] = GDPShocks[, i + 2][9] * .4139
  VA[43] = GDPShocks[, i + 2][9] * .2904
  
  #NAICS 52
  VA[44] = GDPShocks[, i + 2][10] * .3105
  VA[45] = GDPShocks[, i + 2][10] * .2282
  VA[46] = GDPShocks[, i + 2][10] * .4056
  VA[47] = GDPShocks[, i + 2][10] * .0557
  
  #NAICS 53
  VA[48] = GDPShocks[, i + 2][11] * .7963
  VA[49] = GDPShocks[, i + 2][11] * .2037
  
  #NAICS 54
  VA[50] = GDPShocks[, i + 2][12] * .1480
  VA[51] = GDPShocks[, i + 2][12] * .1936
  VA[52] = GDPShocks[, i + 2][12] * .6584
  
  #NAICS 55
  VA[53] = GDPShocks[, i + 2][13]
  
  #NAICS 56
  VA[54] = GDPShocks[, i + 2][14] * .9073
  VA[55] = GDPShocks[, i + 2][14] * .0927
  
  #NAICS 61
  VA[56] = GDPShocks[, i + 2][15]
  
  #NAICS 62
  VA[57] = GDPShocks[, i + 2][16] * .4396
  VA[58] = GDPShocks[, i + 2][16] * .3722
  VA[59] = GDPShocks[, i + 2][16] * .0949
  VA[60] = GDPShocks[, i + 2][16] * .0934
  
  #NAICS 71
  VA[61] = GDPShocks[, i + 2][17] * .5709
  VA[62] = GDPShocks[, i + 2][17] * .4291
  
  #NAICS 72
  VA[63] = GDPShocks[, i + 2][18] * .2011
  VA[64] = GDPShocks[, i + 2][18] * .7989
  
  #NAICS 81
  VA[65] = GDPShocks[, i + 2][19] * .2011
  
  #NAICS UNCLFD
  VA[66] = GDPShocks[, i + 2][20] * .1537
  VA[67] = GDPShocks[, i + 2][20] * .0248
  VA[68] = GDPShocks[, i + 2][20] * .7197
  VA[69] = GDPShocks[, i + 2][20] * .1017
  
  VAm <- VA - consumption_loss # Subtract personal consumption from VA
  
  VAm <- matrix(VAm, nrow = 1)
  Lem <- matrix(consumption_loss, ncol = 1)
  Output <- (VAm %*% Ghosh) - as.vector(VAm)
  LeonOutput <- (L %*% Lem) - as.vector(consumption_loss)
  
  # Create new vectors for condensed output
  CondensedOutput <- numeric(length = 20)
  CondensedLeonOutput <- numeric(length = 20)
  
  # Aggregate by NAICS codes
  # NAICS 11 - Farms
  CondensedOutput[1] <- sum(Output[c(1, 2)])
  CondensedLeonOutput[1] <- sum(LeonOutput[c(1, 2)])
  
  # NAICS 21 - Mining, Quarrying, and Oil and Gas Extraction
  CondensedOutput[2] <- sum(Output[c(3, 4, 5)])
  CondensedLeonOutput[2] <- sum(LeonOutput[c(3, 4, 5)])
  
  # NAICS 22 - Utilities
  CondensedOutput[3] <- sum(Output[6])
  CondensedLeonOutput[3] <- sum(LeonOutput[6])
  
  # NAICS 23 - Construction
  CondensedOutput[4] <- sum(Output[7])
  CondensedLeonOutput[4] <- sum(LeonOutput[7])
  
  # NAICS 31-33 - Manufacturing
  CondensedOutput[5] <- sum(Output[8:26])
  CondensedLeonOutput[5] <- sum(LeonOutput[8:26])
  
  # NAICS 42 - Wholesale Trade
  CondensedOutput[6] <- sum(Output[27])
  CondensedLeonOutput[6] <- sum(LeonOutput[27])
  
  # NAICS 44-45 - Retail Trade
  CondensedOutput[7] <- sum(Output[28:31])
  CondensedLeonOutput[7] <- sum(LeonOutput[28:31])
  
  # NAICS 48-49 - Transportation and Warehousing
  CondensedOutput[8] <-
    sum(Output[32:39])
  CondensedLeonOutput[8] <-
    sum(LeonOutput[32:39])
  
  # NAICS 51 - Information
  CondensedOutput[9] <- sum(Output[40:43])
  CondensedLeonOutput[9] <- sum(LeonOutput[40:43])
  
  # NAICS 52 - Finance and Insurance
  CondensedOutput[10] <- sum(Output[44:47])
  CondensedLeonOutput[10] <- sum(LeonOutput[44:47])
  
  # NAICS 53 - Real Estate and Rental and Leasing
  CondensedOutput[11] <- sum(Output[c(48, 49)])
  CondensedLeonOutput[11] <- sum(LeonOutput[c(48, 49)])
  
  # NAICS 54 - Professional, Scientific, and Technical Services
  CondensedOutput[12] <- sum(Output[50:52])
  CondensedLeonOutput[12] <- sum(LeonOutput[50:52])
  
  # NAICS 55 - Management of Companies and Enterprises
  CondensedOutput[13] <- Output[53]
  CondensedLeonOutput[13] <- LeonOutput[53]
  
  # NAICS 56 - Administrative and Support and Waste Management and Remediation Services
  CondensedOutput[14] <- sum(Output[c(54, 55)])
  CondensedLeonOutput[14] <- sum(LeonOutput[c(54, 55)])
  
  # NAICS 61 - Educational Services
  CondensedOutput[15] <- Output[56]
  CondensedLeonOutput[15] <- LeonOutput[56]
  
  # NAICS 62 - Health Care and Social Assistance
  CondensedOutput[16] <- sum(Output[57:60])
  CondensedLeonOutput[16] <- sum(LeonOutput[57:60])
  
  # NAICS 71 - Arts, Entertainment, and Recreation
  CondensedOutput[17] <- sum(Output[c(61, 62)])
  CondensedLeonOutput[17] <- sum(LeonOutput[c(61, 62)])
  
  # NAICS 72 - Accommodation and Food Services
  CondensedOutput[18] <- sum(Output[c(63, 64)])
  CondensedLeonOutput[18] <- sum(LeonOutput[c(63, 64)])
  
  # NAICS 81 - Other Services (except Public Administration)
  CondensedOutput[19] <- Output[65]
  CondensedLeonOutput[19] <- LeonOutput[65]
  
  # NAICS UNCLFD - Unclassified
  CondensedOutput[20] <- sum(Output[66:69])
  CondensedLeonOutput[20] <- sum(LeonOutput[66:69])
  
  # Create Frame and LeonFrame for the current iteration
  Frame <-
    data.frame(
      "Scenario" = paste0("s",i),
      "Type" = "GDP",
      "Industries" = Industries,
      "Output" = as.vector(Output)
    )
  LeonFrame <-
    data.frame(
      "Scenario" = paste0("s",i),
      "Type" = "Leon",
      "Industries" = Industries,
      "Output" = as.vector(LeonOutput)
    )
  
  ## Mutate gdp shocks to conform with upstream and downstream dataframes ##
  
  direct_impact_df <- GDPShocks %>%
    mutate(
      Output = .[[perc_column]],
      Type = "Direct",        
      Scenario = paste0("s",i)          
    ) %>%
    select(Scenario, Type, NAICSIndustries, Output)
  
  # create condensed frame and LeonFrame
  CondensedFrame <-
    data.frame(
      "Scenario" = paste0("s",i),
      "Type" = "GDP",
      "NAICSIndustries" = NAICSIndustries,
      "Output" = as.vector(CondensedOutput)
    )
  
  CondensedLeonFrame <-
    data.frame(
      "Scenario" = paste0("s",i),
      "Type" = "Leon",
      "NAICSIndustries" = NAICSIndustries,
      "Output" = as.vector(CondensedLeonOutput)
    )
  
  # Combine Frame and LeonFrame for the current iteration
  combined_frame <- rbind(Frame, LeonFrame)
  
  # Append to the aggregated data frame
  aggregated_frame <- rbind(aggregated_frame, combined_frame)
  
  # Conmbine condensed frames
  condensed_combined_frame <-
    rbind(CondensedFrame, CondensedLeonFrame)

  
  # Combine the data
  bound_data <- rbind(CondensedFrame, CondensedLeonFrame, direct_impact_df)
  
  # Append to the condensed aggregated data frame
  condensed_aggregated_frame <- rbind(condensed_aggregated_frame, bound_data)
  
  ## Sum the direct and indirect impacts and append to a table ##

  sum_scenario_data <- bound_data %>%
    group_by(Type) %>%
    summarise(TotalOutput = sum(Output, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Type, values_from = TotalOutput) %>%
    rename(
      "Direct Impact" = Direct,
      "Downstream effects" = Leon,
      "Upstream effects" = GDP
    ) %>%
    mutate(TotalImpact = `Direct Impact` + `Downstream effects` + `Upstream effects`) %>%
    mutate(Scenario = paste0("S", i)) %>%
    mutate(Impact = paste0(i * percentile, "%")) %>%
    mutate(Population = scenario_pop) %>%
    select(Scenario, Impact, Population, everything()) %>%
    mutate(across(c("Direct Impact", "Downstream effects", "Upstream effects", "TotalImpact"), round, digits = 2))
  
  sum_scenario_data <- sum_scenario_data %>%
    mutate(
      `Population` = paste(sprintf("%.2f", `Population`), "(", sprintf("%.2f%%", (`Population` / total_pop) * 100), ")", sep = ""),
      `Direct Impact` = paste(sprintf("%.2f", `Direct Impact`), "(", sprintf("%.2f%%", (`Direct Impact` / daily_gdp) * 100), ")", sep = ""),
      `Downstream effects` = paste(sprintf("%.2f", `Downstream effects`), "(", sprintf("%.2f%%", (`Downstream effects` / daily_gdp) * 100), ")", sep = ""),
      `Upstream effects` = paste(sprintf("%.2f", `Upstream effects`), "(", sprintf("%.2f%%", (`Upstream effects` / daily_gdp) * 100), ")", sep = ""),
      `TotalImpact` = paste(sprintf("%.2f", `TotalImpact`), "(", sprintf("%.2f%%", (`TotalImpact` / daily_gdp) * 100), ")", sep = "")
    ) %>%
    rename(
      'Business Impact' = Impact,
      `Population (M)` = Population,
      `Direct impacts ($Bn)` = `Direct Impact`,
      `Demand-side impacts ($Bn)` = `Downstream effects`,
      `Supply-side impacts ($Bn)` = `Upstream effects`,
      `Total Impact ($Bn)` = `TotalImpact`
    )
  
  
  # Append pivoted and concatenated impacts to a table for export
  table_frame <- rbind(table_frame, sum_scenario_data)
  
  # Create the stack plots of all the impacts
  stacked_plot <- ggplot(data = bound_data, aes(x = Output, y = reorder(NAICSIndustries, Output), fill = Type)) +
    geom_bar(stat = "identity", width = 0.9, position = "stack") +
    scale_fill_manual(values = c("Direct" = "#E41A1C", "Leon" = "#4DAF4A", "GDP" = "#2D72B4"),
                      labels = c("Direct" = "Direct GDP Shock", "Leon" = " Indirect Demand Side Impact", "GDP" = "Indirect Supply Side Impact")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 12)) +
    ylab("") +
    xlab("Daily Losses (In Billions, US $)") +
    labs(
      fill = "Macroeconomic Impact",
      title = paste(
        "(", intToUtf8(64 + i), ") Economic Impact from Space-Weather Induced",
        i * percentile,
        "% Power Failure"
      ),
      subtitle = "Reported by NAICS"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      text = element_text(family = "Times")
    )
  
  CombinedList[[i]] <- stacked_plot
  
}

combined_stacked_plot <- ggarrange(plotlist = CombinedList,
                                   ncol = 1,
                                   common.legend = TRUE, legend = "bottom")

print(combined_stacked_plot)
# Save the graphics

path_out <- file.path(folder, 'figures', 'stacked_gva_losses.png')
ggsave(path_out, combined_stacked_plot, width = 8, height = 12)


### Tabulate the direct, and indirect impacts ###

ft <- flextable(table_frame) %>% 
  align(align = "center", part = "all") %>% 
  line_spacing(space = 2, part = "all") %>% 
  padding(padding = 6, part = "header")

set_table_properties(ft, width = 1, layout = "autofit")

ft <- set_caption(ft, caption = "Direct and Indirect Losses by Scenarios")

print(ft)

# Save the table as a word document
table_impacts = file.path(folder, 'figures', 'impacts.docx')
save_as_docx(ft, path = table_impacts)

# Save the aggregated Impacts by NAICs
condensed_aggregated_frame$OutputPerc = (condensed_aggregated_frame$Output / daily_gdp) * 100
write.csv(condensed_aggregated_frame, "condensed_aggregated_frame.csv", row.names = FALSE)

# Summarize data by Type and Scenario
summarized_data <- aggregated_frame %>%
  group_by(Type, Scenario) %>%
  summarize(
    TotalOutput = sum(Output, na.rm = TRUE),
    MeanOutput = mean(Output, na.rm = TRUE),
    SD = sd(Output, na.rm = TRUE)
  ) %>%
  mutate(CI_lower = MeanOutput - 1.96 * SD / sqrt(n()),
         # 95% CI assuming normal distribution
         CI_upper = MeanOutput + 1.96 * SD / sqrt(n()))

# Separate the data by type
leon_data <- filter(summarized_data, Type == "Leon")
gdp_data <- filter(summarized_data, Type == "GDP")

# Dot plot for Leon data
leon_dot_plot <-
  ggplot(leon_data, aes(x = as.factor(Scenario * 10), y = MeanOutput)) +
  geom_point(color = "#2D72B4",
             position = position_dodge(0.2),
             size = 5) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    color = "#2D72B4",
    linetype = "dashed",
    position = position_dodge(0.2)
  ) +
  labs(x = "Percentage of Businesses without Power",
       y = "Mean Output",
       title = "Sectorial Aggregates of GDP Effects due to Space Weather Induced Power Failure") +
  theme(
    axis.title.x = element_text(margin = margin(t = 9)),
    axis.text.y = element_text(
      hjust = 1,
      vjust = 0.25,
      size = 10
    ),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 15, family = "Times"),
    legend.text = element_text(size = 15),
    text = element_text(family = "Times")
  ) +
  theme_bw()

# Dot plot for Ghosh
gdp_dot_plot <-
  ggplot(gdp_data, aes(x = as.factor(Scenario * 100), y = MeanOutput)) +
  geom_point(color = "#2D72B4",
             position = position_dodge(0.2),
             size = 5) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    color = "#2D72B4",
    linetype = "dashed",
    position = position_dodge(0.2)
  ) +
  labs(x = "Scenario",
       y = "Mean Output",
       title = "Su") +
  theme(
    axis.title.x = element_text(margin = margin(t = 9)),
    axis.text.y = element_text(
      hjust = 1,
      vjust = 0.25,
      size = 10
    ),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 15, family = "Times"),
    legend.text = element_text(size = 15),
    text = element_text(family = "Times")
  ) +
  theme_bw()

# Bar chart for GDP data
gdp_bar_chart <-
  ggplot(gdp_data, aes(
    x = as.factor(Scenario * 10),
    y = TotalOutput,
    fill = Type
  )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Percentage of Businesses without Power", y = "Total Ouput ($)", title = "GDP Data Across Scenarios") +
  theme_minimal()

# Display the plots
print(leon_dot_plot)
print(gdp_bar_chart)


dev.off()

