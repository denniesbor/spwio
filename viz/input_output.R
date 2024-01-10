library(gdata)
library(tidyverse)
library(ggpubr)
library(rlist)
library(cowplot)
library(gridExtra)
library(dplyr)
library(tidyr)
library(flextable)

folder = dirname(rstudioapi::getSourceEditorContext()$path)
data_directory = file.path(folder, '..', 'data', 'input_output')
setwd(data_directory)


# set the working directory
# input - output modeling
TechnicalCoefficients <- read.csv("IndByComs.csv")
grossOutput <- read.csv("Gross.csv")[, 10]
Industries <- read.csv("Gross.csv")[, 1]
Industries[69] = "State Government Enterprises"

Leontief <-
  data.matrix(TechnicalCoefficients) #We create our Leontief matrix
Identity <- diag(1, 69) #We create an Identity Matrix
A = Identity - solve(Leontief) #We use the formula (A-I)^-1 = L to find the Technical Coefficient Matrix.
GrossDiag <-
  diag(grossOutput) #We diagonalize a matrix of the Industry Gross Outputs.
GrossDiagInv <-
  solve(GrossDiag) #We find the Inverse of that matrix
BMatrix <-
  GrossDiagInv %*% (A) %*% GrossDiag #We find the matrix B in accordance with the method described in Section 3.2

Ghosh = solve(Identity - BMatrix)# G = (I-B)^-1, as described in Section 3.2

GDPShocks <- read.csv("GDP_shocks_est.csv")

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

for (i in seq(from = 1, to = 4, by = 1)) {
  VA <- vector(mode = 'numeric', length = 69)
  #NAICS 11 - Farms are 88.7% of Sector Mark
  VA[0] = GDPShocks[, i + 2][1] * .887
  VA[1] = GDPShocks[, i + 2][1] * .113
  
  #NAICS 21 -> "Oil and Gas are 72.5%
  VA[2] = GDPShocks[, i + 2][2] * .725
  VA[3] = GDPShocks[, i + 2][2] * .173
  VA[4] = GDPShocks[, i + 2][2] * .102
  
  #NAICS 22
  VA[5] = GDPShocks[, i + 2][3]
  
  #NAICS 23
  VA[6] = GDPShocks[, i + 2][4]
  
  #NAICS 31
  VA[7] = GDPShocks[, i + 2][5] * .0238
  VA[8] = GDPShocks[, i + 2][5] * .0232
  VA[9] = GDPShocks[, i + 2][5] * .0415
  VA[10] = GDPShocks[, i + 2][5] * .0623
  VA[11] = GDPShocks[, i + 2][5] * .0656
  VA[12] = GDPShocks[, i + 2][5] * .0637
  VA[13] = GDPShocks[, i + 2][5] * .0228
  VA[14] = GDPShocks[, i + 2][5] * .1160
  VA[15] = GDPShocks[, i + 2][5] * .0487
  VA[16] = GDPShocks[, i + 2][5] * .0126
  VA[17] = GDPShocks[, i + 2][5] * .0289
  VA[18] = GDPShocks[, i + 2][5] * .1667
  VA[19] = GDPShocks[, i + 2][5] * .0083
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
  VA[53] = GDPShocks[, i + 2][13] * .9073
  
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
  
  VAm <- matrix(VA, nrow = 1)
  Lem <- matrix(VA, ncol = 1)
  Output <- (VA %*% Ghosh)
  LeonOutput <- (Leontief %*% Lem)
  
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
  CondensedOutput[7] <- sum(Output[c(28, 29, 30, 31)])
  CondensedLeonOutput[7] <- sum(LeonOutput[c(28, 29, 30, 31)])
  
  # NAICS 48-49 - Transportation and Warehousing
  CondensedOutput[8] <-
    sum(Output[c(32, 33, 34, 35, 36, 37, 38, 39)])
  CondensedLeonOutput[8] <-
    sum(LeonOutput[c(32, 33, 34, 35, 36, 37, 38, 39)])
  
  # NAICS 51 - Information
  CondensedOutput[9] <- sum(Output[c(40, 41, 42, 43)])
  CondensedLeonOutput[9] <- sum(LeonOutput[c(40, 41, 42, 43)])
  
  # NAICS 52 - Finance and Insurance
  CondensedOutput[10] <- sum(Output[c(44, 45, 46, 47)])
  CondensedLeonOutput[10] <- sum(LeonOutput[c(44, 45, 46, 47)])
  
  # NAICS 53 - Real Estate and Rental and Leasing
  CondensedOutput[11] <- sum(Output[c(48, 49)])
  CondensedLeonOutput[11] <- sum(LeonOutput[c(48, 49)])
  
  # NAICS 54 - Professional, Scientific, and Technical Services
  CondensedOutput[12] <- sum(Output[c(50, 51, 52)])
  CondensedLeonOutput[12] <- sum(LeonOutput[c(50, 51, 52)])
  
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
  CondensedOutput[16] <- sum(Output[c(57, 58, 59, 60)])
  CondensedLeonOutput[16] <- sum(LeonOutput[c(57, 58, 59, 60)])
  
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
  CondensedOutput[20] <- sum(Output[c(66, 67, 68, 69)])
  CondensedLeonOutput[20] <- sum(LeonOutput[c(66, 67, 68, 69)])
  
  
  # Create Frame and LeonFrame for the current iteration
  Frame <-
    data.frame(
      "Decile" = i * 2.5,
      "Type" = "GDP",
      "Industries" = Industries,
      "Output" = as.vector(Output)
    )
  LeonFrame <-
    data.frame(
      "Decile" = i * 2.5,
      "Type" = "Leon",
      "Industries" = Industries,
      "Output" = as.vector(LeonOutput)
    )
  # create condensed frame and LeonFrame
  CondensedFrame <-
    data.frame(
      "Decile" = i * 2.5,
      "Type" = "GDP",
      "NAICSIndustries" = NAICSIndustries,
      "Output" = as.vector(CondensedOutput)
    )
  
  CondensedLeonFrame <-
    data.frame(
      "Decile" = i * 2.5,
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

  # Append to the condensed aggregated data frame
  condensed_aggregated_frame <- rbind(condensed_aggregated_frame, condensed_combined_frame)
  
  ## Mutate gdp shocks to conform with upstream and downstream dataframes ##
  
  perc_column <- paste0("perc", i * 25)
  direct_impact_df <- GDPShocks %>%
    mutate(
      Output = .[[perc_column]],
      Type = "Direct",        
      Decile = i * 2.5           
    ) %>%
    select(Decile, Type, NAICSIndustries, Output)
  
  bound_data <- rbind(CondensedFrame, CondensedLeonFrame, direct_impact_df)
  
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
    mutate(Scenario = i * 25) %>%
    select(Scenario, everything())
  
  # Append pivoted and concatenated impacts to a table for export
  table_frame <- rbind(table_frame, sum_scenario_data)
  
  # Create the stack plots of all the impacts
  stacked_plot <- ggplot(data = bound_data, aes(x = Output, y = reorder(NAICSIndustries, Output), fill = Type)) +
    geom_bar(stat = "identity", width = 0.9, position = "stack") +
    scale_fill_manual(values = c("Direct" = "#E41A1C", "Leon" = "#4DAF4A", "GDP" = "#2D72B4"),
                      labels = c("Direct" = "Direct GDP Shock", "Leon" = "Demand Side Impact", "GDP" = "Supply Side Impact")) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("") +
    xlab("Daily Losses (In Billions, US $)") +
    labs(
      fill = "Micro-Economic Impact",
      title = paste(
        "(", intToUtf8(64 + i), ") Economic Impact from Space-Weather Induced",
        i * 25,
        "% \nPower Failure, by NAICs Classification"
      )
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(vjust = 1, hjust = 1, size = 9),
      axis.text.y = element_text(hjust = 1, size = 9, vjust = 1),
      plot.title = element_text(size = 10, family = "Times New Roman"),
      legend.text = element_text(size = 9),
      text = element_text(family = "Times New Roman")
    )
  
  
  
  plotGDP <- ggplot(data = CondensedFrame, aes(x = Output, y = reorder(NAICSIndustries, Output))) +
    geom_bar(stat = "identity", width = 0.9, fill = "#2D72B4", position = position_dodge2(width = 0.9)) +
    ylab("") +
    xlab("Daily Losses (In Billions, US $)") +
    theme(
      axis.title.x = element_text(size = 9, margin = margin(t = 9)),
      axis.title.y = element_text(size = 9),
      plot.title = element_text(size = 10)
    ) +
    labs(
      fill = "Daily Impact",
      title = paste(
        "(", intToUtf8(64 + i), ") Economic Impacts from Space-Weather Induced",
        i * 25,
        "% \nFailure by NAICs Classification"
      )
    ) +
    geom_text(
      aes(label = signif(Output, 3), hjust = -.1),
      size = 4
    ) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 9),
      text = element_text(family = "Times New Roman")
    )
  
  
  plotGDP2 <- ggplot(data = CondensedLeonFrame, aes(x = Output, y = reorder(NAICSIndustries, Output))) +
    geom_bar(stat = "identity", width = 0.9, fill = "#2D72B4", position = position_dodge2(width = 0.9)) +
    xlab("Total GDP Output (In Billions, US $)") +
    ylab("") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
      axis.text.y = element_text(hjust = 1, size = 9, angle = 45, vjust = 1),
      plot.title = element_text(size = 10),
    ) +
    labs(
      fill = "Type of \nExpenditure",
      title = paste(
        "(", intToUtf8(64 + i), ") Economic Impacts from Space-Weather Induced",
        i * 25,
        "% \nFailure by NAICs Classification"
      )
    ) +
    geom_text(
      aes(label = signif(Output, 3), hjust = -.1),
      size = 4
    ) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 9),
      text = element_text(family = "Times New Roman")
    )
  
  
  GraphList[[i]] <- plotGDP
  LeonList[[i]] <- plotGDP2
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

## Condense the aggregated Data Further ##

# Summarize data by Type and Decile
summarized_data <- aggregated_frame %>%
  group_by(Type, Decile) %>%
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
  ggplot(leon_data, aes(x = as.factor(Decile * 10), y = MeanOutput)) +
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
  ggplot(gdp_data, aes(x = as.factor(Decile * 100), y = MeanOutput)) +
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
  labs(x = "Decile",
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
    x = as.factor(Decile * 10),
    y = TotalOutput,
    fill = Type
  )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Percentage of Businesses without Power", y = "Total Ouput ($)", title = "GDP Data Across Deciles") +
  theme_minimal()

# Display the plots
print(leon_dot_plot)
print(gdp_bar_chart)


dev.off()
