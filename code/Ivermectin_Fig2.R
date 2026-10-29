################################################################################
#                                                                              #
# PROJECT: Changes in the scope & intent of US state legislation on ivermectin #
#          and pharmacy dispensing trends, 2020–2026                           #                                                                           #
# CODED BY: Jessica Ibiebele                                                   #
# DATE: Jan 21, 2026                                                           #
#                                                                              #
################################################################################

## Manuscript Figure 2: Annual number of bills (2020-2025) by provision type: 
## indicated for COVID-19 treatment, indicated for COVID-19 prophylaxis and treatment, 
## and prohibited speech about indication. 

# Load packages
library(ggplot2)
library(tidyverse)
library(readxl)

# Load data
fig_2 <- read_xlsx('Data_Figure_2.xlsx')

# Data manipulation 
fig_2$Indication <- factor(
  fig_2$Indication,
  levels = c("COVID-19 Treatment", 
             "COVID-19 Prophylaxis and Treatment", 
             "Prohibited Speech")
)

# Create plot
ggplot(fig_2, aes(
  x = Year,
  y = Bill_Count,
  fill = Indication
)) +
  geom_col(
    position = position_dodge(width = 0.8),
    color = "black",        # outlines improve grayscale readability
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "COVID-19 Treatment" = "#1F4E79",
      "COVID-19 Prophylaxis and Treatment" = "#7A7A7A",
      "Prohibited Speech" = "#D17C00"
    )
  ) +
  labs(
    x = "Year",
    y = "Number of Bills Introduced",
    fill = "Indication"
  ) +
  scale_x_continuous(
    breaks = 2020:2025
  ) +
  theme_minimal(base_size = 12)

# Save 
ggsave(
  "figure2.png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
