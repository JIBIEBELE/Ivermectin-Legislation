################################################################################
#                                                                              #
# PROJECT: Changes in the scope & intent of US state legislation on ivermectin #
#          and pharmacy dispensing trends, 2020–2026                           #                                                                           #
# CODED BY: Jessica Ibiebele                                                   #
# DATE: Jan 21, 2026 (Updated: October 13, 2026)                               #
#                                                                              #
################################################################################

## Manuscript Figure 1: Annual counts of bills introduced (January 2020 to 
## September 2026) by legislative objective: off-label access, standing order access, 
## over-the-counter access, and provider liability/disciplinary protections. 

# Load packages
library(ggplot2)
library(tidyverse)
library(readxl)

# Load data
fig_1 <- read_xlsx('Data_Figure_1.xlsx')

# Data manipulation 
fig_1$Topic <- factor(
  fig_1$Topic,
  levels = c("Off-Label Access", 
             "Standing Order Access", 
             "Over-the-Counter Access",
             "Shield Providers from Discipline",
             "Shield Providers from Liability")
)


# Create plot
ggplot(fig_1, aes(
  x = Year,
  y = bill_count,
  color = Topic,
  linetype = Topic
)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "Shield Providers from Liability" = "black",
      "Shield Providers from Discipline" = "darkgray",
      "Off-Label Access" = "dodgerblue",
      "Over-the-Counter Access" = "violetred",
      "Standing Order Access" = "forestgreen"
    )
  ) + 
  geom_point() +
  scale_linetype_manual(
    values = c(
      "Shield Providers from Liability" = "dashed",
      "Shield Providers from Discipline" = "dotted",
      "Off-Label Access" = "solid",
      "Over-the-Counter Access" = "solid",
      "Standing Order Access" = "solid"
    )
  ) +
  labs(
    x = "Year",
    y = "Number of Bills Introduced",
    color = "Topic Area",
    linetype = "Topic Area"
  ) +
  scale_x_continuous(
    breaks = 2020:2026,
    labels = c("2020", "2021", "2022", "2023", "2024", "2025", "2026")
  ) +
  theme_minimal(base_size = 12)

# Save plot
ggsave(
  filename = "figure1.png",
  plot = last_plot(),
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

