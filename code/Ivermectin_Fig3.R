################################################################################
#                                                                              #
# PROJECT: Changes in the scope & intent of US state legislation on ivermectin #
#          and pharmacy dispensing trends, 2020–2026                           #                                                                           #
# CODED BY: Jessica Ibiebele                                                   #
# DATE: Jan 21, 2026 (Updated: October 13, 2026)                               #
#                                                                              #
################################################################################

## Manuscript Figure 3: Distribution of state-level ivermectin legislative activity 
## by number of bills introduced and passage status (January 2020-September 2026) 

# Set working directory
setwd('/Users/jessicaibiebele/Documents/Ivermectin/Manuscripts/Scientific Reports Submission')

# Load packages
library(ggplot2)
library(sf)
library(dplyr)
library(usmap) 
library(ggpattern)

# Load data 
state_data <- read_xlsx('Data_Figure_3.xlsx')

# Get US state map data
states_map <- usmap::us_map(regions = "states") %>%
  st_as_sf()

# Join data to map
map_data <- states_map %>%
  left_join(state_data, by = c("full" = "state"))

map_data <- map_data %>%
  filter(!abbr %in% c("PR", "DC"))

# Highlight states that have passed legislation
highlight_states <- c("Arkansas", "Idaho", "Iowa", "Louisiana", "Missouri", "North Dakota",
                      "Tennessee", "Texas")
map_data <- map_data %>%
  mutate(
    pattern_type = ifelse(full %in% highlight_states, "stripe", "none")
  )


# Create the choropleth map
ggplot(map_data) +
  geom_sf_pattern(
    aes(fill = density, pattern = pattern_type),
    color = "black",          # border color
    linewidth = 0.2,          # border thickness
    pattern_fill = "gray",   # color of lines
    pattern_density = 0.2,     # how dense the lines are
    pattern_alpha = 0.4, 
    pattern_spacing = 0.02
  ) +
  geom_sf_text(
    aes(label = abbr,
        color = ifelse(density >= 8, "white", "black")),
    size = 2.5
  ) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "white",
    high = "#08306B",
    limits = c(0, 15),
    breaks = c(0, 4, 8, 12, 15),
    name = "Number of Bill Introductions"
  ) +
  labs(
    title = "Density of Bill Introductions by State"
  ) +
  scale_pattern_manual(
    values = c("stripe" = "stripe", "none" = "none")
  ) + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave(
  "figure3.tiff",
  width = 12,   # was 7
  height = 8,   # was 5
  units = "in",
  dpi = 300,
  compression = "lzw"
)






