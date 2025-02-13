#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Statistical Analysis of Regional Factors on Oysters and Rockweed

#Author: Grant McKown (james.mckown@unh.edu)


#--------------------------------------------
#Chapter 1: Package Library
#--------------------------------------------

#Data Organization
library(tidyverse)
library(stringr)
library(broom)

#Data Visualization
library(ggplot2)
library(patchwork)
library(wesanderson)
library(ggfortify)


#--------------------------------------------
#Chapter 2: Load the Site-level Dataset
#--------------------------------------------

#Import the Dataframe of rockweed - oyster data summarized at the site-level data with added metrics

site_stats <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Data Summarized by Site.csv") %>%
  #Remove sites that were not completed in the field
  filter(is.na(Elevation_NAVD88m) != TRUE) %>%
  mutate(Nearest_Oyster_Distance_km = ifelse(is.na(Distance_Oyster_Reef_km) == TRUE,
                                              Distance_Oyster_Farm_km,
                                             Distance_Oyster_Reef_km),
         Nearest_Oyster = ifelse(is.na(Distance_Oyster_Reef_km) == TRUE,
                                 "Oyster Farm",
                                 "Natural Oyster Reef"))

glimpse(site_stats)


#------------------------------------------
# Regressions of site-level metrics
#------------------------------------------

# The main metrics at the site-level to considered for analysis are 
  # Oyster Density, Height ~ Distance to Mouth of Estuary
  # Oyster Density, Height ~ Distance to Oyster Farms
  # Rockweed Depth, Length ~ Distance to Mouth of Estuary

#Task 1: Regression of Oyster Density to the Distance to Oyster Farms

density_farm_mod <- lm(Oyster_Density ~ Nearest_Oyster_Distance_km,
                       data = site_stats)

autoplot(density_farm_mod)

anova(density_farm_mod)


density_anova <- tidy(anova(density_farm_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\Density - Distance Farm and Reef ANOVA.csv")


#Task 2: Regression of Oyster Height to the Distance to Oyster Farms

height_farm_mod <- lm(Oyster_Height_mm ~ Nearest_Oyster_Distance_km,
                       data = site_stats)

autoplot(height_farm_mod)

anova(height_farm_mod)

height_anova <- tidy(anova(height_farm_mod))

write.csv(height_anova,
          "Rockweed Analysis\\Output Stats\\Height - Distance Farm and Reef ANOVA.csv")


#Task 3: Regression of Rockweed Canopy Depth to Mouth of the Estuary

depth_mod <- lm(Rockweed_Depth_cm ~ Total_Distance_to_Estuary_Mouth_km,
                       data = site_stats)

autoplot(depth_mod)

anova(depth_mod)

depth_anova <- tidy(anova(depth_mod))

write.csv(depth_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Depth - Distance to Mouth ANOVA.csv")


#Task 4: Regression of Rockweed Canopy Length to Mouth of the Estuary

length_mod <- lm(Rockweed_Length_cm ~ Total_Distance_to_Estuary_Mouth_km,
                data = site_stats)

autoplot(length_mod)

anova(length_mod)

length_anova <- tidy(anova(length_mod))

write.csv(depth_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Length - Distance to Mouth ANOVA.csv")


#Task 5: Exponential decay function of Oyster Density to Distance 

log_density_mod <- lm(log(Oyster_Density + 1) ~ Nearest_Oyster_Distance_km,
                        data = site_stats)

autoplot(log_density_mod)

anova(log_density_mod)

log_density_anova <- tidy(anova(log_density_mod))

write.csv(log_density_anova,
          "Rockweed Analysis\\Output Stats\\Site Density - Distance Log ANOVA.csv")



density_pred <- ggpredict(log_density_mod,
                           terms = paste("Nearest_Oyster_Distance_km [", paste(seq(0, 25, 0.1), collapse = ","), "]"),
                           interval = "confidence") %>%
  rename(Nearest_Oyster_Distance_km = x,
         Oyster_Density = predicted) %>%
  group_by(Nearest_Oyster_Distance_km) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low))



site_graph <- ggplot(
  data = site_stats,
  aes(x = Nearest_Oyster_Distance_km,
      y = Oyster_Density)) +
  geom_point(
    aes(fill = Region),
    shape = 21, size = 6.5, alpha = 0.65) + 
#  geom_ribbon(data = density_pred,
 #             aes(x = Nearest_Oyster_Distance_km,
  #                ymax = conf.high,
   #               ymin = conf.low),
    #          alpha = 0.5, fill = "lightgrey") +
 # geom_line(data = density_pred,
  #          aes(x = Nearest_Oyster_Distance_km,
   #             y = Oyster_Density),
    #        linetype = "dashed", linewidth = 1.5) + 
 # geom_smooth(
  #   method = "lm",
   # alpha = 0.65, linetype = "dashed", linewidth = 1.5,
    # colour = "black", fill = "lightgrey") + 
  labs(y = "Oyster Density (# / m2)",
       x = "Nearest Distance to Oyster Farm or Reef (km)") +
   scale_x_continuous(limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.2)) +
    scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2)) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16))

site_graph


ggsave(site_graph,
       filename = "Rockweed Analysis\\Output Figures\\Oyster Density - Distance - 1.0 km.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)


