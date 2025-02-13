#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Oyster & Rockweed Field Assessment Analysis

#Author: Grant McKown (james.mckown@unh.edu)



#----------------------------------------------
# Chapter 1: Import Package Library
#---------------------------------------------

#Data Organization
library(dplyr)
library(tidyr)
library(stringr)

#Data Visualization
library(ggplot2)
library(patchwork)
library(wesanderson)


#-----------------------------------------------------------------
# Chapter 2: Import & Format Live Oyster & Bivalve Height Dataset
#------------------------------------------------------------------

#Task 1 - Import the dataset
plot_data <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Plot Data.csv")

glimpse(plot_data)


#Task 2: Format the dataset

plot_format <- plot_data %>%
  mutate(oyster_height = ifelse(is.na(Plot1_Oyster_Height_mm), 
                                Plot2_Oyster_Height_mm, Plot1_Oyster_Height_mm)) %>%
  select(Site, Region, Plot, Elevation_m,
         Rockweed_Cover, Canopy_Depth_cm, Canopy_Length_cm, ASCO_Age,
         Substrate, Hardened, 
         Oyster_Density, oyster_height, Oyster_Boxes,
         BlueMussel_Density, BlueMussel_Boxes, Plot_BlueMussel_Height_mm,
         RibbedMussel_Density, RibbedMussel_Boxes, Plot_RibbedMussel_Height_mm)


#Task 3: Reduce the dataset to only sites where oysters were observed

oyster_format <- plot_format %>%
  group_by(Site) %>%
  mutate(oyster_check = colSums(across(Oyster_Density))) %>%
  filter(oyster_check != 0) %>%
  ungroup()

#------------------------------------------------------------------------------------
# Chapter 3: Calculate Descriptive Statistics of Rockweed and Oysters
#-------------------------------------------------------------------------------------

# Task 1: Descriptive statistics of rockweed and oysters by region

#Note - Experimental unit for descriptive stats is plot

region_stats <- plot_format %>%
  select(Site, Region, Plot, Elevation_m,
         Rockweed_Cover, Canopy_Depth_cm, Canopy_Length_cm, ASCO_Age,
         Oyster_Density, oyster_height, 
         BlueMussel_Density, Plot_BlueMussel_Height_mm,
         RibbedMussel_Density, Plot_RibbedMussel_Height_mm) %>%
  group_by(Region) %>%
  reframe(across(c(Elevation_m:Plot_RibbedMussel_Height_mm),
                    list(mean = ~mean(., na.rm = TRUE),
                         se = ~sd(., na.rm = TRUE) / sqrt(n() )))) %>%
  ungroup() %>%
  mutate(across(Elevation_m_mean:Plot_RibbedMussel_Height_mm_se,
                ~round(., 2)))

write.csv(region_stats,
          "Rockweed Analysis\\Output Stats\\Region Descriptive Statsitics.csv")

# Task 2: Descriptive statistics of rockweed and oysters by site

#Note - experimental unit for descriptive stats is plot

site_stats <- plot_format %>%
  select(Site, Region, Plot, Elevation_m,
         Rockweed_Cover, Canopy_Depth_cm, Canopy_Length_cm, ASCO_Age,
         Oyster_Density, oyster_height,
         BlueMussel_Density, Plot_BlueMussel_Height_mm,
         RibbedMussel_Density, Plot_RibbedMussel_Height_mm) %>%
  group_by(Site) %>%
  reframe(across(c(Elevation_m:Plot_RibbedMussel_Height_mm),
                 list(mean = ~mean(., na.rm = TRUE),
                      se = ~sd(., na.rm = TRUE) / sqrt(n() )))) %>%
  ungroup() %>%
  mutate(across(Elevation_m_mean:Plot_RibbedMussel_Height_mm_se,
                ~round(., 2)))

write.csv(site_stats,
          "Rockweed Analysis\\Output Stats\\Site Descriptive Statsitics.csv")


#----------------------------------------------------------------------
# Chapter 4: Bar Graphs to visualize descriptive statistics by Region
#----------------------------------------------------------------------

region_format <- region_stats %>%
  mutate(Region = ifelse(Region == "Cocheco - Salmon Falls Rivers", "Cocheco - Salmon", Region)) %>%
  mutate(Region = format(Region,
                      levels = c("Portsmouth & Coast", "Piscataqua River",
                                 "Cocheco - Salmon Falls Rivers",
                                 "Bellamy River", "Little Bay",
                                 "Oyster River", "Great Bay")))



region_bargraph <- ggplot(
  data = region_format,
  aes(x = Region,
      y = Oyster_Density_mean)) +
  geom_bar(aes(fill = Region),
           position = position_dodge(0.9), stat = "identity", 
           size = 1.25, colour = "black", linewidth = 1.25) +
  geom_errorbar(aes(
    ymin = Oyster_Density_mean - Oyster_Density_se,
    ymax = Oyster_Density_mean + Oyster_Density_se),
    width = 0.5, size = 1.1) + 
  labs(
    x = "",
    y = ("Oyster Density (# / m2)")) + 
  scale_y_continuous(limits = c(0, 10),
                     breaks = seq(0, 10, 2),
                     expand = c(0,0)) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 18))

region_bargraph

ggsave(region_bargraph,
       filename = "Rockweed Analysis\\Output Figures\\Oyster Density by Region Bar Graph.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)


#-------------------------------------------------------
# Chapter 5: Calculate Oyster Population by Region
#-----------------------------------------------------

region_data <- read.csv("Rockweed Analysis\\Input Data\\Oyster - Rockweed Regional Compiled Stats.csv")

glimpse(region_data)

oyster_pop <- region_data %>%
  mutate(Oyster_Density_acre = round(Oyster_Density * 4046.86, 1),
         Oyster_Pop = Rockweed_Area_Acre * Oyster_Density_acre) %>%
  mutate(Oyster_Pop = round(Oyster_Pop, 1)) %>%
  select(Region, Rockweed_Area_Acre, Oyster_Density, Oyster_Density_acre, Oyster_Pop)

glimpse(oyster_pop)

write.csv(oyster_pop,
          "Rockweed Analysis\\Output Stats\\Oyster Population Estimates.csv")
