#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Script 8 - Spat on Shell Analysis

#Author: Grant McKown (james.mckown@unh.edu)


#----------------------------------------------
# Chapter 1: Import Package Library
#---------------------------------------------

#Data Organization
library(tidyverse)
library(stringr)
library(broom)

#Data Analysis
library(Hmisc)
library(GGally)

#Data Visualization
library(ggplot2)
library(patchwork)
library(wesanderson)
library(ggfortify)
library(ggeffects)


#-----------------------------------------------------
# Chapter 2: Import and Format Spat on Shell Analysis
#-----------------------------------------------------

spat <- read.csv("Rockweed Analysis\\Input Data\\Oyster Spat on Shell Data.csv")

glimpse(spat)

spat_bag <- spat %>%
  mutate(Bag = as.character(Bag),
         Dead.Alive = ifelse(Timing == "Deployment", "A", Dead.Alive)) %>%
  filter(Dead.Alive == "A") %>%
  group_by(Timing, Site, Bag) %>%
    summarise(
      Alive_Oysters = n(),
      Mean_Height = mean(Height_mm, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Mean_Height = round(Mean_Height, 1))

glimpse(spat_bag)


#---------------------------------------------
# Chapter 3: Calculate Mortality & Growth
#---------------------------------------------

#First, calculate the mortality of each pair of bags between deployment and collection

mortality <- spat_bag %>%
  #First, remove Bag 20 from WHF, since most of the shell fell out of the bag during the experiment
  filter(Bag != 20 |
           Site != "Wagon Hill Farm") %>%
  #Remove all of the bags that were not found during Collection (~2 - 3 bags out of 20)
  group_by(Site, Bag) %>%
  mutate(Timing.Count = length(Alive_Oysters)) %>%
  ungroup() %>%
  filter(Alive_Oysters > 1) %>%
  group_by(Site, Bag) %>%
  #Caluclate the Mortality for each bag and double-check with the number of alive oysters
  reframe(
    Alive.Deployment = Alive_Oysters[Timing == "Deployment"],
    Alive.Collection = Alive_Oysters[Timing == "Collection"],
    Mortality = 100 - ((Alive_Oysters[Timing == "Collection"] / Alive_Oysters[Timing == "Deployment"]) * 100) ) %>%
  mutate(
    Mortality = round(Mortality, 1)) %>%
  arrange(Site_Bag)

glimpse(mortality)


#Second, calculate the average and standard error of mortality across each site

mortality_stats <- mortality %>%
  group_by(Site) %>%
  summarise(
    mortality_mean = mean(Mortality, na.rm = TRUE),
    mortality_se = sd(Mortality, na.rm = TRUE) / sqrt(n())) %>%
  ungroup() %>%
  mutate(across(mortality_mean:mortality_se,
         ~round(., 1)))

glimpse(mortality_stats)

#Third, calculate the difference in mean height between paired bags

growth <- spat_bag %>%
  #First, remove Bag 20 from WHF, since most of the shell fell out of the bag during the experiment
  filter(Bag != 20 |
           Site != "Wagon Hill Farm") %>%
  #Remove all of the bags that were not found during Collection (~2 - 3 bags out of 20)
  group_by(Site, Bag) %>%
  mutate(Timing.Count = length(Alive_Oysters)) %>%
  ungroup() %>%
  filter(Alive_Oysters > 1) %>%
  group_by(Site, Bag) %>%
  reframe(
    Height.Deployment = Mean_Height[Timing == "Deployment"],
    Height.Collection = Mean_Height[Timing == "Collection"],
    Height_Difference = Height.Collection - Height.Deployment) %>%
  arrange(Site, Bag)

glimpse(growth)    


    

#Fourth, calculate the average and standard error of height difference and mean height across site

growth_stats <- growth %>%
  group_by(Site) %>%
  summarise(
    Difference_mean = mean(Height_Difference, na.rm = TRUE),
    Difference_se = sd(Height_Difference, na.rm = TRUE) / sqrt(n())) %>%
  ungroup() %>%
  mutate(
    across(Difference_mean:Difference_se,
           ~round(., 1)))

glimpse(growth_stats)    


    
#Fifth, combine the Mortality and Growth tables into one table for export to CSV

stats_bag <- merge(mortality, growth, by = c("Site", "Bag"))

glimpse(stats_bag)    

write.csv(stats_bag,
          "Rockweed Analysis\\Output Stats\\Oyster Bag Growth and Mortality.csv")    
    
    
    
      