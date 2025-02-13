#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Oyster & Other Bivalve Histogram

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


#--------------------------------------------------------
# Chapter 2: Import Live Oyster & Bivalve Height Dataset
#-----------------------------------------------------------


oyster_height <- read.csv("Rockweed Analysis\\Input Data\\Rockweed_Oysters_Live_Lengths.csv")

glimpse(oyster_height)


height_format <- oyster_height %>%
  gather(Oyster_1:Hard_Clam_1,
         key = "Specimen",
         value = "Height") %>%
  filter(!is.na(Height)) %>%
  mutate(Species = ifelse(str_detect(Specimen, "Oyster"), "Eastern Oyster", Specimen),
         Species = ifelse(str_detect(Specimen, "Blue_Mussel"), "Blue Mussel", Species),
         Species = ifelse(str_detect(Specimen, "Ribbed_Mussel"), "Ribbed Mussel", Species),
         Species = ifelse(str_detect(Specimen, "Hard_Clam"), "Hard Clam", Species),
         Species = ifelse(str_detect(Specimen, "European_Oyster"), "European Oyster", Species)) %>%
  mutate(Species = factor(Species,
                             levels = c("Eastern Oyster", "European Oyster",
                                        "Blue Mussel", "Ribbed Mussel", "Hard Clam")))


#-------------------------------------------------------------------------
# Chapter 3: Create histogram for heights of oysters and other bivalves
#-------------------------------------------------------------------------

height_format_graph <- height_format %>%
  filter(Species != "Hard Clam",
         Species != "European Oyster")


histogram <- ggplot(
  data = height_format_graph,
  aes(group = Species)) +
  geom_histogram(aes(
    x = Height),
    position = position_dodge(),
    binwidth = 5,
    colour = "black", width = 1.5) +
  labs(x = "Bivalve Height (mm)",
       y = "Count") +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    expand = c(0,0)) + 
  scale_x_continuous(
    limits = c(0, 103),
    breaks = seq(0, 100, 10),
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
    axis.text = element_text(size = 18, colour = "black"),     
    strip.background = element_blank(),
    strip.text = element_text(size = 18)) +
  facet_wrap(~Species,
             nrow = 2, ncol = 2)

histogram


ggsave(histogram,
       height = 8, width = 14, dpi = 300,
       limitsize = FALSE, units = "in",
       filename = "Rockweed Analysis\\Output Figures\\Live Bivalves Histogram.jpg")





























