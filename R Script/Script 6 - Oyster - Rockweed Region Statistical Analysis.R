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
library(ggeffects)

#--------------------------------------------
#Chapter 2: Load the Regional Dataset
#--------------------------------------------

region_data <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Data Summarized by Region.csv") %>%
mutate(Rockweed_Standardized_Intertidal_Area = Rockweed_Standardized_Intertidal_Area * 100)

glimpse(region_data)



#------------------------------------------------------------
#Chapter 2: Relationships between Rockweed & Water Quality
#------------------------------------------------------------

#The rockweed metrics tested against water quality metrics are:
# (1) Rockweed acreage within a region

#Task 1: Water Quality - Monthly Mean Salinity

rockweed_wq_mean <- lm(Rockweed_Standardized_Intertidal_Area ~ Monthly_Mean_Salinity_psu,
                      data = region_data)

autoplot(rockweed_wq_mean)

anova(rockweed_wq_mean)

rockweed_wq_mean_anova <- tidy(anova(rockweed_wq_mean))

write.csv(rockweed_wq_mean_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Acreage - WQ Mean ANOVA.csv")


# Task 2: Water Quality - Monthly Minimum Salinity

rockweed_wq_min <- lm(Rockweed_Standardized_Intertidal_Area ~ Max_Salinity_Below_10_psu,
                       data = region_data)

autoplot(rockweed_wq_min)

anova(rockweed_wq_min)

rockweed_wq_min_anova <- tidy(anova(rockweed_wq_min))

write.csv(rockweed_wq_min_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Acreage - WQ Min ANOVA.csv")



# Task 4: Water Quality - Monthly Maximum Salinity

rockweed_wq_max <- lm(Rockweed_Standardized_Intertidal_Area ~ Monthly_Max_Salinity_psu,
                      data = region_data)

autoplot(rockweed_wq_max)

anova(rockweed_wq_max)

rockweed_wq_max_anova <- tidy(anova(rockweed_wq_max))

write.csv(rockweed_wq_max_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Acreage - WQ Max ANOVA.csv")



#Task 5: Exponential Decay Relationship of Days below 10 psu and Ratio of Rockweed to Intertidal Area



rockweed_wq_days <- lm(log(Rockweed_Standardized_Intertidal_Area + 1) ~ Max_Salinity_Below_10_psu,
                       data = region_data)

autoplot(rockweed_wq_days)

anova(rockweed_wq_days)

rockweed_wq_days_anova <- tidy(anova(rockweed_wq_days))

write.csv(rockweed_wq_days_anova,
          "Rockweed Analysis\\Output Stats\\Rockweed Acreage - WQ Days LOG ANOVA.csv")


salinity_pred <- ggpredict(rockweed_wq_days,
                           terms = paste("Max_Salinity_Below_10_psu [", paste(seq(0, 30, 0.1), collapse = ","), "]"),
                           interval = "confidence") %>%
  rename(Max_Salinity_Below_10_psu = x,
         Standardized_Area = predicted)



region_graph <- ggplot() +
  geom_point(data = region_data,
               aes(x = Max_Salinity_Below_10_psu,
                   y = Rockweed_Standardized_Intertidal_Area),
               shape = 21, size = 6.5, 
             alpha = 0.65, fill = "darkblue") + 
  geom_ribbon(data = salinity_pred,
              aes(x = Max_Salinity_Below_10_psu,
                  ymax = conf.high,
                  ymin = conf.low),
              alpha = 0.5, fill = "lightgrey") +
  geom_line(data = salinity_pred,
            aes(x = Max_Salinity_Below_10_psu,
                y = Standardized_Area),
            linetype = "dashed", linewidth = 1.5) + 
#   geom_smooth(
 #    method = "lm",,
  # alpha = 0.65, linetype = "dashed", linewidth = 1.5,
   # colour = "black", fill = "lightgrey") + 
  labs(y = "Ratio of Rockweed to Intertidal Zone Area (%)",
       x = "Maximum Consecutive Days Below 10 psu") +
  #scale_x_continuous(limits = c(20, 32),
   #                breaks = seq(20, 32, 2)) +
  #  scale_y_continuous(limits = c(-2.5, 10),
   #                 breaks = seq(0, 10, 2)) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"))

region_graph


ggsave(region_graph,
       filename = "Rockweed Analysis\\Output Figures\\Rockweed Ratio - WQ Days.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)
























