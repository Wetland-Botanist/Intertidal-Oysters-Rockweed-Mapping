#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Script 7 - Statistical Analysis of Plot-level Metrics

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


#-------------------------------------------
#Chapter 2: BAI - Biomass Regression
#------------------------------------------

#Regression of extracted values of brown algae index and the dry biomass of collected macroalgae

#Task 1: Import the compiled datasets of BAI with plots and the biomass datasets, Merge datasets together

biomass <- read.csv("Rockweed Analysis\\Input Data\\Rockweed Biomass .csv")

glimpse(biomass)

bai_plots <- read.csv("Rockweed Analysis\\Input DAta\\BAI Plot Values.csv") %>%
  select(Site, Plot, Elevation_NAVD88m, BAI_Extracted, Lidar_Extracted, NDWI_Extracted, RBR_Extracted)

glimpse(bai_plots)


biomass_bai <- left_join(biomass, bai_plots, by = c("Site", "Plot")) %>%
  mutate(across(BAI_Extracted:RBR_Extracted,
                ~round(., 3)))

glimpse(biomass_bai)

write.csv(biomass_bai,
          "Rockweed Analysis\\Formatted Datasets\\Rockweed Biomasss - Extracted Values.csv")


#Task 2: Simple linear regression between dry weight and extracted remote sensing values

#Sub-Task 1: Dry Weight - Brown Algae Index

bai_model <- lm(Dry_Weight_g ~ BAI_Extracted,
                data = biomass_bai)

autoplot(bai_model)

#Remove an outlier of New Castle Commons - Plot 4

biomass_bai_format <- biomass_bai %>%
  filter(Site != "Newcastle Commons" |   
           Plot != 4)

bai_model <- lm(Dry_Weight_g ~ BAI_Extracted,
                data = biomass_bai_format)

autoplot(bai_model)


bai_anova <- tibble(anova(bai_model))

bai_anova

write.csv(bai_anova, 
          "Rockweed Analysis\\Output Stats\\Biomass - BAI Regression ANOVA.csv")



#Sub-Task: Dry Biomass - Plot Elevation

elev_model <- lm(Dry_Weight_g ~ Elevation_NAVD88m,
                 data = biomass_bai_format)

autoplot(elev_model)

elev_anova <- tibble(anova(elev_model))

elev_anova

write.csv(elev_anova, 
          "Rockweed Analysis\\Output Stats\\Biomass - Elevation ANOVA.csv")



#Sub-Task: Dry Biomass - Plot Elevation

ndwi_model <- lm(Dry_Weight_g ~ NDWI_Extracted,
                 data = biomass_bai_format)

autoplot(ndwi_model)

ndwi_anova <- tibble(anova(ndwi_model))

ndwi_anova

write.csv(ndwi_anova, 
          "Rockweed Analysis\\Output Stats\\Biomass - NDWI ANOVA.csv")

biomass_value_graph <- ggplot(
  data = biomass_bai_format,
  aes(x = Elevation_NAVD88m,
      y = Dry_Weight_g)) +
  geom_point(
    aes(fill = Region),
    shape = 21, size = 6, alpha = 0.65) + 
#  geom_smooth(
 #   method = "lm",
  #  alpha = 0.65, linetype = "dashed", linewidth = 1.5,
   # colour = "black", fill = "lightgrey") + 
  labs(y = "Dry Rockweed Biomass (g)",
       x = "Plot Elevation (NAVD88 m)") +
 # scale_x_continuous(limits = c(-0.30, 0.50),
  #                   seq = c(-0.40, 0.40, 0.10)) +
#  scale_y_continuous(limits = c(0, 250),
  #                   seq = c(0, 250, 50)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.80, 0.80),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16))

biomass_value_graph


ggsave(biomass_value_graph,
       filename = "Rockweed Analysis\\Output Figures\\Rockweed Biomass - Elev.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)




#-----------------------------------------------------------
# Chapter 3: Correlation Analyses between Rockweed Metrics
#-----------------------------------------------------------

plot_data <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Plot Data.csv")

glimpse(plot_data)

#Task 1: Correlation Matrix between rockweed metrics

rockweed_correlation <- plot_data %>%
  select(Rockweed_Cover, Canopy_Depth_cm, Canopy_Length_cm, ASCO_Age) %>%
  as.matrix(.) %>%
  rcorr(., type = "pearson")

rockweed_corr_r <- rockweed_correlation$r

rockweed_corr_p <- rockweed_correlation$P

write.csv(rockweed_corr_r,
          "Rockweed Analysis\\Output Stats\\Rockweed Plot Metrics Corr r.csv")

write.csv(rockweed_corr_p,
          "Rockweed Analysis\\Output Stats\\Rockweed Plot Metrics Corr p.csv")


#Task 2: Visualize correlation matrix between rockweed metrics


# function to format the scatterplots
# could change method to lm, gam, whatever other things you
# can use in geom_smooth. can also change colors, transparency, etc.
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "navy", alpha = 0.8) +
    geom_smooth(method = method, color = "darkorange", 
                se = FALSE, linewidth = 0.9, ...)
  p
}

# build the plot and correlation matrix
# with 'method' can use pearson, spearman, kendall
# make sure your data frame is either all numeric or
# that you subset the numeric columns

rockweed_matrix <- plot_data %>%
  select(Rockweed_Cover, Canopy_Depth_cm, Canopy_Length_cm, ASCO_Age)

rockweed_corr <- rockweed_matrix |> 
  ggpairs(upper = list(continuous = wrap("cor", 
                                         method = "pearson", 
                                         use = "pairwise.complete.obs",
                                         colour = "black")),
          lower = list(continuous = wrap(lowerFn)))

rockweed_corr_graph <- rockweed_corr + 
  theme(
    axis.text = element_text(size = 10, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, colour = "black")
  )

rockweed_corr_graph


ggsave(rockweed_corr_graph,
       height = 10, width = 12, dpi = 300,
       filename = "Rockweed Analysis\\Output Figures\\Rockweed Correlation.jpg")



#----------------------------------------------------------
#Chapter 4: Plot Regressions of Oyster Metrics
#---------------------------------------------------------

#Plot-level regressions will inspect possible correlations between plot elevation and oyster characteristics

#Task 1: Reduce the Site summary dataset to just sites with oyster presence

plots <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Plot Data.csv")

glimpse(plots)

sites_oysters <- read.csv("Rockweed Analysis\\Input Data\\Rockweed - Oyster Data Summarized By Site.csv") %>%
  filter(Oyster_Density > 0)

glimpse(sites_oysters)

plots_oysters <- plots %>%
  filter(Site %in% sites_oysters$Site) %>%
  mutate(Oyster_Height = ifelse(is.na(Plot1_Oyster_Height_mm) == TRUE, Plot2_Oyster_Height_mm, Plot1_Oyster_Height_mm))

glimpse(plots_oysters)


#Task 2: Regression of Oyster Density with Plot Elevation

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

density_mod <- lm(Oyster_Density ~ Elevation_m + Site,
                  data = plots_oysters)

autoplot(density_mod)

#Remove an outlier: Wagon Hill Farm East - Plot 2 (row 42)

plots_rockweed_oysters <- plots_oysters %>%
  filter(Site != "Wagon Hill Farm East",
         Plot != 2)

#Redo the analysis and double-check assumptions of linearity

density_mod <- lm(Oyster_Density ~ Elevation_m + Site,
                  data = plots_rockweed_oysters)

autoplot(density_mod)

anova(density_mod)

density_anova <- tidy(anova(density_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster Density - Elevation Plot.csv")


density_pred <- ggpredict(density_mod,
                          terms = "Elevation_m [all]",
                          interval = "confidence") %>%
  rename(Elevation_m = x,
         Oyster_Density = predicted)


#Task 3: Regression of Oyster Heights with Plot Elevation

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

oyster_height_plots <- plots_oysters %>%
  filter(is.na(Oyster_Height) !=)

height_mod <- lm(Oyster_Height ~ Elevation_m + Site,
                  data = plots_oysters)

autoplot(height_mod)

#No need to remove an outlier from the oyster height - elevation dataset

anova(height_mod)

height_anova <- tidy(anova(height_mod))

write.csv(height_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster height - Elevation Plot.csv")

height_pred <- ggpredict(height_mod,
                          terms = "Elevation_m [all]",
                          interval = "confidence") %>%
  rename(Elevation_m = x,
         Oyster_Height = predicted)



#Task 4: Regression of Oyster Density with Canopy Depth

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

density_mod <- lm(Oyster_Density ~ Canopy_Depth_cm + Site,
                 data = plots_oysters)

autoplot(density_mod)

#Remove an outlier: Wagon Hill Farm East - Plot 2 (row 42)

plots_rockweed_oysters <- plots_oysters %>%
  filter(Site != "Wagon Hill Farm East",
         Plot != 2)

density_mod <- lm(Oyster_Density ~ Canopy_Depth_cm + Site,
                  data = plots_rockweed_oysters)

autoplot(density_mod)

anova(density_mod)

density_anova <- tidy(anova(density_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster Density - Depth Plot.csv")

density_pred <- ggpredict(density_mod,
                         terms = "Canopy_Depth_cm [all]",
                         interval = "confidence") %>%
  rename(Canopy_Depth_cm = x,
         Oyster_Density = predicted)



#Task 5: Regression of Oyster Height with Canopy Depth

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

height_mod <- lm(Oyster_Height ~ Canopy_Depth_cm + Site,
                  data = plots_oysters)

autoplot(height_mod)

anova(height_mod)

height_anova <- tidy(anova(height_mod))

write.csv(height_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster height - Depth Plot.csv")

height_pred <- ggpredict(height_mod,
                          terms = "Canopy_Depth_cm [all]",
                          interval = "confidence") %>%
  rename(Canopy_Depth_cm = x,
         Oyster_Height = predicted)




#Task 6: Regression of Oyster Density with Canopy Length

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

density_mod <- lm(Oyster_Density ~ Canopy_Length_cm + Site,
                  data = plots_oysters)

autoplot(density_mod)

#Remove an outlier: Wagon Hill Farm East - Plot 2 (row 42)

plots_rockweed_oysters <- plots_oysters %>%
  filter(Site != "Wagon Hill Farm East",
         Plot != 2)

density_mod <- lm(Oyster_Density ~ Canopy_Length_cm + Site,
                  data = plots_rockweed_oysters)

autoplot(density_mod)

anova(density_mod)

density_anova <- tidy(anova(density_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster Density - Length Plot.csv")

density_pred <- ggpredict(density_mod,
                          terms = "Canopy_Length_cm [all]",
                          interval = "confidence") %>%
  rename(Canopy_Length_cm = x,
         Oyster_Density = predicted)



#Task 6: Regression of Oyster Height with Canopy Length

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

height_mod <- lm(Oyster_Height ~ Canopy_Length_cm + Site,
                 data = plots_oysters)

autoplot(height_mod)

anova(height_mod)

height_anova <- tidy(anova(height_mod))

write.csv(height_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Oyster Height - Length Plot.csv")

height_pred <- ggpredict(height_mod,
                         terms = "Canopy_Length_cm [all]",
                         interval = "confidence") %>%
  rename(Canopy_Length_cm = x,
         Oyster_Height = predicted)


#Task 6: Visualize the relationships with a plug - n - play ggplot code


plot_graph <- ggplot(
  data = height_pred,
  aes(x = Canopy_Length_cm,
      y = Oyster_Height)) +
  geom_point(aes(x = Canopy_Length_cm,
                 y = Oyster_Height),
             data = plots_oysters,
             fill = "darkblue", shape = 21, 
             size = 6.5, alpha = 0.65) + 
 # geom_ribbon(aes(ymin = conf.low,
  #               ymax = conf.high),
   #         alpha = 0.75, fill = "lightgrey") +
  #  geom_line(linetype = "dashed", linewidth = 1.5, colour = "black") + 
  labs(y = "Oyster Height (mm)",
       x = "Rockweed Canopy Length (cm)") +
  #scale_x_continuous(limits = c(20, 32),
  #                breaks = seq(20, 32, 2)) +
  #  scale_y_continuous(limits = c(-2.5, 10),
  #                 breaks = seq(0, 10, 2)) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"))

plot_graph


ggsave(plot_graph,
       filename = "Rockweed Analysis\\Output Figures\\Plot Oyster Height - Rockweed Length.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)





#----------------------------------------------------------
#Chapter 5: Plot Regressions of Rockweed Metrics
#---------------------------------------------------------

#Rockweed Metrics will consider all plots within the entire study, not just plots with oysters

#Task 1: Regression of Rockweed Length with Plot Elevation

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

length_mod <- lm(Canopy_Length_cm ~ Elevation_m + Site,
                  data = plots)

autoplot(length_mod)

# Data visually meets all assumptions of linearity

anova(length_mod)

length_anova <- tidy(anova(length_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Rockweed Length - Elevation Plot.csv")


length_pred <- ggpredict(length_mod,
                          terms = "Elevation_m [all]",
                          interval = "confidence") %>%
  rename(Elevation_m = x,
         Canopy_Length_cm = predicted)



#Task 2: Regression of Rockweed Depth with Plot Elevation

#First, create a model and visually confirm it meets parametric assumptions of ANOVA
#Site is added as a blocking factor in the data analysis

depth_mod <- lm(Canopy_Depth_cm ~ Elevation_m + Site,
                 data = plots)

autoplot(depth_mod)

# Removed one outlier - Row 35, New Castle Ave Plot 5

rockweed_plots <- plots %>%
  filter(Site != "New Castle Ave" |
           Plot != 5)

depth_mod <- lm(Canopy_Depth_cm ~ Elevation_m + Site,
                data = rockweed_plots)


anova(depth_mod)

length_anova <- tidy(anova(depth_mod))

write.csv(density_anova,
          "Rockweed Analysis\\Output Stats\\ANOVA Rockweed Depth - Elevation Plot.csv")


depth_pred <- ggpredict(depth_mod,
                         terms = "Elevation_m [all]",
                         interval = "confidence") %>%
  rename(Elevation_m = x,
         Canopy_Depth_cm = predicted)




plot_graph <- ggplot(
  data = length_pred,
  aes(x = Elevation_m,
      y = Canopy_Length_cm)) +
  geom_point(aes(x = Elevation_m,
                 y = Canopy_Length_cm),
             data = rockweed_plots,
             fill = "darkblue", shape = 21, 
             size = 6.5, alpha = 0.65) + 
  geom_ribbon(aes(ymin = conf.low,
                 ymax = conf.high),
            alpha = 0.75, fill = "lightgrey") +
    geom_line(linetype = "dashed", linewidth = 1.5, colour = "black") + 
  labs(y = "Rockweed Frond Length (cm)",
       x = "Elevation (NAVD88 m)") +
  #scale_x_continuous(limits = c(20, 32),
  #                breaks = seq(20, 32, 2)) +
  #  scale_y_continuous(limits = c(-2.5, 10),
  #                 breaks = seq(0, 10, 2)) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"))

plot_graph


ggsave(plot_graph,
       filename = "Rockweed Analysis\\Output Figures\\Plot Rockweed Length - Elevation Blocking.jpg",
       limitsize = FALSE, units = "in",
       height = 8, width = 12, dpi = 300)












