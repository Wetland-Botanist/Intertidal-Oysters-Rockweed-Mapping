#Project: Rockweed Mapping - Spectral Profiles and Analysis


library(ggplot2)
library(dplyr)
library(tidyr)


#Analysis of 2015 Leaf-off Imagery

leafoff <- read.csv("Spectral Profiles Over Time//Leaf_off_Spectral_Samples.csv") %>%
  rename(Training = training_poly) %>%
  mutate(Training = ifelse(Training == 0, "Rockweed", 
                           ifelse(Training == 1, "Marsh",
                                  ifelse(Training == 2, "Water",
                                         ifelse(Training == 3, "Mudflat",
                                                ifelse(Training == 4, "Canopy", Training)))))) %>%
  select(-system.index, -.geo)

glimpse(leafoff)


#Calculate the mean and standard deviation of the bands for the 2015 leaf-off imagery for 
# each training classification

leafoff_mean <- leafoff %>%
  select(R, G, B, N, Training) %>%
  group_by(Training) %>%
  summarise(across(R:N, mean)) %>%
  ungroup() %>%
  gather(R:N, key = "band", value = "mean") %>%
  arrange(Training, band)

leafoff_sd <- leafoff %>%
  select(R, G, B, N, Training) %>%
  group_by(Training) %>%
  summarise(across(R:N, sd)) %>%
  ungroup() %>%
  gather(R:N, key = "band", value = "sd") %>%
  arrange(Training, band)

leafoff_stats <- leafoff_mean %>% 
  merge(leafoff_sd, by = c('Training', 'band')) %>%
  mutate(band = ifelse(band == "R", "Red",
                       ifelse(band == "G", "Green",
                              ifelse(band == "B", "Blue",
                                     ifelse(band == "N", "Near Infrared", band))))) %>%
  mutate(band = factor(band, levels = c('Red', 'Green', 'Blue', 'Near Infrared'))) %>%
  mutate(across(mean:sd, ~round(., 2)))

write.csv(leafoff_stats,
          "Spectral Profile Stats//Leafoff 2015 Mean Band Values.csv")


#Graph the mean and standard deviation of bands for each training classification
# in the 2015 Leaf-off Imagery

leafoff_plot <- ggplot(data = leafoff_stats,
       aes(x = band, y = mean, group = Training)) + 
  geom_errorbar(aes(ymin = mean - (sd), 
                    ymax = mean + (sd),
                    colour = Training),
                size = 1.5, width = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(aes(fill = Training),
             shape = 21, size = 8, colour = 'black', stroke = 1.25,  
             position = position_dodge(width = 0.5)) + 
  labs(x = '', 
       y = 'Mean Reflectance') + 
  scale_fill_manual(values = c("dark green", "gray", "brown", "orange", "blue")) +
  scale_colour_manual(values = c("dark green", "gray", "brown", "orange", "blue")) + 
  theme_bw() +
  theme(
    legend.position = c(0.075, 0.10),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"))

leafoff_plot

ggsave(leafoff_plot,
       filename = "Spectral Profiles\\Leafoff_Profiles_Plot.jpg",
       limitsize = FALSE, 
       dpi = 300, height = 10, width = 14)

#Calculate the selected vegetation indices of each training group of the 2015 Leaf-off Imagery


indices <- leafoff %>%
  mutate(NDRBR = (R - B) / (R + B),
         NDWI = (G - N) / (G + N),
         NDBAI = (N - G) / (N + G))

glimpse(indices)

indices_mean <- indices %>%
  group_by(Training) %>%
  summarise(across(c(NDRBR:NDBAI, Lidar), mean)) %>%
  ungroup() %>%
  gather(NDRBR, NDWI, NDBAI, Lidar,
         key = "index", value = "mean")

indices_sd <- indices %>%
  group_by(Training) %>%
  summarise(across(c(NDRBR:NDBAI, Lidar), sd)) %>%
  ungroup() %>%
  gather(NDRBR, NDWI, NDBAI, Lidar,
         key = "index", value = "sd") %>%
  mutate(sd_15 = sd * 1.5)



indices_stats <- indices_mean %>%
  merge(indices_sd, by = c("Training", "index")) %>%
  mutate(threshold_max = mean + sd_15,
          threshold_min = mean - sd_15) %>%
  mutate(across(mean:threshold_min, ~round(., 3)))

write.csv(indices_stats, 
          "Spectral Profile Stats\\Leafoff Vegetation Indices Stats.csv")







#Graph the selected selected vegetation indices of each training group of the 2015 Leaf-off Imagery

indices_plot <- ggplot(data = indices_stats,
       aes(x = Training, y = mean)) + 
         geom_errorbar(aes(ymin = mean - sd_15, 
                           ymax = mean + sd_15,
                           colour = Training),
                       size = 1.25, width = 0.75, linetype = 'dashed') + 
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd,
                    colour = Training),
                size = 1.25, width = 0.75) + 
  geom_point(aes(fill = Training),
             shape = 21, size = 6, colour = 'black') + 
  scale_fill_manual(values = c("dark green", "gray", "brown", "orange", "blue")) +
  scale_colour_manual(values = c("dark green", "gray", "brown", "orange", "blue")) + 
  labs(x = "", y = "") + 
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black")) + 
  facet_wrap(~index,
             ncol = 2,
             nrow = 2,
             scales = 'free')


indices_plot

ggsave(indices_plot,
       filename = "Spectral Profiles\\Leafoff_Indices_Plot.jpg",
       limitsize = FALSE, 
       dpi = 300, height = 10, width = 15)






#Analysis of NAIP Imagery

naip <- read.csv("Spectral Profiles Over Time//Spectral Profiles Over Time.csv") %>%
  rename(Training = training_poly) %>%
  mutate(Training = ifelse(Training == 0, "Rockweed", 
                           ifelse(Training == 1, "Marsh",
                                  ifelse(Training == 2, "Water",
                                         ifelse(Training == 3, "Mudflat",
                                                ifelse(Training == 4, "Canopy", Training))))))

glimpse(naip)

naip_mean <- naip %>%
  select(R, G, B, N, Training, year) %>%
  group_by(Training, year) %>%
  summarise(across(R:N, mean)) %>%
  ungroup() %>%
  gather(R:N, key = "band", value = "mean") %>%
  arrange(Training, year, band)

naip_sd <- naip %>%
  select(R, G, B, N, Training, year) %>%
  group_by(Training, year) %>%
  summarise(across(R:N, sd)) %>%
  ungroup() %>%
  gather(R:N, key = "band", value = "sd") %>%
  arrange(Training, year, band)

naip_stats <- naip_mean %>% 
  merge(naip_sd, by = c('Training', 'year', 'band')) %>%
  mutate(band = ifelse(band == "R", "Red",
                       ifelse(band == "G", "Green",
                              ifelse(band == "B", "Blue",
                                     ifelse(band == "N", "Near Infrared", band))))) %>%
  mutate(band = factor(band, levels = c('Red', 'Green', 'Blue', 'Near Infrared')))


write.csv(naip_stats,
          "Spectral Profile Stats\\NAIP Imagery Spectral Profiles Stats.csv")


naip_plot <- ggplot(data = naip_stats,
       aes(x = band, y = mean, group = Training)) + 
  geom_errorbar(aes(ymin = mean - (sd), 
                    ymax = mean + (sd),
                    colour = Training),
                size = 1.25, width = 0.25, position = position_dodge(width = 0.5)) + 
  geom_point(aes(fill = Training),
             shape = 21, size = 6, colour = 'black', position = position_dodge(width = 0.5)) + 
  labs(x = '', 
       y = 'Mean Reflectance') + 
  scale_fill_manual(values = c("dark green", "yellow", "brown", "orange", "blue")) +
  scale_colour_manual(values = c("dark green", "yellow", "brown", "orange", "blue")) + 
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(size = 20, colour = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black")) + 
  facet_wrap(~year,
             ncol = 3,
             nrow = 2,
             scales = 'free')


ggsave(naip_plot,
       filename = "Spectral Profiles\\NAIP Spectral Profiles Plot.jpg",
       limitsize = FALSE, 
       dpi = 300, height = 12, width = 20)


indices <- naip %>%
  mutate(RBR = (R - B) / (R + B),
         NDWI = (G - N) / (G + N),
         BAI = (N - G) / (N + G))

glimpse(indices)

indices_max <- indices %>%
  group_by(Training, Point) %>%
  summarise(across(RBR:BAI, max)) %>%
  ungroup() %>%
  group_by(Training) %>%
  summarise(across(RBR:BAI, mean)) %>%
  gather(RBR, NDWI, BAI, 
         key = "index", value = "max")

indices_max_sd <- indices %>%
  group_by(Training, Point) %>%
  summarise(across(RBR:BAI, max)) %>%
  ungroup() %>%
  group_by(Training) %>%
  summarise(across(RBR:BAI, sd)) %>%
  gather(RBR, NDWI, BAI, 
         key = "index", value = "sd")

indices_max <- indices_max %>%
  merge(indices_max_sd, by = c("Training", "index"))

write.csv(indices_max,
          "Spectral Profile Stats\\NAIP Vegetation Indices MAX Stats.csv")



naip_max <- ggplot(data = indices_max,
       aes(x = Training, y = max)) + 
  geom_errorbar(aes(ymin = max - (1.5 * sd), 
                    ymax = max + (1.5 * sd),
                    colour = Training),
                size = 1.25, width = 0.75) + 
  geom_point(aes(fill = Training),
             shape = 21, size = 6, colour = 'black') + 
  labs(x = "", y = "") + 
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black")) + 
  facet_wrap(~index,
             ncol = 2,
             nrow = 4,
             scales = 'free')

ggsave(naip_max,
       filename = "Spectral Profiles\\NAIP Indices Max Plot.jpg",
       limitsize = FALSE, 
       dpi = 300, height = 12, width = 20)


indices_min <- indices %>%
  group_by(Training, Point) %>%
  summarise(across(RBR:BAI, min)) %>%
  ungroup() %>%
  group_by(Training) %>%
  summarise(across(RBR:BAI, mean)) %>%
  gather(RBR, NDWI, BAI, 
         key = "index", value = "min")

indices_min_sd <- indices %>%
  group_by(Training, Point) %>%
  summarise(across(RBR:BAI, min)) %>%
  ungroup() %>%
  group_by(Training) %>%
  summarise(across(RBR:BAI, sd)) %>%
  gather( RBR, NDWI, BAI, 
         key = "index", value = "sd")

indices_min <- indices_min %>%
  merge(indices_min_sd, by = c("Training", "index"))

write.csv(indices_min,
          "Spectral Profile Stats\\NAIP Vegetation Indices MIN Stats.csv")


naip_min <- ggplot(data = indices_min,
       aes(x = Training, y = min)) + 
  geom_errorbar(aes(ymin = min - (2 * sd), 
                    ymax = min + (2 * sd),
                    colour = Training),
                size = 1.25, width = 0.75) + 
    geom_point(aes(fill = Training),
             shape = 21, size = 6, colour = 'black') + 
  labs(x = "", y = "") + 
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 20, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black")) + 
  facet_wrap(~index,
             ncol = 2,
             nrow = 4,
             scales = 'free')


ggsave(naip_min,
       filename = "Spectral Profiles\\NAIP Indices Min Plot.jpg",
       limitsize = FALSE, 
       dpi = 300, height = 12, width = 20)




