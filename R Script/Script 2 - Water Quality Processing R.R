#Project: Mapping Distribution of Intertidal Rockweed and Oysters in Great Bay
#R Script: Processing Water Quality Data

#Author: Grant McKown (james.mckown@unh.edu)


#--------------------------------------------
#Chapter 1: Package Library
#--------------------------------------------

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)
library(purrr)
library(stringr)

#--------------------------------------------
#Chapter 2: Load Annual Water Quality CSVs
#--------------------------------------------

#Task 1: Format the CDMO Files

wq15 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2015.csv") %>%
  filter(Sal < 35)

wq16 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2016.csv")

wq17 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2017.csv")

wq18 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2018.csv")

wq19 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2019.csv")

wq20 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2020.csv")

wq21 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2021.csv")

wq22 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2022.csv")

wq23 <- read.csv("Water Quality Analysis\\Input Data\\grbsqwq2023.csv")


wq_all <- rbind(wq15, wq16, wq17, wq18, wq19, wq20, wq21, wq22, wq23) %>%
  filter(!is.na(Sal)) %>%
  select(-contains("F_"), -X, -ChlFluor, -isSWMP, -Historical, -ProvisionalPlus,
         -Level, -cLevel) %>%
  mutate(DateTimeStamp = as.POSIXct(DateTimeStamp, format = '%m/%d/%Y %H:%M')) %>%
  mutate(Year = year(DateTimeStamp),
         Month = month(DateTimeStamp),
         Day = day(DateTimeStamp)) %>%
  filter(Month >= 5, 
         Month <= 11)

glimpse(wq_all)


wq_all <- wq_all %>%
  select(StationCode, DateTimeStamp, Year, Month, Day, Temp:Turb)

glimpse(wq_all)

write.csv(wq_all,
          "Water Quality Analysis\\Formatted Datasets\\grbsq Compiled 2015 - 2023.csv")






#Task 2: Format the EPA Files

wq17 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2017_Summer.csv")

wq17_2 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2017_Fall.csv")

wq18 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2018.csv")

wq19 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2019.csv")

wq20 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2020.csv")

wq21 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2021.csv")

wq22 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2022.csv")

wq23 <- read.csv("Water Quality Analysis\\Input Data\\grbcml_2023.csv")


wq_all <- read.csv("Water Quality Analysis\\Input Data\\grbcmlwq_all.csv") %>%
 # filter(!str_detect(RESULT.COMMENT, "<-3>")) %>%
  filter(!is.na(Sal)) %>%
  filter(Sal > 0) %>%
  mutate(DateTimeStamp = as.POSIXct(DateTimeStamp, format = '%m/%d/%Y %H:%M')) %>%
  mutate(Year = year(DateTimeStamp),
         Month = month(DateTimeStamp),
         Day = day(DateTimeStamp)) %>%
  filter(Month >= 5, 
         Month <= 11)


write.csv(wq_all,
          "Water Quality Analysis\\Formatted Datasets\\grbcml Compiled 2018 - 2023.csv")


#--------------------------------------------------
#Chapter 3: Process and format Water Quality Data
#-------------------------------------------------

#wq_all <- read.csv("Water Quality Analysis\\Input Data\\grbcmlwq_all.csv") %>%
#  filter(!is.na(Sal),
#           Sal != "-999") %>%
#  mutate(DateTimeStamp = as.POSIXct(DateTimeStamp, format = '%m/%d/%Y %H:%M')) %>%
#  mutate(Year = year(DateTimeStamp),
#         Month = month(DateTimeStamp),
#         Day = day(DateTimeStamp))

#write.csv(wq_all,
#          "Water Quality Analysis\\Formatted Datasets\\grbcml Compiled 2015 - 2023.csv")


#-------------------------------------------------
#Chapter 4: Calculate Monthly Water Quality Stats
#-------------------------------------------------

wq_monthly <- wq_all %>%
  group_by(Year, Month) %>%
  summarise(across(Sal,
            list(mean = ~mean(., na.rm = TRUE),
            se = ~sd(., na.rm = TRUE)/sqrt(n()),
            min = ~min(., na.rm = TRUE),
            max = ~max(., na.rm = TRUE)))) %>%
  mutate(across(Sal_mean:Sal_max, ~round(., 1))) %>%
  ungroup()

glimpse(wq_monthly)

write.csv(wq_monthly,
          "Water Quality Analysis\\Output Stats\\grbcml - Monthly Averages Each Year.csv")


wq_monthly_across_time <- wq_monthly %>%
  group_by(Month) %>%
  summarise(across(contains("mean"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n()))),
            across(contains("min"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n()))),
           across(contains("max"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n())))) %>%
  mutate(across(Sal_mean_mean:Sal_max_se, ~round(., 1)))

glimpse(wq_monthly_across_time)

write.csv(wq_monthly_across_time,
          "Water Quality Analysis\\Output Stats\\grbcml - Monthly Across Time 17 - 23.csv")



wq_annual_lowsalt_days <- wq_all %>%
  filter(Sal <= 10) %>%
  distinct(Day, Month, Year) %>%
  mutate(DateTime = paste(Year, Month, Day, sep = "/"),
         DateTimeStamp = as.POSIXct(DateTime, 
                                    format = "%Y/%m/%d")) %>%
group_by(Year) %>%
  mutate(Days_Between = as.numeric(DateTimeStamp - lag(DateTimeStamp))) %>%
nest() %>%
  mutate(rle_salt = map(.x = data,
                          ~rle(.x$Days_Between)),
         salt_days = map(.x = rle_salt,
                         ~.x$lengths[.x$values == 1]),
         max_salt_days = map(.x = salt_days,
                             ~max(.x, na.rm = TRUE))) %>%
  select(Year, max_salt_days) %>%
  unnest(max_salt_days) %>%
  ungroup() %>%
  mutate(max_salt_days = max_salt_days + 1) %>%
  mutate(max_salt_days = ifelse(max_salt_days == "-Inf", 1, max_salt_days))

glimpse(wq_annual_lowsalt_days)

write.csv(wq_annual_lowsalt_days,
          "Water Quality Analysis\\Output Stats\\grbcml - Annual Max Low Salt days 17 - 23.csv")


#-------------------------------------------------------------------------
#Chapter 5: Calculate Daily and then Overall Statistics
#-------------------------------------------------------------------------

#Task 1 - Calculate Mean, Min, and Max Values of all metrics for each day

wq_daily <- wq_all %>%
  group_by(Day, Month, Year) %>%
  summarise(across(Sal,
                list(mean = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE)/sqrt(n()),
                     min = ~min(., na.rm = TRUE),
                     max = ~max(., na.rm = TRUE)))) %>%
  mutate(across(Sal_mean:Sal_max, ~round(., 1))) %>%
  ungroup()

glimpse(wq_daily)

write.csv(wq_daily,
          "Water Quality Analysis\\Formatted Datasets\\grbcml - Daily Averages 17 - 23.csv")

#Task 2 - Calculate Average Mean, Min, and Max values of all metrics across the entire monitoring period

wq_daily_across_time <- wq_daily %>%
  summarise(across(contains("mean"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n()))),
            across(contains("min"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n()))),
            across(contains("max"),
                   list(mean = ~mean(., na.rm = TRUE),
                        se = ~sd(., na.rm = TRUE)/sqrt(n())))) %>%
  mutate(across(Sal_mean_mean:Sal_max_se, ~round(., 2)))

glimpse(wq_daily_across_time)

write.csv(wq_daily_across_time,
          "Water Quality Analysis\\Output Stats\\grbcml - Daily Across Time 17 - 23.csv")

#-------------------------------------------------
#Chapter 6: Graph the Salinity Trends
#-----------------------------------------------

#Task 1: Graph the Average Max, Mean, and Min Salinity for each month across the monitoring timeframe

wq_graph <- ggplot(
  data = wq_monthly_across_time,
  aes(x = Month)) +
  geom_errorbar(aes(ymin = Sal_mean_mean - Sal_mean_se, 
                    ymax = Sal_mean_mean + Sal_mean_se),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.5),
                colour = "grey") + 
  geom_errorbar(aes(ymin = Sal_max_mean - Sal_max_se, 
                    ymax = Sal_max_mean + Sal_max_se),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.5),
                colour = "forestgreen") + 
  geom_errorbar(aes(ymin = Sal_min_mean - Sal_min_se, 
                    ymax = Sal_min_mean + Sal_min_se),
                size = 1.5, width = 0.25, position = position_dodge(width = 0.5),
                colour = "orange") + 
  geom_point(aes(y = Sal_mean_mean),
             shape = 21, size = 8, colour = 'black', stroke = 1.25, fill = "grey",  
             position = position_dodge(width = 0.5)) + 
  geom_point(aes(y = Sal_min_mean),
             shape = 21, size = 8, colour = 'black', stroke = 1.25, fill = "orange",  
             position = position_dodge(width = 0.5)) + 
  geom_point(aes(y = Sal_max_mean),
             shape = 21, size = 8, colour = 'black', stroke = 1.25, fill = "forestgreen",  
             position = position_dodge(width = 0.5)) + 
  scale_y_continuous(limits = c(0, 32.5),
                     breaks = seq(0, 32, 4),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(4.5, 11.5),
                     breaks = seq(5, 11, 1),
                     expand = c(0,0)) +
  labs(x = 'Month (2015 - 17, 2019 - 21)', 
       y = 'Salintiy (psu)') + 
  theme_bw() +
  theme(
    legend.position = c(0.10, 0.10),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 18, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"))


wq_graph


ggsave(wq_graph,
       filename = "Water Quality Analysis\\Output Figures\\grbcml - Salinity Graph.jpg",
       units = "in",
       height = 10, width = 16, dpi = 300, limitsize = FALSE)


#Task 2: Graph the maximum number of days of salinity reaching or falling below 10 psu

wq_annual_lowsalt_days$Year <- as.integer(wq_annual_lowsalt_days$Year)

min_salinity_graph <- ggplot(
  data = wq_annual_lowsalt_days) +
  geom_bar(aes(x = Year, 
               y = max_salt_days),
             position = position_dodge(0.9),
              stat = "identity",
            linewidth = 1.25, 
           fill = "cornflowerblue", colour = "black") + 
  scale_y_continuous(limits = c(0, 41),
                     breaks = seq(0, 40, 5),
                     expand = c(0,0)) +
  scale_x_continuous(limits= c(2014.5, 2021.5),
                     breaks = seq(2015, 2021, 1)) + 
  labs(x = "", 
       y = 'Max Timespan Reaching or Below 10 psu (days)') + 
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 18, colour = "black"),
    strip.background = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"))


min_salinity_graph


ggsave(min_salinity_graph,
       filename = "Water Quality Analysis\\Output Figures\\grbcml - Low Salinity Graph.jpg",
       units = "in",
       height = 8, width = 12, dpi = 300, limitsize = FALSE)

