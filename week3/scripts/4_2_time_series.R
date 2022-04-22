# Apr 18 2022
# F. Dallo

# Call libraries

library(tidyverse)
library(lubridate)
library(here)
library(patchwork) # plot annotation

# Use 'here' to simplify handling paths

here::i_am("ARCH 249 Module 3.Rproj")

# Upload file in memory

## Meteo Station: https://www.ndbc.noaa.gov/station_history.php?station=ftpc1
## Metadata: https://www.ndbc.noaa.gov/measdes.shtml
## Location: https://www.ndbc.noaa.gov/station_page.php?station=ftpc1

meteo_df <- read_rds(file = "week3/data/meteo_SF_FTPC1_2011-2020.rds")

# Data set characteristics:

dim(meteo_df)
head(meteo_df, n = 11)
# note: we have 10 obs per hour
tail(meteo_df)
str(meteo_df)
glimpse(meteo_df)

# Let's work on Atmospheric Temperature

temperature_df <- meteo_df %>%
  select(TIME, ATMP)

# Data set inspection:

summary(temperature_df)
# Note:
# 1- time is a character
# 2- there are some strange values.. such as ATMP mean/median/max


# TIME is a character.. check metadata for info
## TIME.1 Convert character string to time object
#temperature_df$TIME <- lubridate::ymd_hms( temperature_df$TIME, tz = "UTC" )

temperature_df <- temperature_df %>%
  mutate(TIME = lubridate::ymd_hms(TIME, tz = "UTC"))

# TEMPERATURE. There are some strange 999 values.. see meta: these are NAs
## TEMPERATURE.1 Re-code, convert values to NA
temperature_df <- temperature_df %>%
  mutate(ATMP = na_if(x = ATMP, y = 999))

# --------------------------------------
# Make a plot to check if data are sound

temperature_df %>%
  filter(TIME >= "2020-01-01",
         TIME < "2020-01-02") %>%
  ggplot(data = ., aes(x = TIME, y = ATMP)) +
  geom_line() + 
  theme_classic()

# Note:
# 1- There are some missing values?
# 2- It seems weird that at Noon we have the cooler moment of the day..  why?

# temperature_df %>%
#   filter(TIME >= "2020-01-01",
#          TIME < "2020-01-02") %>%
#   view()
# 
# --------------------------------------

## TIME.2 Create a new time object considering local time

temperature_df <- temperature_df %>%
  mutate(TIME_LOCAL = lubridate::with_tz(TIME, tzone = "America/Los_Angeles")) %>%
  glimpse()

# Redo the plot and check now
temperature_df %>%
  select(TIME_LOCAL, ATMP) %>%
  filter(TIME_LOCAL >= "2020-01-01",
         TIME_LOCAL < "2020-01-02") %>%
  ggplot(data = ., aes(x = TIME_LOCAL, y = ATMP)) +
  geom_line() + 
  theme_classic()

# --------------------------------------

## TIME. Check for duplicated rows

temperature_df$TIME[duplicated(as.character(temperature_df$TIME))]
temperature_df$TIME_LOCAL[duplicated((temperature_df$TIME_LOCAL))] 
temperature_df$TIME_LOCAL[duplicated(as.character(temperature_df$TIME_LOCAL))] 
# Note: The same months... every year... the same hour!  

# Which day? Any guess??
wday(ymd("2020-11-01"), label = TRUE)


# --------------------------------------

# Check when change in "Daylight saving time":
#  Daylight saving time 2020 in California began at 2:00 AM on
#  Sunday, March 8
#  and ended at 2:00 AM on
#  Sunday, November 1
#  All times are in Pacific Time.

temperature_df %>%
  filter(TIME_LOCAL >= "2020-03-08 00:00:00",
         TIME_LOCAL < "2020-03-08 05:00:00") %>%
  view()

temperature_df %>%
  filter(TIME_LOCAL >= "2020-10-31 23:00:00",
         TIME_LOCAL < "2020-11-01 05:00:00") %>%
  view()


# Note:
# 1- There is one hour missing in March..
# 2- There are two identical time in November.. a mess.. 
# Comment:
# Depending on the analysis you're carrying you might want different time representation.. 
# We might want to have a Solar Local Time:

# --------------------------------------

## TIME.3 Create a Local Solar Time.

temperature_df <- temperature_df %>% 
  mutate(TIME_LOCAL_SOLAR = TIME - lubridate::hours(8))

temperature_df %>%
  filter(TIME_LOCAL >= "2020-03-08 00:00:00",
         TIME_LOCAL < "2020-03-08 05:00:00") %>%
  view()

temperature_df %>%
  filter(TIME_LOCAL >= "2020-10-31 23:00:00",
         TIME_LOCAL < "2020-11-01 05:00:00") %>%
  view()

summary(temperature_df)
temperature_df$TIME_LOCAL_SOLAR[duplicated(as.character(temperature_df$TIME_LOCAL_SOLAR))] 

# --------------------------------------

## We are using meteorological data.. make sense to use SOLAR LOCAL TIME

# Drop ....
temperature_df[,names(temperature_df) %in% c("TIME_LOCAL_SOLAR", "ATMP")] # more complicated
temperature_df[, c("TIME_LOCAL_SOLAR", "ATMP")] # a bit uncomprehensive
subset(temperature_df, select = c("TIME_LOCAL_SOLAR", "ATMP")) # maybe better

# Tidy solution?
temperature_df_solar <- temperature_df %>% 
  select(TIME_LOCAL_SOLAR, ATMP)

summary(temperature_df_solar)
head(temperature_df_solar)
tail(temperature_df_solar)

# --------------------------------------------------
# We finally have our data-set in Solar time
# and we can start to Group to get some info
# Year average
# Monthly
# note: data set is not perfect.. we have about 8hr missing 
# to have a complete 2011-2020 (10 years summary).. 

# --------------------------------------------------
# Mutate to create "Y" == "year" variable

tmp_df <- temperature_df_solar %>%
  mutate(YEAR = lubridate::year(TIME_LOCAL_SOLAR))

tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  group_by(YEAR) %>%
  #summarise(TIME = mean(TIME_LOCAL_SOLAR, na.rm=TRUE), # we don't really need this.. 
  # option 1, we remove it.. option 2 below
  summarise(
            AVG_TEMP = mean(ATMP, na.rm=TRUE),
            SD_TEMP = sd(ATMP, na.rm=TRUE),
            SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100, # this is cool
            SD_TEMP_PERC2 = sd(ATMP, na.rm=TRUE)/mean(ATMP, na.rm=TRUE)*100, 
            nr = n()
            ) %>%
  glimpse()

# tmp_df %>%
#   filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
#   group_by(YEAR) %>%
#   summarise(TIME = lubridate::year(mean(TIME_LOCAL_SOLAR, na.rm=TRUE)), # but this is redundant.. see "ungroup" below.. 
#     AVG_TEMP = mean(ATMP, na.rm=TRUE),
#     SD_TEMP = sd(ATMP, na.rm=TRUE),
#     SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100,
#     SD_TEMP_PERC2 = sd(ATMP, na.rm=TRUE)/mean(ATMP, na.rm=TRUE)*100, # this is cool
#     nr = n()
#   ) %>%
#   ungroup() %>%
#   select(!YEAR) %>% # so here we remove year and this is more coding for keeping the "TIME" var.. 
#   glimpse()

### Some visualization.. 

# Plot 1a:
tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  group_by(YEAR) %>%
  summarise(
    AVG_TEMP = mean(ATMP, na.rm=TRUE),
    SD_TEMP = sd(ATMP, na.rm=TRUE),
    SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100,
    nr = n()
  ) %>%
  ggplot(data = ., aes(x = YEAR, y = AVG_TEMP)) +
  geom_line(color="#006494") + 
  geom_point(size=3, color="#003554") +
  # geom_point(size=3, color="red") + # red covering the blue
  #geom_line() + #this cover the points! better before
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2011,2020)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#B3B0AE"))
 
# Plot 1b: adding SD
tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  group_by(YEAR) %>%
  summarise(
    AVG_TEMP = mean(ATMP, na.rm=TRUE),
    SD_TEMP = sd(ATMP, na.rm=TRUE),
    SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100,
    nr = n()
  ) %>%
  ggplot(data = .) +
  geom_line(aes(x = YEAR, y = (AVG_TEMP + SD_TEMP )), color = "#006494") + # THIS THE SD
  geom_line(aes(x = YEAR, y = (AVG_TEMP - SD_TEMP )), color = "#006494") + # THIS THE SD
  geom_line(aes(x = YEAR, y = AVG_TEMP), color="#006494") +           # THIS FOR THE AVERAGE 
  geom_point(aes(x = YEAR, y = AVG_TEMP), size=3, color="#003554") +  # THIS FOR THE AVERAGE 
  #geom_line() + #this cover the points! better before
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2011,2020)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#B3B0AE"))

# Plot 1c: adding max and min and segment
tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  group_by(YEAR) %>%
  summarise(
    AVG_TEMP = mean(ATMP, na.rm=TRUE),
    SD_TEMP = sd(ATMP, na.rm=TRUE),
    SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100,
    T_MAX = max(ATMP, na.rm = TRUE),
    T_MIN = min(ATMP, na.rm = TRUE),
    nr = n()
  ) %>%
  ggplot(data = .) +
  geom_point(aes(x = YEAR, y = T_MAX), color = "#051923", bg = "#051923", pch = 24) + #ADD 1 
  geom_point(aes(x = YEAR, y = T_MIN), color = "#051923", bg = "#051923", pch = 25) + #ADD 1 
  geom_segment(aes(x = YEAR, xend = YEAR, y = T_MIN, yend = T_MAX), color = "#00A6FB") +
  geom_line(aes(x = YEAR, y = (AVG_TEMP + SD_TEMP )), color = "#006494") +
  geom_line(aes(x = YEAR, y = (AVG_TEMP - SD_TEMP )), color = "#006494") +
  geom_line(aes(x = YEAR, y = AVG_TEMP), color="#006494") +
  geom_point(aes(x = YEAR, y = AVG_TEMP), size=3, color="#003554") +
  #geom_line() + #this cover the points! better before
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2011,2020)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

# Ok.. but maybe go right to a boxplot???

tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  ggplot(.) +
  geom_boxplot(aes(x = YEAR, y = ATMP, group = YEAR)) +
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2010.5,2020.5)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

# comparison using patchwork ------------------------ START

library(patchwork) # at the end for extra time

p1 <- tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  group_by(YEAR) %>%
  summarise(
    AVG_TEMP = mean(ATMP, na.rm=TRUE),
    SD_TEMP = sd(ATMP, na.rm=TRUE),
    SD_TEMP_PERC = SD_TEMP/AVG_TEMP*100,
    T_MAX = max(ATMP, na.rm = TRUE),
    T_MIN = min(ATMP, na.rm = TRUE),
    nr = n()
  ) %>%
  ggplot(data = .) +
  geom_point(aes(x = YEAR, y = T_MAX), color = "#051923", bg = "#051923", pch = 24) + #ADD 1 
  geom_point(aes(x = YEAR, y = T_MIN), color = "#051923", bg = "#051923", pch = 25) + #ADD 1 
  geom_segment(aes(x = YEAR, xend = YEAR, y = T_MIN, yend = T_MAX), color = "#00A6FB") +
  geom_line(aes(x = YEAR, y = (AVG_TEMP + SD_TEMP )), color = "#006494") +
  geom_line(aes(x = YEAR, y = (AVG_TEMP - SD_TEMP )), color = "#006494") +
  geom_line(aes(x = YEAR, y = AVG_TEMP), color="#006494") +
  geom_point(aes(x = YEAR, y = AVG_TEMP), size=3, color="#003554") +
  #geom_line() + #this cover the points! better before
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2011,2020)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

p2 <- tmp_df %>%
  filter(TIME_LOCAL_SOLAR >= lubridate::ymd("2011-01-01")) %>%
  ggplot(.) +
  geom_boxplot(aes(x = YEAR, y = ATMP, group = YEAR)) +
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2010.5,2020.5)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

p1 / p2
p1 + p2
p1 | p2

# comparison using patchwork ------------------------ END


# --------------------------------------------------
# Mutate to create "Y" == "year" variable
# Mutate to create "M" == "month" variable

tmp_df <- temperature_df_solar %>%
  filter(TIME_LOCAL_SOLAR > lubridate::ymd("2011-01-01")) %>%
  mutate(YEAR = lubridate::year(TIME_LOCAL_SOLAR),
         MONTH = lubridate::month(TIME_LOCAL_SOLAR), 
         MONTH_NAME = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE),
         ) %>%
  glimpse()

# montly boxplot a
tmp_df %>%
  filter(MONTH == 1) %>% # change from 1 to 12
  ggplot(.) +
  geom_boxplot(aes(x = YEAR, y = ATMP, group = YEAR) ) +
  scale_x_continuous(breaks = seq(2011,2020,1), limits = c(2010.5,2020.5)) +
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + #https://ggplot2.tidyverse.org/reference/theme.html
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

# montly boxplot b: climatic analysis over 10 yrs.. see single plots and with patchwork
tmp_df %>%
  ggplot(.) +
  geom_boxplot(aes(x = MONTH, y = ATMP, group = MONTH) ) +
  #scale_x_continuous(breaks = seq(1,12,1), limits = c(0.5,12.5)) +
  scale_x_continuous(breaks = seq(1,12,1), limits = c(0.5,12.5), labels = c(levels(tmp_df$MONTH_ABBR))) + #https://ggplot2.tidyverse.org/reference/scale_continuous.html
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + 
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

tmp_df %>% 
  group_by(MONTH) %>%
  summarise(YEAR = YEAR,
            MONTH = MONTH,
            T_AVG = median(ATMP, na.rm = TRUE),
            n = n()
            ) %>%
  ggplot(.) +
  geom_point(aes(x = MONTH, y = T_AVG, group = MONTH)) +
  scale_x_continuous(breaks = seq(1,12,1), limits = c(0.5,12.5), labels = c(levels(tmp_df$MONTH_ABBR))) +
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + 
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

#p1 | p2

## Strange case.. Season... this is not if a real climatic interest...
# but we can see the Indian Summer! https://en.wikipedia.org/wiki/Indian_summer

# tidy version
tmp_df <- tmp_df %>%
  mutate(SEASON = case_when( MONTH %in% c(1:2,12) ~ "Winter",
                                  MONTH %in% c(3:5) ~ "Spring",
                                  MONTH %in% c(6:8) ~ "Summer",
                                  MONTH %in% c(9:11) ~ "Fall"
                                  ))

# # base R version
# seasons_fun = function(x){
#   if(x %in% 3:5) return("Spring")
#   if(x %in% 6:8) return("Summer")
#   if(x %in% 9:11) return("Fall")
#   if(x %in% c(12,1,2)) return("Winter")
# }
# tmp_df$SEASON = sapply(tmp_df$MONTH, seasons_fun)
# identical(tmp_df$SEASON_tidy, tmp_df$SEASON)

tmp_df %>%
  filter(YEAR == 2015) %>%
  ggplot() +
  geom_boxplot(aes(x = SEASON, y = ATMP, group = SEASON))

#note: there is something wring here....

# make season a factor:

tmp_df <- tmp_df %>%
  mutate(SEASON = factor(SEASON, levels = c("Winter", 
                                            "Spring",
                                            "Summer",
                                            "Fall")
                         )
         )

tmp_df %>%
  filter(YEAR == 2015) %>%
  ggplot() +
  geom_boxplot(aes(x = SEASON, y = ATMP, group = SEASON))

tmp_df %>%
  filter(YEAR == 2015) %>%
  group_by(MONTH) %>%
  summarise(T_AVG = mean(ATMP, na.rm = TRUE)
            ) %>%
  view()

# The Indian summer is real!!


# --------------------------------------------------
# Mutate to create "Y" == "year" variable
# Mutate to create "M" == "month" variable
# Mutate to create "D" == "day" variable

tmp_df <- temperature_df_solar %>%
  filter(TIME_LOCAL_SOLAR > lubridate::ymd("2011-01-01")) %>%
  mutate(YEAR = lubridate::year(TIME_LOCAL_SOLAR),
         MONTH = lubridate::month(TIME_LOCAL_SOLAR), 
         MONTH_NAME = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME_LOCAL_SOLAR),
         DAY_NAME = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME_LOCAL_SOLAR),
         SEASON = case_when( MONTH %in% c(1:2,12) ~ "Winter",
                             MONTH %in% c(3:5) ~ "Spring",
                             MONTH %in% c(6:8) ~ "Summer",
                             MONTH %in% c(9:11) ~ "Fall"
                             )
         ) %>%
  mutate(SEASON = factor(SEASON, levels = c("Winter", 
                                            "Spring",
                                            "Summer",
                                            "Fall")
                         )
         ) %>%
  glimpse()

# lines plot and spend time in pretty face

tmp_df %>%
  group_by(YEAR, MONTH) %>%
  summarise(TIME = lubridate::floor_date(x = mean(TIME_LOCAL_SOLAR, na.rm=TRUE), unit = "day"),
            TEMPERATURE = mean(ATMP, na.rm = T)
  ) %>% 
  ungroup() %>%
  #filter(YEAR == 2011) %>%
  glimpse() %>%
  ggplot(data = ., aes(x = MONTH, y = TEMPERATURE, group = as.factor(YEAR), color=as.factor(YEAR))) +
  geom_line() +
  scale_x_continuous(breaks = seq(1,12,1), limits = c(0.5,12.5), labels = c(levels(tmp_df$MONTH_ABBR))) +
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + 
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))
  
tmp_df %>%
  group_by(YEAR,DAY) %>%
  summarise(TIME = lubridate::floor_date(x = mean(TIME_LOCAL_SOLAR, na.rm=TRUE), unit = "day"),
            TEMPERATURE = mean(ATMP, na.rm = T)
  ) %>% 
  ungroup() %>%
  #filter(YEAR == 2011) %>%
  ggplot(data = ., aes(x = DAY, y = TEMPERATURE, group = YEAR, color=YEAR)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1,31,1), limits = c(0.5,31.5)) +
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + 
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))

# note: 
# do not have much sense to average x day of each month of the year... this is why is flat.. 
# let's compare january over the 10 years!
tmp_df %>%
  filter(MONTH == 1) %>%
  group_by(YEAR,DAY) %>%
  summarise(TIME = lubridate::floor_date(x = mean(TIME_LOCAL_SOLAR, na.rm=TRUE), unit = "day"),
            TEMPERATURE = mean(ATMP, na.rm = T)
  ) %>% 
  ungroup() %>%
  #filter(YEAR == 2011) %>%
  ggplot(data = ., aes(x = DAY, y = TEMPERATURE, group = as.factor(YEAR), color=as.factor(YEAR))) +
  geom_line() +
  scale_x_continuous(breaks = seq(1,31,1), limits = c(0.5,31.5)) +
  scale_y_continuous(limits = c(0,35)) +
  xlab(element_blank()) + 
  ylab("Atmospheric Temperature (°C)") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "#CCCCCC"))


# --------------------------------------------------
# Mutate to create "Y" == "year" variable
# Mutate to create "M" == "month" variable
# Mutate to create "D" == "day" variable
# Mutate to create "h" == "hour" variable

tmp_df <- temperature_df_solar %>%
  filter(TIME_LOCAL_SOLAR > lubridate::ymd("2011-01-01")) %>%
  mutate(YEAR = lubridate::year(TIME_LOCAL_SOLAR),
         MONTH = lubridate::month(TIME_LOCAL_SOLAR), 
         MONTH_NAME = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME_LOCAL_SOLAR),
         DAY_NAME = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME_LOCAL_SOLAR),
         HOUR = lubridate::hour(TIME_LOCAL_SOLAR)
  ) %>%
  glimpse()

# hourly average:
tmp_df_1H <- temperature_df_solar %>%
  filter(TIME_LOCAL_SOLAR > lubridate::ymd("2011-01-01")) %>%
  mutate(YEAR = lubridate::year(TIME_LOCAL_SOLAR),
         MONTH = lubridate::month(TIME_LOCAL_SOLAR), 
         MONTH_NAME = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME_LOCAL_SOLAR),
         DAY_NAME = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME_LOCAL_SOLAR, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME_LOCAL_SOLAR),
         HOUR = lubridate::hour(TIME_LOCAL_SOLAR)
  ) %>%
  group_by(YEAR, MONTH, DAY, HOUR) %>%
  summarise(TIME_HR = lubridate::ceiling_date(x = mean(TIME_LOCAL_SOLAR, na.rm=TRUE), unit = "hour"),
            ATMP_AVG = mean(ATMP, na.rm = TRUE),
            NR_OBS = n()
            ) %>%
  ungroup() %>%
  print(n=10)
  



# LINEAR MODEL... TREND

tmp_df %>%
  group_by(YEAR, MONTH_ABBR) %>%
  summarise(TEMPERATURE = median(ATMP, na.rm = TRUE)) %>%
  filter(MONTH_ABBR == "Jan") %>%
  ungroup() %>%
  glimpse() %>%
  ggplot(data = ., aes(x = YEAR, y = TEMPERATURE)) +
  geom_smooth(method = "lm") +
  geom_point() +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        parse = TRUE, label.x = "left", label.y = "top",
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  plot_annotation(title = paste0("Temperature trend ", "January" ),
                  #subtitle = "Number of thermostat events in California homes during a wildfire episode; days with more events have darker shading",
                  theme = theme(plot.title = element_text(size = 14, colour = "grey20", face = "bold", hjust = 0),
                                plot.subtitle = element_text(size = 10, colour = "grey20", face = "italic", hjust = 0, margin = margin(b = 10)),
                                plot.background = element_rect(fill = "white"))) +
  ylab("Temperature (°C)") +
  xlab(element_blank())
  



## WIND PLOTS

# ok stop with tempearature! Something about wind

# Wind Direction

wind_df <- tmp_df_SF %>%
  select(TIME, WDIR, WSPD)

wind_df %>%
  filter(TIME >= "2020-01-01",
         TIME < "2020-01-02") %>%
  mutate(YEAR = lubridate::year(TIME),
         MONTH = lubridate::month(TIME), 
         MONTH_NAME = lubridate::month(TIME, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME),
         DAY_NAME = lubridate::wday(TIME, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME),
         HOUR = lubridate::hour(TIME)
         ) %>%
  glimpse() %>%
  group_by(HOUR) %>%
  summarise(TIME = round_date(mean(TIME, na.rm=TRUE), unit = "hour"),
            WSPD = mean(WSPD, na.rm = TRUE),
            WDIR = (360+(atan2(sum(sin(WDIR*pi/180)),sum(cos(WDIR*pi/180)))*180/pi))%%360
  ) %>%
  ungroup() %>%
  mutate(WIND_SECTOR = case_when( WDIR > 0 & WDIR <= 22.5 ~ "North",
                                  WDIR > 22.5 & WDIR <= 67.5 ~ "North-East",
                                  WDIR > 67.5 & WDIR <= 112.5 ~ "East",
                                  WDIR > 112.5 & WDIR <= 157.5 ~ "South-East",
                                  WDIR > 157.5 & WDIR <= 202.5 ~ "South",
                                  WDIR > 202.5 & WDIR <= 247.5 ~ "South-West",
                                  WDIR > 247.5 & WDIR <= 292.5 ~ "West",
                                  WDIR > 292.5 & WDIR <= 337.5 ~ "North-West",
                                  WDIR > 337.5 & WDIR <= 360 ~ "North")
         ) %>%
  glimpse() %>%
  ggplot(., mapping = aes(x = TIME, y = WSPD)) +
  geom_segment(aes(x = HOUR,
  #geom_segment(aes(x = lubridate::hour(TIME),
  #geom_segment(aes(x = TIME,
                   y = 0,
                   # xend = HOUR - lubridate::dhours(1 * -cos((90-WDIR) / 360 * 2 * pi)),         # NORMALIZE TO 1 
                   # yend = -1 * (1 * -sin((90-WDIR) / 360 * 2 * pi))#, col = factor(mean_WS_ms)  # NORMALIZE TO 1 
                   xend = HOUR - lubridate::dhours(WSPD * -cos((90-WDIR) / 360 * 2 * pi)),        # ADDING LENGTH AS SPEED 
                   yend = - 1 * (WSPD * -sin((90-WDIR) / 360 * 2 * pi))                             # ADDING LENGTH AS SPEED
  ),
  arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  geom_point(aes(HOUR, 0), size = 1, colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  coord_fixed(3600) +
  theme(legend.position = "none") +
  ylab("WD") +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 20), axis.title.x = element_blank(), 
        axis.text.y = element_text(colour = "white"), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA))

# windrose

library(clifro)
library(openair)
  
wind_df_WR <- wind_df %>%
  filter(TIME >= "2020-01-01",
         TIME < "2020-01-02") %>%
  mutate(YEAR = lubridate::year(TIME),
         MONTH = lubridate::month(TIME), 
         MONTH_NAME = lubridate::month(TIME, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME),
         DAY_NAME = lubridate::wday(TIME, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME),
         HOUR = lubridate::hour(TIME)
  ) %>%
  glimpse() %>%
  group_by(HOUR) %>%
  summarise(TIME = round_date(mean(TIME, na.rm=TRUE), unit = "hour"),
            WSPD = mean(WSPD, na.rm = TRUE),
            WDIR = (360+(atan2(sum(sin(WDIR*pi/180)),sum(cos(WDIR*pi/180)))*180/pi))%%360
  ) %>%
  ungroup() %>%
  mutate(WIND_SECTOR = case_when( WDIR > 0 & WDIR <= 22.5 ~ "North",
                                  WDIR > 22.5 & WDIR <= 67.5 ~ "North-East",
                                  WDIR > 67.5 & WDIR <= 112.5 ~ "East",
                                  WDIR > 112.5 & WDIR <= 157.5 ~ "South-East",
                                  WDIR > 157.5 & WDIR <= 202.5 ~ "South",
                                  WDIR > 202.5 & WDIR <= 247.5 ~ "South-West",
                                  WDIR > 247.5 & WDIR <= 292.5 ~ "West",
                                  WDIR > 292.5 & WDIR <= 337.5 ~ "North-West",
                                  WDIR > 337.5 & WDIR <= 360 ~ "North")
  ) %>%
  glimpse()

with(wind_df_WR, windrose(speed = WSPD, 
                          direction = WDIR, 
                          speed_cuts = c(2,4,6,8), 
                          ggtheme = "bw"
                          )
     )

windRose(mydata = wind_df_WR, 
         ws = "WSPD",
         wd = "WDIR"
         )
  

# Wind Speed

wind_df %>%
  filter(TIME >= "2020-01-01",
         TIME < "2020-01-02") %>%
  mutate(YEAR = lubridate::year(TIME),
         MONTH = lubridate::month(TIME), 
         MONTH_NAME = lubridate::month(TIME, label = TRUE, abbr = FALSE),
         MONTH_ABBR = lubridate::month(TIME, label = TRUE, abbr = TRUE),
         DAY = lubridate::day(TIME),
         DAY_NAME = lubridate::wday(TIME, label = TRUE, abbr = FALSE),
         DAY_ABBR = lubridate::wday(TIME, label = TRUE, abbr = TRUE),
         WEEKDAY = lubridate::wday(TIME),
         HOUR = lubridate::hour(TIME)
  ) %>%
  glimpse() %>%
  group_by(HOUR) %>%
  summarise(#TIME = round_date(mean(TIME, na.rm=TRUE), unit = "hour"),
    WSPD = mean(WSPD, na.rm = TRUE),
    WDIR = (360+(atan2(sum(sin(WDIR*pi/180)),sum(cos(WDIR*pi/180)))*180/pi))%%360
  ) %>%
  ungroup() %>%
  mutate(WIND_SECTOR = case_when( WDIR > 0 & WDIR <= 22.5 ~ "North",
                                  WDIR > 22.5 & WDIR <= 67.5 ~ "North-East",
                                  WDIR > 67.5 & WDIR <= 112.5 ~ "East",
                                  WDIR > 112.5 & WDIR <= 157.5 ~ "South-East",
                                  WDIR > 157.5 & WDIR <= 202.5 ~ "South",
                                  WDIR > 202.5 & WDIR <= 247.5 ~ "South-West",
                                  WDIR > 247.5 & WDIR <= 292.5 ~ "West",
                                  WDIR > 292.5 & WDIR <= 337.5 ~ "North-West",
                                  WDIR > 337.5 & WDIR <= 360 ~ "North")
  ) %>%
  glimpse() %>%
  ggplot(df, mapping = aes(x = HOUR, y = WSPD)) +
  theme_classic(base_size = 20) + 
#  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  geom_line(aes(x = HOUR, y = WSPD),
            lwd = 1, alpha = 0.2, colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  geom_smooth(aes(x = HOUR, y = WSPD),
              lwd = 2, colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  geom_point(aes(x = HOUR, y = WSPD, 
                 size = 2, 
                 #size = log(WSPD), 
  ), shape = 19, colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  #scale_color_viridis_d() +
  #scale_color_brewer() +
  #scale_colour_brewer(type = "seq", palette = "Spectral") +
  #scale_color_brewer(palette = "Spectral") +
  #scale_color_brewer(palette = "Purples") +
  #scale_fill_distiller() +
  #scale_color_scico_d() +
  theme(legend.position="none") + 
  scale_x_continuous(breaks = seq(0,23,1), labels = c(as.character(unique(lubridate::hour(wind_df$TIME)))) ) +
  ylab("m/s")


# create the plot ----



ggplot(wind_df, mapping = aes(x = TIME, y = WSPD)) +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 20), axis.title.x = element_blank(), 
        axis.text.y = element_text(colour = "white"), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  geom_segment(aes(x = TIME,
                   y = 0,
                   xend = TIME - lubridate::dhours(1 * -cos((90-WDIR) / 360 * 2 * pi)),
                   yend = -1 * (1 * -sin((90-WDIR) / 360 * 2 * pi))
  ),
  arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  geom_point(aes(TIME, 0), size = 1, colour = RColorBrewer::brewer.pal(name = "BuPu", n = 9)[7]) +
  coord_fixed(3600) +
  ylim(-1,1) +
  xlim(df$TIME[1]-3600, df$TIME[nrow(df)]+3600) +
  theme(legend.position = "none") +
  ylab("WD")









# x- CALCULATING Relative Humidity
## Formula.. 
## Using function library: https://cran.r-project.org/web/packages/humidity/humidity.pdf

meteo_df %>% 
  select()
  



