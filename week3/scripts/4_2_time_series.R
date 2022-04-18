# Apr 18 2022
# F. Dallo

library(tidyverse)
library(lubridate)
library(here)

here::i_am("ARCH 249 Module 3.Rproj")

meteo_df <- readRDS(file = "")

meteo_01M %>%
  select(dttyDatetime, mean_T_C) %>%
  #filter(mean_T_C > -20) %>%
  ##FEDE## create daily average data:
  mutate(YEAR = lubridate::year(dttyDatetime),
         MONTH = lubridate::month(dttyDatetime),
         DAY = lubridate::day(dttyDatetime)
  ) %>%
  group_by(YEAR, MONTH, DAY) %>%
  summarise(DATE = lubridate::floor_date(x = mean(dttyDatetime, na.rm=TRUE), unit = "day"),
            TEMPERATURE = mean(mean_T_C, na.rm = T)
  ) %>%
  ungroup() %>%
  select(DATE, TEMPERATURE) %>%
  #nrow()
  ##FEDE## plot:
  ggplot(data = ., aes(x = DATE, y = TEMPERATURE)) +
  geom_line() + 
  geom_point()

