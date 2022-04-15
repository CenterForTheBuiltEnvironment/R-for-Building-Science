# March 3 2022
# Federico Dallo

library(tidyverse)
library(lubridate)
library(tsibble)
library(tsibbledata)
library(rvest)

# Following Forecasting: Principles and Practice book
# https://otexts.com/fpp3/tsibbles.html

# --- tsibble --- #

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

# --- from tibble to tsibble ---#
z <- tibble(
  Month = c("2019 Jan", "2019 Feb", "2019 Mar", "2019 Apr", "2019 May"),
  Observation = c(50 ,23, 34, 30, 25)
)

z %>% 
  mutate(Month = yearmonth(Month)) %>%
  as_tibble(index = Month)

olympic_running %>% distinct(Sex)

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6) -> a10 


# --- get Col Margherita Obs. Data ---#
## -- load multiple data file into a single file

file_meteo <- Sys.glob(paste0(here::here("week3", "data", "MRG_2021"), "/MRG_**********_METEO_01M.txt"))
targetFile <- paste0(here::here("week3", "data", "MRG_2021"), "/recombined_METEO_01M.txt")
output_file_meteo <- file.create( targetFile )
output_file_meteo <- file( targetFile , "r+")
for (i in file_meteo) {
  input <- readLines(i)
  input <- input[-c(1)] # delete header
  writeLines(input, output_file_meteo)
}
close(output_file_meteo)

## --  load data in memory and description and data types

meteo_01M <- read.csv(file = targetFile , header = FALSE, sep = " ")

names(meteo_01M) <- c("datetime", 
                      "mean_WD_deg", "sd_WD_deg", "mean_WS_ms", "sd_WS_ms", 
                      "mean_T_C", "sd_T_C", 
                      "mean_RH_perc", "sd_RH_perc", 
                      "mean_P_hPa", "sd_P_hPa", 
                      "mean_WC_C", "sd_WC_C",
                      "nr_obs" # the observation collected for specific datetime
)
head(meteo_01M) # check the file
str(meteo_01M) # get more info on the structure of the file
glimpse(meteo_01M) # alternative to str()

## -- convert into tsibble --##

meteo_01M <- meteo_01M %>%
  mutate(dttyDatetime = lubridate::ymd_hms(datetime),
         Quarter = yearquarter(datetime))


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



















