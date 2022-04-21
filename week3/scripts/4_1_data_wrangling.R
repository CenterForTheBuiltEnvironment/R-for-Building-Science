# Apr 18 2022
# Federico Dallo

library(tidyverse)
library(lubridate)

# Historical Data San Francisco: https://www.ndbc.noaa.gov/station_history.php?station=ftpc1
# Location: https://www.ndbc.noaa.gov/station_page.php?station=ftpc1

# rm(list = ls())

# Prepare Dataset

### data_dir <- here("week3", "data", "tmp")
### setwd(data_dir)
### file_list <- Sys.glob("ftpc1h****.txt")
### output_recombined_file <- file.create("recombined_ftpc1h.txt")
### output_recombined_file <- file("recombined_ftpc1h.txt", "r+")
### for (i in file_list) {
###   input <- readLines(i)
###   input <- input[-c(1,2)] # delete 1st line of the header
###   #input <- input[-c(1)] # delete 2nd line of the header
###   writeLines(input, output_recombined_file)
### }
### close(output_recombined_file)
### 
### # use Vim to add header
### # use Vim to add commas beteween colums
### # use Vim to remove "#"

# Save header 

df_SF_names <- read_csv("df_SanFran2011-2020.txt", n_max = 0) %>% names()
df_SF_units <- read_csv("df_SanFran2011-2020.txt", n_max = 1)
df_SF_data <- read_csv("df_SanFran2011-2020.txt", col_names = df_SF_names, skip = 2)
head(df_SF_data)

# add column with date-time character
df_SF_data$TIME <- paste0(df_SF_data$YY, "-", df_SF_data$MM, "-", df_SF_data$DD, " ", df_SF_data$hh, ":", df_SF_data$mm)
head(df_SF_data)

# Select variable of interests: check metadata at https://www.ndbc.noaa.gov/measdes.shtml

tmp_df_SF <- df_SF_data %>%
  select(TIME, WDIR, WSPD, PRES, ATMP) %>%
  glimpse()

tmp_df_SF$TIME <- lubridate::ymd_hm(tmp_df_SF$TIME, tz = "UTC")

# quick check
summary(tmp_df_SF) # there are FLAGS

tmp_df_SF %>%
  #select(TIME, ATMP) %>%
  filter(TIME >= "2020-01-01 00:00",
         TIME < "2020-01-02 00:00",
         ATMP <= 50,
         ATMP >= -20) %>%
  ggplot(data = ., aes(x = TIME, y = ATMP)) + 
  geom_point() +
  theme_classic()

# make data as character :) to increase difficulty

tmp_df_SF <- tmp_df_SF %>%
  mutate(TIME = as.character(TIME)) %>%
  glimpse()
  
saveRDS(object = tmp_df_SF, 
        file = "week3/data/tmp/meteo_SF_FTPC1_2011-2020.rds", 
        compress = "gzip")

# file.copy(from = "week3/data/tmp/meteo_SF_FTPC1_2011-2020.rds", 
#           to = "week3/data/meteo_SF_FTPC1_2011-2020.rds", overwrite = TRUE)

















