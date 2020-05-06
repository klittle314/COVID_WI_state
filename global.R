library(readr)
library(tidyverse)
library(GGally)
library(gridExtra)
library(data.table)
source("helper.R")

#set up the file, filepath and data directory
data_file_stateWI <- paste0('data/WI_state_data_', as.character(Sys.Date()), '.csv')

data_csv <- "https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.csv"

ifelse(!dir.exists('data'), dir.create('data'), FALSE)

if(!file.exists(data_file_stateWI)) {
  data1 <- try(fread(data_csv))
  write_csv(data1, path=data_file_stateWI)
}

if(file.exists(data_file_stateWI)) {
  #GEOID is a state, county and census tract code
  #GEOID: 55=State of Wisconsin; 55001:55141 for counties (odd numbers only)                                                
  #census tract ids are then of the form 55xxxyyyyyy
  #Missing census tract id will be stated as TRACT N/A so GEOID is assigned as character type for now
  df0 <- read_csv(data_file_stateWI, col_types=cols(.default = 'i',
                                                    GEOID = 'c',
                                                    GEO = 'c',
                                                    NAME = 'c',
                                                    LoadDttm = 'd'))
  
}
#pull state and county records
df1 <- df0 %>% filter(GEO %in% c('State','County'))

#time records trashed on conversion to CSV
df1$Date_Time <- df1$LoadDttm/1000

class(df1$Date_Time) <- c('POSIXt','POSIXct')

df1$Date_reported <- as.Date(df1$Date_Time)

#distinguish baseline from most recent 14 days
cut_date <- max(df1$Date_reported) - 14

df1$phase <- ifelse(df1$Date_reported <= cut_date,"baseline","last 14 days")

df1_small <- df1 %>% select(GEOID,GEO,NAME,Date_reported,NEGATIVE,POSITIVE) 
df1_small$NEGATIVE[df1_small$Date_reported == as.Date("2020-03-29") & df1_small$GEO == "State"] <- 15856
df1_small$NEGATIVE[df1_small$Date_reported == as.Date("2020-03-30") & df1_small$GEO == "State"] <- 16550

#now extract only the overall testing numbers and create daily counts
df1_small <- df1_small %>%
  group_by(NAME) %>% 
  mutate(lag_POSITIVE = lag(POSITIVE)) %>% mutate(POSITIVE_daily = POSITIVE - lag_POSITIVE) %>%
  mutate(lag_NEGATIVE = lag(NEGATIVE)) %>% mutate(NEGATIVE_daily = NEGATIVE - lag_NEGATIVE) %>%
  #issue is that counties early in series do not have negative tests
  mutate(Total_daily_tests = POSITIVE_daily + NEGATIVE_daily) %>%
  mutate(POS_pct_daily = 100*(POSITIVE_daily/Total_daily_tests)) 

