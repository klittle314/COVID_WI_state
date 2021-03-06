library(readr)
library(tidyverse)
library(GGally)
library(gridExtra)
library(data.table)
library(geofacet)
library(lubridate)
library(jsonlite)
source("helper.R")

#set up the file, filepath and data directory
#CHANGE THE AS CHARACTER DATE STRING
#data_file_stateWI <- paste0('data/WI_state_data_', "2020-08-08", '.csv')

data_file_stateWI <- paste0('data/WI_state_data_', Sys.Date(), '.csv')

#data_csv <- "https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.csv"

#November 30:  found the file paths DHS is now using 
state_url <- "https://opendata.arcgis.com/datasets/859602b5d427456f821de3830f89301c_11.geojson"

county_url <- "https://opendata.arcgis.com/datasets/5374188992374b318d3e2305216ee413_12.geojson"

ifelse(!dir.exists('data'), dir.create('data'), FALSE)

if(!file.exists(data_file_stateWI)) {
  #data1 <- try(fread(data_csv))
  data1 <- try(JSON_fread(state_url,county_url))
  write_csv(data1, path=data_file_stateWI)
}

#if(file.exists(data_file_stateWI)) {
  #GEOID is a state, county and census tract code
  #GEOID: 55=State of Wisconsin; 55001:55141 for counties (odd numbers only)                                                
  #census tract ids are then of the form 55xxxyyyyyy
  #Missing census tract id will be stated as TRACT N/A so GEOID is assigned as character type for now
  #df_in <- read_csv(data_file_stateWI, col_types=cols(.default = 'i',
                                               #     GEOID = 'c',
                                               #     GEO = 'c',
                                               #     NAME = 'c',
                                               #     DATE = 'c'))
  #date time format of the file changed on 6 May 2020
  #records have form "2020/03/15 19:00:00+00".  So strip off +00 and convert to date-time object
  #df_in$Date_time <- as.POSIXct(gsub("\\+00","",df_in$DATE))
  #df_in$Date_reported <- as.Date(df_in$Date_time, tz = "US/Central")
  
#}

if(file.exists(data_file_stateWI)) {
  df_in <- read_csv(data_file_stateWI)
  #df_in$Date_time <- as.POSIXct(gsub("\\+00","",df_in$DATE))
  #df_in$Date_reported <- as.Date(df_in$Date_time, tz = "US/Central")
}

#pull state and county records
df1 <- df_in %>% filter(GEO %in% c('State','County'))

#distinguish baseline from most recent 14 days
cut_date <- max(df1$Date_reported) - 14

df1$phase <- ifelse(df1$Date_reported <= cut_date,"baseline","last 14 days")

df1_small <- df1 %>% select(GEOID,GEO,NAME,Date_reported,NEGATIVE,POSITIVE)

# non monotonicity now handled by the function make_vec
# #Error in Negative Series for State
# df1_small$NEGATIVE[df1_small$Date_reported == as.Date("2020-03-29") & df1_small$GEO == "State"] <- 15856
# df1_small$NEGATIVE[df1_small$Date_reported == as.Date("2020-03-30") & df1_small$GEO == "State"] <- 16550
# 
# #Error in Positive Series for Dane
# df1_small$POSITIVE[df1_small$Date_reported == as.Date("2020-04-16") & df1_small$NAME == "Dane"] <- 351
# df1_small$POSITIVE[df1_small$Date_reported == as.Date("2020-04-17") & df1_small$NAME == "Dane"] <- 352

# #now extract only the overall testing numbers and create daily counts
# df1_small <- df1_small %>%
#   group_by(NAME) %>% 
#   mutate(POSITIVE_daily = make_vec(POSITIVE)) %>%
#   mutate(NEGATIVE_daily = make_vec(NEGATIVE)) %>%
#   #issue is that counties early in series do not have negative tests
#   mutate(Total_daily_tests = POSITIVE_daily + NEGATIVE_daily) %>%
#   mutate(POS_pct_daily = 100*(POSITIVE_daily/Total_daily_tests)) 

#extract only the overall testing numbers and create daily counts forcing monotonicity, Steve G use of cummin
df1_small <- df1_small %>%
  group_by(NAME) %>%
  arrange(desc(Date_reported)) %>%
  mutate(NEGATIVE = cummin(NEGATIVE)) %>%
  mutate(POSITIVE = cummin(POSITIVE)) %>%
  arrange(Date_reported) %>% 
  mutate(POSITIVE_daily = make_vec1(POSITIVE)) %>%
  mutate(NEGATIVE_daily = make_vec1(NEGATIVE)) %>%
  #issue is that counties early in series do not have negative tests
  mutate(Total_daily_tests = POSITIVE_daily + NEGATIVE_daily) %>%
  mutate(POS_pct_daily = if_else(Total_daily_tests > 0,100*(POSITIVE_daily/Total_daily_tests),as.numeric(NA)))  

#create a 14 day index for use in slope calculations
daycode0 <- seq.int(from = 0, to = 13, by = 1)


#minimum number of positive tests used to determine whether or not to make the slope chart
min_n_pos_tests_slope_chart <- 100

#minimum records for control charts used to determine whether or not attempt to make control chart
min_n_control_charts <- 10

#minimum number of positive tests used to determine whether or not attempt to make control chart
min_n_pos_tests <- 100

#maximum number of daily positive tests to indicate a c-chart for small counts
POS_daily_max_limit <- 10

#cut point for reference of 'small' counts
Date_limit_small_counts <- today()#as.Date("2020-05-11")

#name list of counties with small counts
df1_small_gate1 <- df1_small %>% filter(Date_reported <= Date_limit_small_counts) %>%
                      group_by(NAME) %>% mutate(max_POS_Daily = max(POSITIVE_daily, na.rm=TRUE)) %>%
                      filter(max_POS_Daily <= POS_daily_max_limit)

counties_small_counts <- unique(df1_small_gate1$NAME)
    
#geofacet grid
mygrid <- data.frame(
  name = c("Douglas County", "Bayfield County", "Washburn County", "Sawyer County", "Ashland County", "Iron County", "Oneida County", "Vilas County", "Polk County", "Burnett County", "Barron County", "Chippewa County", "Rusk County", "Price County", "Lincoln County", "Langlade County", "Forest County", "Florence County", "Marinette County", "St. Croix County", "Dunn County", "Eau Claire County", "Clark County", "Taylor County", "Marathon County", "Shawano County", "Menominee County", "Oconto County", "Calumet County", "Door County", "Pierce County", "Pepin County", "Trempealeau County", "Jackson County", "Wood County", "Portage County", "Waushara County", "Waupaca County", "Outagamie County", "Brown County", "Kewaunee County", "Buffalo County", "La Crosse County", "Monroe County", "Juneau County", "Adams County", "Green Lake County", "Winnebago County", "Washington County", "Sheboygan County", "Manitowoc County", "Vernon County", "Richland County", "Columbia County", "Marquette County", "Dodge County", "Fond du Lac County", "Ozaukee County", "Crawford County", "Iowa County", "Sauk County", "Dane County", "Jefferson County", "Waukesha County", "Milwaukee County", "Grant County", "Lafayette County", "Green County", "Rock County", "Walworth County", "Kenosha County", "Racine County"),
  code = c("douglas", "bayfield", "washburn", "sawyer", "ashland", "iron", "oneida", "vilas", "polk", "burnett", "barron", "chippewa", "rusk", "price", "lincoln", "langlade", "forest", "florence", "marinette", "st croix", "dunn", "eau claire", "clark", "taylor", "marathon", "shawano", "menominee", "oconto", "calumet", "door", "pierce", "pepin", "trempealeau", "jackson", "wood", "portage", "waushara", "waupaca", "outagamie", "brown", "kewaunee", "buffalo", "la crosse", "monroe", "juneau", "adams", "green lake", "winnebago", "washington", "sheboygan", "manitowoc", "vernon", "richland", "columbia", "marquette", "dodge", "fond du lac", "ozaukee", "crawford", "iowa", "sauk", "dane", "jefferson", "waukesha", "milwaukee", "grant", "lafayette", "green", "rock", "walworth", "kenosha", "racine"),
  row = c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9),
  col = c(3, 4, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 5, 6, 7, 8, 9, 10, 11, 5, 6, 7, 8, 9, 10, 11, 5, 6, 7, 8, 9, 10, 11),
  stringsAsFactors = FALSE
)

mygrid1 <- mygrid

mygrid1$name <- gsub(" County","",mygrid1$name)

# grid_design()
#geofacet::grid_preview(mygrid)
# geofacet::grid_submit(mygrid)

