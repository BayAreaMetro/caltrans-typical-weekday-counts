#
## Administration
#
#### Purpose
# This script consumes Caltrans traffic count files.
# The data is melted into a simpler database format and outlier counts are identified and removed. 
#
#### Outputs
#  A consolidated database of clean Caltrans counts for typical weekdays
#
#### TODO
#  Move a step back in the sequence?
#


## Procedure

#### Overhead
library(reshape2)
suppressMessages(library(dplyr))
library(timeDate)
library(chron)

#### Start timer
start_time <- proc.time()

#### Remote IO Locations
F_INPUT_DIR = "M:/Data/Traffic/Caltrans/Processed Data"
INPUT_FILE  = "counts_1998-2020.csv"

F_OUTPUT_DIR = file.path(Sys.getenv("HOME"),"..","Box","Modeling and Surveys","Share Data","caltrans-typical-weekday")
OUTPUT_FILE  = "typical-weekday-counts.csv"


#### Parameters
# Relevant holidays database
HOLIDAY_LIST  <- c("USLaborDay", "USMemorialDay", "USThanksgivingDay", "USVeteransDay")
HOLIDAY_DATES <- dates(as.character(holiday(1998:2020, HOLIDAY_LIST)), format = "Y-M-D")

# Minimum number of days observed for estimates to be retained
MIN_DAYS_OBSERVED = 15


#### Data reads
input_file_name <- file.path(F_INPUT_DIR, INPUT_FILE)
input_df <- read.table(file = input_file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE, strip.white = TRUE,
                       colClasses=c("ROUTE"="character"))

#### Data cleaning
# select the variables of interest
clean_df <- input_df %>%
  select(route = ROUTE, 
         county = COUNTY, 
         post_mile = PM, 
         leg = LEG, 
         direction = DIR, 
         station = STATION,
         description = DESCRIP, 
         date_string = date,
         END1AM, END2AM, END3AM, END4AM, END5AM, END6AM, END7AM, END8AM, END9AM, END10AM, END11AM, END12NOO,
         END1PM, END2PM, END3PM, END4PM, END5PM, END6PM, END7PM, END8PM, END9PM, END10PM, END11PM, END12NIG)

# Melt
clean_df <- melt(clean_df, id = c("route", "county", "post_mile", "leg", 
                                  "direction", "station", "description", "date_string"))

# Tidy-up the melt
clean_df <- clean_df %>%
  rename(count = value) %>%
  mutate(integer_hour = 
           (variable == 'END1AM')   * 1 +
           (variable == 'END2AM')   * 2 +
           (variable == 'END3AM')   * 3 +
           (variable == 'END4AM')   * 4 +
           (variable == 'END5AM')   * 5 +
           (variable == 'END6AM')   * 6 +
           (variable == 'END7AM')   * 7 +
           (variable == 'END8AM')   * 8 +
           (variable == 'END9AM')   * 9 +
           (variable == 'END10AM')  * 10 +
           (variable == 'END11AM')  * 11 +
           (variable == 'END12NOO') * 12 +
           (variable == 'END1PM')   * 13 +
           (variable == 'END2PM')   * 14 +
           (variable == 'END3PM')   * 15 +
           (variable == 'END4PM')   * 16 +
           (variable == 'END5PM')   * 17 +
           (variable == 'END6PM')   * 18 +
           (variable == 'END7PM')   * 19 +
           (variable == 'END8PM')   * 20 +
           (variable == 'END9PM')   * 21 +
           (variable == 'END10PM')  * 22 +
           (variable == 'END11PM')  * 23 +
           (variable == 'ENDNIG')   * 24) %>%
  select(-variable)

# Create typical weekday flag
clean_df <- clean_df %>%
  mutate(date = as.Date(date_string)) %>%
  mutate(day_of_week = weekdays(date)) %>%
  mutate(typical_day_of_week = (day_of_week == "Tuesday") * 1 + 
           (day_of_week == "Wednesday") * 1 + 
           (day_of_week == "Thursday") * 1) %>%
  mutate(month = months(date)) %>%
  mutate(typical_month = (month == "March") * 1 +
           (month == "April") * 1 +
           (month == "May") * 1 +
           (month == "September") * 1 +
           (month == "October") * 1 +
           (month == "November") * 1) %>%
  mutate(year = years(date)) %>%
  mutate(not_holiday = !(is.holiday(date, HOLIDAY_DATES))) %>%
  mutate(typical_weekday = typical_day_of_week * typical_month * not_holiday) %>%
  select(-day_of_week, -typical_day_of_week, -month, -typical_month, -not_holiday)

# We only care about records for typical weekdays an non-NA
typical_df <- clean_df %>%
  filter(typical_weekday == 1, !is.na(count))

# Typical weekday summaries
typical_sum_df <- typical_df %>%
  group_by(route, county, post_mile, leg, direction, station, description, integer_hour) %>%
  summarise(median_count = median(count), avg_count = mean(count), sd_count = sd(count), days_observed = n())

# Join typical weekday stats to each record
typical_df <- left_join(typical_df, typical_sum_df, by = c("route", "county", "post_mile", "leg", "direction", "station", "description", "integer_hour"))

# Flag suspect counts (use four standard deviations across years)
typical_df <- typical_df %>%
  mutate(four_sd_low  = avg_count - 4 * sd_count) %>%
  mutate(four_sd_high = avg_count + 4 * sd_count) %>%
  mutate(suspect_flag = (count < four_sd_low) + (count > four_sd_high))

table(typical_df$suspect_flag)

# Identify and account for duplicate records
duplicate_df <- typical_df %>%
  group_by(route, county, post_mile, leg, direction, station, date, integer_hour) %>%
  summarise(duplicate = n())

table(duplicate_df$duplicate)

typical_df <- left_join(typical_df, duplicate_df, by = c("route", "county", "post_mile", "leg", "direction", "station", "date", "integer_hour"))

typical_df <- typical_df %>%
  mutate(count = count / duplicate) %>%
  mutate(record_weight = 1 / duplicate)

# Remove suspects and small sample size, redo summaries
typical_sum_df <- typical_df %>%
  filter(suspect_flag == 0) %>%
  group_by(route, county, post_mile, leg, direction, station, description, year, integer_hour) %>%
  summarise(median_count = median(count), avg_count = mean(count), 
            sd_count = sd(count), days_observed = sum(record_weight)) %>%
  filter(days_observed > MIN_DAYS_OBSERVED)

#### Write to disk
output_file_name <- file.path(F_OUTPUT_DIR, OUTPUT_FILE)
write.csv(typical_sum_df, file = output_file_name, row.names = FALSE, quote = T)


#### End timer
run_time <- proc.time() - start_time
run_time


