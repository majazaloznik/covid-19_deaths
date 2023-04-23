###############################################################################
###############################################################################
#' Script for original import and weekly update of three files:
#' daily_deaths_slovenia_by_age_sex_and_admin_area.csv
#' daily_deaths_slovenia_by_age.csv
#' daily_deaths_slovenia.csv
#' 
#' The data source is a csv file extract from the Central Population Registry in 
#' the /data folder. Weekly updates include historical changes and are sometimes
#' available for the whole period, sometimes less. If the files are not already
#' there, the script creates them, otherwise they get updated. The update 
#' overwrites all existing data without warning, 
#' 
###############################################################################
###############################################################################

###############################################################################
# preliminaries
###############################################################################
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 

###############################################################################
## UPDATE  for csv and API BY AGE AND ADMINISTRATIVE AREA
###############################################################################
# check if this is the first time running the script or just updating the data. 
update <- file.exists("../../data/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")

# read new data CHANGE NAME and last date!!!
x <- read_csv("data/SLEDILNIK-COVID_19(2.4.2023)/UMRLI po DNEVIH-STAROSTNIH SKUPINAH-SPOLU--(od-do)-VIRŠČEK-po UE VPISA(1.1.2010-2.4.2023).csv")
max.date <- "2023-03-20"

# read administrative area dictionary
dict <- read_csv("../../data/csv/dict-administrative_areas.csv")
dict %>% 
  group_by(ue.id) %>% 
  summarise(ue.id = first(ue.id), ue.name = first(ue.name), 
            region.code = first(region.code),
            region.name = first(region.name)) -> dict

# rename, recode and cut last week off
x %>%
  select(date = "Datum smrti",
         sex = "Spol",
         age = "Starostna skupina",
         deceased = "Starostna skupina COUNT",
         ue = "Upravna enota") %>%
  filter(! is.na(date),
         deceased != 0) %>%
  mutate(age = substring(age, 1,1),
         date = as.Date(date, "%d/%m/%Y"),
         age.group = recode(age, `1` = "0-3",
                       `2` = "4-18",
                       `3` = "19-31",
                       `4` = "32-41",
                       `5` = "42-51",
                       `6` = "52-61",
                       `7` = "62-71",
                       `8` = "72+",
                       .default = "na")) %>%
  filter( date < max.date) -> temp.df

min.date <- min(temp.df$date)

temp.df %>% 
  mutate(sex = recode(sex, "M" = "deceased.age.male", 
                      "Ž" = "deceased.age.female")) %>% 
  unite(group, sex, age.group, sep = ".", remove = FALSE) -> df

# get correct column order
df %>%
  group_by(sex, age, group) %>%
  summarise(n = n()) %>%
  pull(group) -> col.order

# shape and clean into long table format and add regions from dictionary
temp.df %>% 
  mutate(sex = recode(sex, "M" = "M",  "Ž" = "F")) %>% 
  select(date, ue, sex, age.group, deceased) %>% 
  left_join(dict, by = c("ue" = "ue.name")) %>% 
  relocate(region.code, .after=date) %>% 
  relocate(region.name, .after=region.code) %>% 
  relocate(ue.id, .after=region.name) %>% 
  rename(ue.name = ue) %>% 
  arrange(date, ue.id, sex, age.group) -> long.df

# collapse areas
df %>%
  mutate(group = factor(group, levels = col.order, ordered = TRUE)) %>%
  group_by(date, group) %>%
  summarise(deceased = sum(deceased))  %>%
  spread(group, deceased) -> age.df

# collapse areas and ages
df %>%
  mutate(group = factor(group, levels = col.order, ordered = TRUE)) %>%
  group_by(date) %>%
  summarise(deceased = sum(deceased))  -> collapsed.df

# append/overwrite existing complete long file
###############################################################################
if (update) {
  old <- read_csv("../../data/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")
  old %>% 
    filter(date < min.date) %>% 
    bind_rows(long.df) -> long.df}

# export full long dataset
write_csv(long.df, "../../data/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv", 
          na = "")
write.table(round(as.numeric(Sys.time()), 0),
            "../../data/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

# append/overwrite existing collapsed file 
###############################################################################

if (update) {
  old <- read_csv("../../data/csv/daily_deaths_slovenia.csv")
  old %>% 
    filter(date < min.date ) %>% 
    bind_rows(collapsed.df) -> collapsed.df}

# export aggregated daily totals
write_csv(collapsed.df, "../../data/csv/daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0),
            "../../data/csv/daily_deaths_slovenia.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

# append/overwrite existing file by age
###############################################################################
if (update) {
  old <- read_csv("../../data/csv/daily_deaths_slovenia_by_age.csv", 
                  col_types = c(date = "D", .default = "n"))
  old %>% 
    filter(date < min.date) %>% 
    bind_rows(age.df) -> age.df}

# export aggregated daily totals by age
write_csv(age.df, "../../data/csv/daily_deaths_slovenia_by_age.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "../../data/csv/daily_deaths_slovenia_by_age.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

