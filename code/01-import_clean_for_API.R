###############################################################################
# preliminaries
###############################################################################
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 

###############################################################################
## clean  for API
##############################################################################

# x <- read_excel("data/št. UMRLI_2010_2020_1.xls")
# 
# x %>%
#   filter(! is.na(Dan)) %>%
#   rename(date = Dan, deceased = "št. na Dan") %>%
#   select(date, deceased) %>%
#   mutate(date = as.Date(date, "Y%-m%-d%")) %>%
#   filter(date < "2020-11-23") -> csv
# 
# write_csv(csv, "data/daily_deaths_slovenia.csv")
# 
# write.table(round(as.numeric(Sys.time()), 0), "data/daily_deaths_slovenia.timestamp",
#             row.names = FALSE, col.names = FALSE)
#

## update for API
##############################################################################

# update filename
new <- read_excel("data/št. UMRLI po mesecih-dnevih-letih-(1.1.2020-7.12.2020).xls")

new %>%
  filter(! is.na(Dan)) %>%
  rename(date = Dan, deceased = "št. na Dan") %>%
  select(date, deceased) %>%
  mutate(date = as.Date(date, "Y%-m%-d%")) %>%
  filter(date < "2020-11-30") %>% 
  mutate(deceased = as.numeric(deceased))-> new.csv

old <- read_csv("data/daily_deaths_slovenia.csv")

old %>% 
  filter(date < "2020-01-01") %>% 
  bind_rows(new.csv) -> update

write_csv(update, "data/daily_deaths_slovenia.csv")

write.table(round(as.numeric(Sys.time()), 0), "data/daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)


###############################################################################
## clean  for API BY AGE
###############################################################################
# x <- read_excel("data/St. umrlih po dnevu, spolu in starostnih skupinah 1.2010 do 11.2020 MNZ.xlsx")
# 
# x %>%
#   select(date = "Datum smrti", 
#          sex = "Spol",
#          age = "Starostna skupina",
#          deceased = "Starostna skupina COUNT") %>%
#   filter(! is.na(date),
#          deceased != 0) %>% 
#   mutate(age = substring(age, 1,1),
#          date = as.Date(date, "Y%-m%-d%"),
#          sex = recode(sex, "M" = "deceased.age.male",  "Ž" = "deceased.age.female"),
#          ageg = recode(age, `1` = "0-3",
#                       `2` = "4-18",
#                       `3` = "19-31",
#                       `4` = "32-41",
#                       `5` = "42-51",
#                       `6` = "52-61",
#                       `7` = "62-71",
#                       `8` = "72+",
#                       .default = "na")) %>% 
#   unite(group, sex, ageg, sep = ".", remove = FALSE) -> df
# 
# # get correct column order
# df %>% 
#   group_by(sex, age, group) %>% 
#   summarise(n = n()) %>% 
#   pull(group) -> col.order
# 
# noquote(col.order)
# df %>% 
#   select(date, group, deceased) %>% 
#   spread(group, deceased) %>% 
#   select(date, col.order) %>% 
#   filter(date < "2020-11-23") -> csv
# 
# 
# write_csv(csv, "data/age_daily_deaths_slovenia.csv", na = "")
# write.table(round(as.numeric(Sys.time()), 0), "data/age_daily_deaths_slovenia.timestamp",
#             row.names = FALSE, col.names = FALSE)

###############################################################################
## update  for API b AGE
###############################################################################

# u[date file name
x <- read_excel("data/UMRLI po DNEVIH-STAROSTNIH SKUPINAH-SPOLU(1.1.2020-7.12.2020).xls")

x %>%
  select(date = "Datum smrti", 
         sex = "Spol",
         age = "Starostna skupina",
         deceased = "Starostna skupina COUNT") %>%
  filter(! is.na(date),
         deceased != 0) %>% 
  mutate(age = substring(age, 1,1),
         date = as.Date(date, "Y%-m%-d%"),
         sex = recode(sex, "M" = "deceased.age.male",  "Ž" = "deceased.age.female"),
         ageg = recode(age, `1` = "0-3",
                       `2` = "4-18",
                       `3` = "19-31",
                       `4` = "32-41",
                       `5` = "42-51",
                       `6` = "52-61",
                       `7` = "62-71",
                       `8` = "72+",
                       .default = "na")) %>% 
  unite(group, sex, ageg, sep = ".", remove = FALSE) -> df

# get correct column order
df %>% 
  group_by(sex, age, group) %>% 
  summarise(n = n()) %>% 
  pull(group) -> col.order

noquote(col.order)
df %>% 
  select(date, group, deceased) %>% 
  spread(group, deceased) %>% 
  select(date, col.order) %>% 
  filter(date < "2020-11-30") %>% 
  mutate_if(is.character, as.numeric) -> new.csv


old <- read_csv("data/age_daily_deaths_slovenia.csv", col_types = c(date = "D", .default = "n"))

old %>% 
  filter(date < "2020-01-01") %>% 
  bind_rows(new.csv) -> update

write_csv(update, "data/age_daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "data/age_daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)




