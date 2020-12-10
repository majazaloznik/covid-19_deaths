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
## update  for API
###############################################################################
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


# 
# # 
# daily <- read_csv("data/daily_deaths_slovenia.csv")
# csv %>%
#   mutate_if(is.character, function(x) as.numeric(x)) %>%
#   mutate(total = rowSums(.[2:17], na.rm = TRUE)) %>%
#   left_join(daily) %>%
#   filter(total != deceased) -> check
# write_csv(check, "data/age_daily_deaths_diff.csv", na = "")
# 
#   csv %>%
#     mutate_if(is.character, function(x) as.numeric(x)) %>%
#     summarise_if(is.numeric,  ~ sum(., na.rm = TRUE))
# 
# # 
#   daily %>% 
#     filter(date > "2019-12-31") %>% 
#     summarise(sum = sum(deceased))
#   