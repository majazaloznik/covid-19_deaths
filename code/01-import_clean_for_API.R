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
# 
# # update filename
# # new <- read_excel("data/št. UMRLI po mesecih-dnevih-letih-(1.1.2020-21.12.2020).xls")
# new <- read_csv("data/št. UMRLI po mesecih-dnevih-letih-(brez UE)-VIRŠČEK-(1.1.2020-21.12.2020).csv")
# 
# 
# # adjust date in filter to cut off last week or so where data is incomplete
# new %>%
#   filter(! is.na(Dan)) %>%
#   rename(date = Dan, deceased = "št. na Dan") %>%
#   select(date, deceased) %>%
#   mutate(date = as.Date(date, "%d/%m/%Y")) %>%
#   filter(date < "2020-12-14") %>% 
#   mutate(deceased = as.numeric(deceased))-> new.csv
# 
# old <- read_csv("data/daily_deaths_slovenia.csv")
# 
# old %>% 
#   filter(date < "2020-01-01") %>% 
#   bind_rows(new.csv) -> update
# 
# write_csv(update, "outputs/daily_deaths_slovenia.csv")
# 
# write.table(round(as.numeric(Sys.time()), 0), "outputs/daily_deaths_slovenia.timestamp",
#             row.names = FALSE, col.names = FALSE)


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
## update  for API by AGE
###############################################################################

# # update file name
# x <- read_excel("data/UMRLI po DNEVIH-STAROSTNIH SKUPINAH-SPOLU(1.1.2020-14.12.2020).xls")
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
#                        `2` = "4-18",
#                        `3` = "19-31",
#                        `4` = "32-41",
#                        `5` = "42-51",
#                        `6` = "52-61",
#                        `7` = "62-71",
#                        `8` = "72+",
#                        .default = "na")) %>% 
#   unite(group, sex, ageg, sep = ".", remove = FALSE) -> df
# 
# # get correct column order
# df %>% 
#   group_by(sex, age, group) %>% 
#   summarise(n = n()) %>% 
#   pull(group) -> col.order
# 
# df %>% 
#   select(date, group, deceased) %>% 
#   spread(group, deceased) %>% 
#   select(date, col.order) %>% 
#   filter(date < "2020-12-06") %>% 
#   mutate_if(is.character, as.numeric) -> new.csv
# 
# 
# old <- read_csv("data/age_daily_deaths_slovenia.csv", col_types = c(date = "D", .default = "n"))
# 
# old %>% 
#   filter(date < "2020-01-01") %>% 
#   bind_rows(new.csv) -> update
# 
# write_csv(update, "data/age_daily_deaths_slovenia.csv", na = "")
# write.table(round(as.numeric(Sys.time()), 0), "data/age_daily_deaths_slovenia.timestamp",
#             row.names = FALSE, col.names = FALSE)


# ###############################################################################
# ## clean  for API BY AGE AND ADMINISTRATIVE AREA
# ###############################################################################
x <- read_csv("data/UMRLI po DNEVIH-STAROSTNIH SKUPINAH-SPOLU--(1.1.2010-16.12.2020)po UE.csv")

# rename, recode and cut last week
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
  filter( date < "2020-12-14") %>%
  unite(group, sex, ageg, sep = ".", remove = FALSE) -> df

# get correct column order
df %>%
  group_by(sex, age, group) %>%
  summarise(n = n()) %>%
  pull(group) -> col.order

# shape and clean into long table format
df %>%
  select(date, group, ue, deceased) %>%
  mutate(group = factor(group, levels = col.order, ordered = TRUE)) %>%
  arrange(date, group) -> long.df

# collapse ages
long.df %>%
  group_by(date, ue) %>%
  summarise(deceased = sum(deceased))  %>%
  spread(ue, deceased) -> ue.df

# collapse areas
long.df %>%
  group_by(date, group) %>%
  summarise(deceased = sum(deceased))  %>%
  spread(group, deceased) -> age.df

# collapse areas and ages
long.df %>%
  group_by(date) %>%
  summarise(deceased = sum(deceased))  -> collapsed.df

# group into weeks
ue.df %>%
  mutate(week = isoweek(date),
         year = year(date)) %>%
  group_by(year, week) %>%
  summarise_at(vars(Ajdovščina:Žalec), ~sum(.x, na.rm = TRUE)) -> ue.weekly

# group into months (with no of days)
ue.df %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise_at(vars(Ajdovščina:Žalec), ~sum(.x, na.rm = TRUE)) %>%
  left_join(ue.df %>%
              mutate(month = month(date),
                     year = year(date)) %>%
              group_by(year, month) %>%
              summarise(days = n())) -> ue.monthly


# export aggregated daily totals
write_csv(collapsed.df, "outputs/daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

# export aggregated daily totals by age
write_csv(age.df, "outputs/age_daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/age_daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

# export aggregated weekly totals by administrative area
write_csv(ue.weekly, "outputs/admin_area_weekly_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/admin_area_weekly_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

# export aggregated weekly totals by administrative area
write_csv(ue.monthly, "outputs/admin_area_monthly_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/admin_area_monthly_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

# export full long dataset
write_csv(long.df, "outputs/age_admin_area_daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/age_admin_area_daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)


###############################################################################
## UPDATE  for csv and API BY AGE AND ADMINISTRATIVE AREA
###############################################################################

# read new data for 2020
x <- read_csv("data/UMRLI po DNEVIH-STAROSTNIH SKUPINAH-SPOLU--(1.1.2020-21.12.2020)po UE.csv")

# rename, recode and cut last week
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
  filter( date < "2020-12-14") %>%
  unite(group, sex, ageg, sep = ".", remove = FALSE) -> new

# get correct column order
new %>%
  group_by(sex, age, group) %>%
  summarise(n = n()) %>%
  pull(group) -> col.order

# shape and clean into long table format
new %>%
  select(date, group, ue, deceased) %>%
  mutate(group = factor(group, levels = col.order, ordered = TRUE)) %>%
  arrange(date, group) -> long.new

# collapse ages
long.new %>%
  group_by(date, ue) %>%
  summarise(deceased = sum(deceased))  %>%
  spread(ue, deceased) -> ue.new


# collapse areas
long.new %>%
  group_by(date, group) %>%
  summarise(deceased = sum(deceased))  %>%
  spread(group, deceased) -> age.new


# collapse areas and age
long.new %>%
  group_by(date) %>%
  summarise(deceased = sum(deceased))  -> collapsed.new.df


# group into weeks
ue.new %>%
  mutate(week = isoweek(date),
         year = year(date)) %>%
  group_by(year, week) %>%
  summarise_at(vars(Ajdovščina:Žalec), ~sum(.x, na.rm = TRUE)) -> ue.new.weekly

# group into months (with no of days)
ue.new %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise_at(vars(Ajdovščina:Žalec), ~sum(.x, na.rm = TRUE)) %>%
  left_join(ue.df %>%
              mutate(month = month(date),
                     year = year(date)) %>%
              group_by(year, month) %>%
              summarise(days = n())) -> ue.new.monthly



###############################################################################
old <- read_csv("outputs/daily_deaths_slovenia.csv")

old %>% 
  filter(date < "2020-01-01") %>% 
  bind_rows(collapsed.new.df) -> update

# export aggregated daily totals 
write_csv(update, "outputs/daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

###############################################################################
old <- read_csv("outputs/age_daily_deaths_slovenia.csv", col_types = c(date = "D", .default = "n"))

old %>% 
  filter(date < "2020-01-01") %>% 
  bind_rows(age.new) -> update

# export aggregated daily totals 
write_csv(update, "outputs/age_daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/age_daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

###############################################################################
old <- read_csv("outputs/admin_area_weekly_deaths_slovenia.csv")

old %>% 
  filter(year < 2020) %>% 
  bind_rows(ue.new.weekly) -> update

# export aggregated weekly totals by administrative area
write_csv(update, "outputs/admin_area_weekly_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/admin_area_weekly_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

###############################################################################
old <- read_csv("outputs/admin_area_monthly_deaths_slovenia.csv")

old %>% 
  filter(year < 2020) %>% 
  bind_rows(ue.new.monthly) -> update

# export aggregated weekly totals by administrative area
write_csv(update, "outputs/admin_area_monthly_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/admin_area_monthly_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

###############################################################################
old <- read_csv("outputs/age_admin_area_daily_deaths_slovenia.csv")

old %>% 
  filter(date < "2020-01-01") %>% 
  bind_rows(long.df) -> update

# export full long dataset
write_csv(update, "outputs/age_admin_area_daily_deaths_slovenia.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "outputs/age_admin_area_daily_deaths_slovenia.timestamp",
            row.names = FALSE, col.names = FALSE)

