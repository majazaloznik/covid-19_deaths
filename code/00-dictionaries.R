################################################################################
###############################################################################
#' Script for original import of this file:
#' /data/csv/dict-administrative_areas.csv"
#' 
#' The data source are two SURS tables:
#' http://www.stat.si/dokument/5428/regije_UE.xls
#' http://www.stat.si/dokument/5430/SKTE4_SKTE5.xls
#' 
#' The output has the following vars:
#' 
#' iso.code - ISO 3166-2:SI of Slovenian municipalities (SI-001 - SI-213)
#' muicipality.name - municipality name
#' ue.id - ID code of administrative area (1-58)
#' ue.name - administrative area name
#' region.id - numeric ID code of region
#' region.code - two letter alpha code of region (same as in dict.regions.csv)
#' region.name - region name
#' 
###############################################################################
###############################################################################

##############################################################################
# preliminaries
###############################################################################
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(fuzzyjoin)
library(stringr)

###############################################################################
## import and prepare clean table
##############################################################################

# mapping regions on UEs source: http://www.stat.si/dokument/5428/regije_UE.xls
map1 <- read_excel("data/regije_UE.xls", skip = 2)

# add region abreviations
reg.code <- c("ms", "mb", "sg", "ce", "za", "kk", "nm", "lj",
              "kr", "po", "ng", "kp")

# clean up dictionary
map1 %>% 
  select(1:4) %>% 
  bind_rows(map1[6:9]) %>% 
  setNames(c("region.id", "region.name", "ue.id", "ue.name")) %>% 
  mutate(ue.name = sub("^(\\D*).*", "\\1", ue.name)) %>% 
  mutate(region.code = reg.code[region.id]) %>% 
  relocate(region.code, .after = region.id) -> ue.reg

# get ue names from long table
ue.names <- read_csv("outputs/age_admin_area_daily_deaths_slovenia.csv")

ue.names %>% 
  pull(ue) %>% 
  unique() %>% 
  as.data.frame() %>% 
  setNames("ue.name")-> ue.names

# match ue names fuzzy 
ue.reg %>% 
  stringdist_right_join(ue.names, by=(c(ue.name = "ue.name")), 
                        ignore_case =TRUE, max_dist = 1) %>% 
  select(-ue.name.x) %>% 
  rename(c("ue.name" = "ue.name.y")) ->    ue.reg


# mapping UEs on municipalities: http://www.stat.si/dokument/5430/SKTE4_SKTE5.xls
map2 <- read_excel("data/SKTE4_SKTE5.xls", skip = 2)

# clean up dictionary
map2 %>% 
  select(1:4) %>% 
  bind_rows(map2[6:9]) %>% 
  setNames(c("ue.id", "ue.name", "iso.code", "municipality.name")) %>% 
  mutate(ue.name = sub("^(\\D*).*", "\\1", ue.name)) %>% 
  mutate(iso.code = paste0("SI-",
                           str_pad(iso.code, width=3, 
                                   side="left", pad="0")))-> municipality.ue

# match ue names fuzzy, but manually fix izola
municipality.ue %>% 
  select(iso.code, municipality.name, ue.id, ue.name) %>% 
  mutate(ue.name = ifelse(ue.name == "IZOLA/ISOLA", "Izola", ue.name)) %>% 
  stringdist_right_join(ue.names, by=(c(ue.name = "ue.name")), 
                        ignore_case =TRUE, max_dist = 1) %>% 
  select(-ue.name.x, ue.id, ue.name.y, iso.code, municipality.name) %>% 
  rename(c("ue.name" = "ue.name.y")) ->    municipality.ue   

# join both together 
municipality.ue %>% 
  left_join(ue.reg) %>% 
  arrange(municipality.name) -> dictionary

# write to csv
write_csv(dictionary, "../data/csv/dict-administrative_areas.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), 
            "../data/csv/dict-administrative_areas.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

