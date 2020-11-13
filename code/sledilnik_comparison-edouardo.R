library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)

sl <- fread("https://github.com/sledilnik/data/raw/master/csv/stats.csv", select = c("date", "state.deceased.todate"))
setnames(sl, c("date", "total_deaths"))
sl <- sl[!is.na(total_deaths)]
sl[, source := "Sledilnik"]
sl[, date := ymd(date)]

ecdc <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv", select = c("location", "date", "total_deaths"))
ecdc <- ecdc[location == "Slovenia"]
ecdc[, source := "ECDC"]
ecdc[, date := ymd(date)]

jhu <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
jhu <- jhu[`Country/Region` == "Slovenia"]
jhu[, c("Lat", "Long", "Province/State", "Country/Region") := NULL]
jhu <- jhu %>% gather(date, total_deaths, 1:ncol(jhu)) %>% data.table()
jhu[, date := mdy(date)]
jhu[, source := "JHU"]

df <- rbindlist(list(sl, ecdc, jhu), fill = T)

ggplot(data = df, aes(x = date, y = total_deaths, colour = source)) + geom_line()
