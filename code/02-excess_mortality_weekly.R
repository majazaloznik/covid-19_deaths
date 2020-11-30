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
# 
# x <- read_excel("data/št. UMRLI_2010_2020_1.xls")
# 
# x %>% 
#   filter(! is.na(Dan)) %>% 
#   rename(date = Dan, deceased = "št. na Dan") %>% 
#   select(date, deceased) %>% 
#   mutate(date = as.Date(date, "Y%-m%-d%"))-> csv
# 
# write_csv(csv, "data/daily_deaths_slovenia.csv")
# 
# write.table(round(as.numeric(Sys.time()), 0), "data/daily_deaths_slovenia.timestamp",
#             row.names = FALSE, col.names = FALSE)


###############################################################################
#### import data
###############################################################################

# uncomment to download
# sledilnik = NIJZ daily death data from region table tb5
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/daily_deaths_slovenia.csv"),
              "data/daily_deaths_slovenia.csv")

# sledinlnik = gov data + NIJZ daily entry data
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/stats.csv"),
              "data/stats.csv")

# import data
sledilnik <- read_csv("data/stats.csv")

crp <- read_csv("data/daily_deaths_slovenia.csv")


###############################################################################
# clean and join data - weekly
###############################################################################

# aggregate to weekly
sledilnik %>% 
  select(date, state.deceased.todate) %>% 
  mutate(covid = diff(c(0, state.deceased.todate))) %>% 
  mutate(week = week(date)) %>% 
  group_by(week) %>%
  summarise(covid = sum(covid, na.rm = TRUE),
            days = n(),
            daily.covid = covid / days) %>% 
  select(week, daily.covid) -> covid.weekly

min.week <- min(covid.weekly$week)
max.week <- max(covid.weekly$week)

crp %>% 
  mutate(year = year(date),
         week = week(date)) %>% 
  group_by(year, week) %>%
  summarise(deceased = sum(deceased),
            days = n(),
            daily.deceased = deceased / days) %>% 
  ungroup()-> crp.weekly

# get 5 - year average daily death for each week + c.i.
crp.weekly %>% 
  select(year, week, daily.deceased) %>% 
  filter(year > 2014 & year < 2020) %>% 
  group_by(week) %>% 
  summarise(mean = mean(daily.deceased),
         sd = sd(daily.deceased),
         se = sd/sqrt(5),
         low.ci = mean - qt(1 - (0.05 / 2), 5 -1 ) * se,
         hi.ci = mean + qt(1 - (0.05 / 2), 5 - 1) * se) %>% 
  select(-se, -sd) ->    crp.weekly.5y

# merge with covid data calculate ratios and clean up
crp.weekly %>% 
  filter(year == 2020 & week <= (max.week -2)) %>% 
  select(week, daily.deceased.2020 = daily.deceased) %>% 
  right_join(crp.weekly.5y) %>%  
  left_join(covid.weekly) %>% 
  mutate(excess = (daily.deceased.2020 / mean - 1 ) * 100,
         covid = (daily.covid / mean) * 100,
         low = (low.ci / mean - 1 ) * 100,
         hi = (hi.ci / mean - 1 ) * 100) -> df


  
###############################################################################
## plot - weekly
###############################################################################

png(filename="figures/weekly.ecxcess-15-19-baseline.png", 800, 480)
par(mar = c(4, 1, 4, 4.5) + 0.1)
plot(df$week, df$excess, type = "n",
     xlab = "",
     ylab = "",
     ylim = c(-20,100),
     axes = FALSE)

axis(1, )
axis(4, las = 2, at =  seq(-20,100, by = 10),labels = paste0(seq(-20,100, by = 10), " %"))

abline(h = seq(-20,100, 10), col = "gray", lty = "93", )
polygon(c(min.week - 1, min.week:max.week, max.week), c(0, df$covid[8:48], 0),
        col = "bisque1", border = "bisque1")
lines(df$week, df$low,   lwd = 3, col = "gray", lty = 3)
lines(df$week, df$hi,   lwd = 3, col = "gray", lty = 3)

lines(c(1,52), c(0,0))
lines(df$week, df$excess,   lwd = 3, col = "red3")

mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "weekly excess mortality relative to historical baseline and Covid-19 attributed deaths")
mtext(side = 3, line = 0.5,  adj = 0, cex = 0.9,
      "(based on simple average over 2015-2019 with 95% confidence intervals in gray)")
mtext(side = 1, line = 2.5,  cex = 0.9,
      "week")
dev.off()


###############################################################################
# clean and join data - monthly
###############################################################################

sledilnik %>% 
  select(date, state.deceased.todate) %>% 
  mutate(covid = diff(c(0, state.deceased.todate))) %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>%
  summarise(covid = sum(covid, na.rm = TRUE),
            days = n(),
            daily.covid = covid / days) %>% 
  select(month, daily.covid) -> covid.monthly 

crp %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>%
  summarise(deceased = sum(deceased),
            days = n(),
            daily.deceased = deceased / days) %>% 
  ungroup()-> crp.monthly

# get 5 - year average daily death for each week + c.i.
crp.monthly %>% 
  select(year, month, daily.deceased) %>% 
  filter(year > 2014 & year < 2020) %>% 
  group_by(month) %>% 
  summarise(mean = mean(daily.deceased),
            sd = sd(daily.deceased),
            se = sd/sqrt(5),
            low.ci = mean - qt(1 - (0.05 / 2), 5 -1 ) * se,
            hi.ci = mean + qt(1 - (0.05 / 2), 5 - 1) * se) %>% 
  select(-se, -sd) ->    crp.monthly.5y

crp.monthly %>% 
  filter(year == 2020 ) %>% 
  select(month, daily.deceased.2020 = daily.deceased) %>% 
  right_join(crp.monthly.5y) %>%  
  left_join(covid.monthly) %>% 
  mutate(excess = (daily.deceased.2020 / mean - 1 ) * 100,
         covid = (daily.covid / mean) * 100,
         low = (low.ci / mean - 1 ) * 100,
         hi = (hi.ci / mean - 1 ) * 100) -> df.m


###############################################################################
## plot - monthly
###############################################################################

# png(filename="figures/monthly.excess-15-19-baseline.png", 800, 480)
par(mar = c(4, 2, 4, 4.5) + 0.1)
plot(df.m$month, df.m$excess, type = "n",
     xlab = "",
     ylab = "",
     ylim = c(-10,60),
     axes = FALSE)

axis(1, at = 1:12, labels = c(month.abb[1:10], paste0(month.abb[11], "*"), month.abb[12] ))
axis(4, las = 2, at =  seq(-10,60, by = 10),labels = paste0(seq(-10,60, by = 10), " %"))
abline(h = seq(-10,60, 10), col = "gray", lty = "93", )
polygon(c(2, 3:11, 11), c(0, df.m$covid[3:11], 0),
        col = "bisque1", border = "bisque1")
lines(c(1,12), c(0,0))
lines(df.m$month, df.m$low,   lwd = 3, col = "gray", lty = 3)
lines(df.m$month, df.m$hi,   lwd = 3, col = "gray", lty = 3)
lines(df.m$month, df.m$excess,   lwd = 3, col = "red3")

mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to historical baseline and Covid-19 attributed deaths")
mtext(side = 3, line = 0.5,  adj = 0, cex = 0.9,
      "(based on simple average over 2015-2019)")
mtext(side = 1, line = 2.5,  adj = 0, cex = 0.8,
      "* Data for November are incomplete")
# dev.off()


