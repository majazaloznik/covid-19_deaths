###############################################################################
# preliminaries
###############################################################################
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 


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
  select(date, state.deceased.todate, deceased.todate) %>% 
  filter(date < "2021-01-01") %>% 
  mutate(covid = diff(c(0, state.deceased.todate)),
         covid.nijz = diff(c(0, deceased.todate))) %>% 
  mutate(week = isoweek(date)) %>% 
  group_by(week) %>%
  summarise(covid = sum(covid, na.rm = TRUE),
            covid.nijz = sum(covid.nijz, na.rm = TRUE),
            days = n(),
            daily.covid = covid / days,
            daily.covid.nijz = covid.nijz / days) %>% 
  select(week, daily.covid, daily.covid.nijz, covid, covid.nijz) -> covid.weekly

covid.weekly %>% 
  ungroup() %>% 
  summarise_all(sum)
min.week <- min(covid.weekly$week)
max.week <- max(covid.weekly$week)

crp %>% 
  mutate(year = year(date),
         week = isoweek(date)) %>% 
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
            min = min(daily.deceased),
            max = max(daily.deceased)) ->    crp.weekly.5y

# merge with covid data calculate ratios and clean up
crp.weekly %>% 
  filter(year == 2020 & week <= (max.week -2)) %>% 
  select(week, daily.deceased.2020 = daily.deceased) %>% 
  right_join(crp.weekly.5y) %>%  
  left_join(covid.weekly) %>% 
  mutate(excess = (daily.deceased.2020 / mean - 1 ) * 100,
         covid = (daily.covid / mean) * 100,
         covid.nijz = (daily.covid.nijz / mean) * 100,
         min = (min / mean - 1 ) * 100,
         max = (max / mean - 1 ) * 100) -> df

write_csv(df, "outputs/weekly.excess.deaths.csv")



###############################################################################
## plot - weekly wiht min&max
###############################################################################

png(filename="figures/weekly.min.max.ecxcess-15-19-baseline-w-nijz.png", 800, 480)
par(mar = c(4, 1, 4, 4.5) + 0.1)
plot(df$week, df$excess, type = "n",
     xlab = "",
     ylab = "",
     ylim = c(-20,100),
     axes = FALSE,
     xlim = c(0, 53))

axis(1, )
axis(4, las = 2, at =  seq(-20,100, by = 10),labels = paste0(seq(-20,100, by = 10), " %"))

abline(h = seq(-20,100, 10), col = "gray", lty = "93", )
# polygon(c(min.week - 1, min.week:max.week, max.week), c(0, df$covid[!is.na(df$covid)], 0),
#         col = "bisque1", border = "bisque1")
lines(df$week, df$min,   lwd = 3, col = "gray", lty = 3)
lines(df$week, df$max,   lwd = 3, col = "gray", lty = 3)

lines(c(1,52), c(0,0))
lines(df$week, df$excess,   lwd = 3, col = "red3")
lines(df$week, df$covid,   lwd = 2, col = "purple4")
lines(df$week, df$covid.nijz,   lwd = 2, col = "purple1")
lines(df$week, df$excess,   lwd = 3, col = "red3")
legend(5, 90,
       legend = c("Eexcess", "NIJZ data", "Gvmnt data"),
       col = c("red3", "purple1", "purple4"), lwd = c(3,2,2), bty = "n")
mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "weekly excess mortality relative to historical baseline and Covid-19 attributed deaths")
mtext(side = 3, line = 0.5,  adj = 0, cex = 0.9,
      "(based on simple average over 2015-2019 with minimum and maximum in gray)")
mtext(side = 1, line = 2.5,  cex = 0.9,
      "week")
dev.off()

###############################################################################
# clean and join data - monthly
###############################################################################

sledilnik %>% 
  filter(date < "2021-01-01") %>% 
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
            min = min(daily.deceased),
            max = max(daily.deceased)) ->    crp.monthly.5y

crp.monthly %>% 
  filter(year == 2020 ) %>% 
  select(month, daily.deceased.2020 = daily.deceased) %>% 
  right_join(crp.monthly.5y) %>%  
  left_join(covid.monthly) %>% 
  mutate(excess = (daily.deceased.2020 / mean - 1 ) * 100,
         covid = (daily.covid / mean) * 100,
         min = (min / mean - 1 ) * 100,
         max = (max / mean - 1 ) * 100) -> df.m

write_csv(df.m, "outputs/monthly.excess.deaths.csv")

###############################################################################
## plot - monthly
###############################################################################

# png(filename="figures/monthly.excess-15-19-baseline.png", 800, 480)
par(mar = c(4, 2, 4, 4.5) + 0.1)
plot(df.m$month, df.m$excess, type = "n",
     xlab = "",
     ylab = "",
     ylim = c(-10,90),
     axes = FALSE)

axis(1, at = 1:12, labels = c(month.abb[1:12] ))
axis(4, las = 2, at =  seq(-10,90, by = 10),labels = paste0(seq(-10,90, by = 10), " %"))
abline(h = seq(-10,90, 10), col = "gray", lty = "93", )
polygon(c(2, 3:12, 12), c(0, df.m$covid[3:12], 0),
        col = "bisque1", border = "bisque1")
lines(c(1,12), c(0,0))
lines(df.m$month, df.m$min,   lwd = 3, col = "gray", lty = 3)
lines(df.m$month, df.m$max,   lwd = 3, col = "gray", lty = 3)
lines(df.m$month, df.m$excess,   lwd = 3, col = "red3")

mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to historical baseline and Covid-19 attributed deaths")
mtext(side = 3, line = 0.5,  adj = 0, cex = 0.9,
      "(based on simple average over 2015-2019)")
mtext(side = 1, line = 2.5,  adj = 0, cex = 0.8,
      "* Data for December are incomplete")
# dev.off()


