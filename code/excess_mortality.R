###############################################################################
# preliminaries
###############################################################################
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 

###############################################################################
#### import data ##############################################################
###############################################################################

# # uncomment to download
# # sledilnik = NIJZ daily death data from region table tb5
# download.file(paste0("https://raw.githubusercontent.com/",
#                      "sledilnik/data/master/csv/monthly_deaths_slovenia.csv"),
#               "data/monthly_deaths_slovenia.csv")
#
# # sledinlnik = gov data + NIJZ daily entry data
# download.file(paste0("https://raw.githubusercontent.com/",
#                      "sledilnik/data/master/csv/stats.csv"),
#               "data/stats.csv")

# import data
sledilnik <- read_csv("data/stats.csv")

surs <- read_csv("data/monthly_deaths_slovenia.csv")

###############################################################################
# clean and join data
sledilnik %>% 
  select(date, state.deceased.todate) %>% 
  mutate(covid.deaths = diff(c(0, state.deceased.todate))) %>% 
  mutate(month = as.numeric(format(date, "%m"))) %>%
  group_by(month) %>%
  summarise(covid.deaths = sum(covid.deaths)) -> covid

st <- as.Date("2020-02-01")
en <- as.Date("2020-11-01")
t <-days_in_month(seq(st, en, by = "1 month"))

# normalize to daily
covid %>% 
  dplyr::mutate(daily  = covid.deaths / t ) -> covid

st <- as.Date("1977-01-01")
en <- as.Date("2020-10-01")
t <-days_in_month(seq(st, en, by = "1 month"))

surs %>% 
  dplyr::mutate(month = as.numeric(month),
                daily  = deceased / t ) %>% 
  select(-deceased) %>% 
  spread(year, daily) %>% 
  select(month, `2015`:`2020`) %>% 
  mutate(mean = rowMeans(.[,2:6]))   -> df.5

# join and calculate perncent excess 
left_join(df.5, covid)  %>% 
  mutate(excess = (`2020` / mean - 1 ) * 100,
         covid = (daily / mean) * 100) -> df
  
  
###############################################################################
## plot 
###############################################################################
# inspiration: https://www.economist.com/graphic-detail/2020/07/15/tracking-covid-19-excess-deaths-across-countries
# https://github.com/TheEconomist/covid-19-excess-deaths-tracker

png(filename="figures/ecxcess-15-19-baseline.png", 800, 480)
par(mar = c(4, 4, 4, 1.5) + 0.1)
plot(df$month, df$excess, type = "n",
     xlab = "",
     ylab = "",
     ylim = c(0,60),
     axes = FALSE)

axis(1, at = 1:12, labels = c(month.abb[1:10], paste0(month.abb[11], "*"), month.abb[12] ))
axis(2, las = 2, at =  seq(0,60, by = 10),labels = paste0(seq(0,60, by = 10), " %"))
abline(h = seq(0,60, 10), col = "gray", lty = "93", )
polygon(c(3, 4:11, 11), c(0, df$covid[4:11], 0),
        col = "bisque1", border = "bisque1")
lines(c(1,12), c(0,0))
lines(df$month, df$excess,   lwd = 3, col = "red3")

mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to historical baseline and Covid-19 attributed deaths")
mtext(side = 3, line = 0.5,  adj = 0, cex = 0.9,
      "(based on simple average over 2015-2019)")
mtext(side = 1, line = 2.5,  adj = 0, cex = 0.8,
      "* Data for November are incomplete")
legend(1, 46,
       legend=c("Overall excess deaths", 
       "Historical baseline for 2015-2019 (0 %)"),
       col=c("red3","black"),
       cex=0.9, bty = "n", lty = c(1, 1), lwd = c(3, 1))
legend(1.25, 40,
       legend=c( "Deaths attributed to Covid-19"),
       bty = "n", cex = 0.9, fill ="bisque1", border = "bisque1")

dev.off()

