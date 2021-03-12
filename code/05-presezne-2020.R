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

# prese#ne surs
download.file("https://pxweb.stat.si:443/SiStatData/sq/2400",
              "data/excess_mortality_surs.csv")

surs <- read_csv("data/excess_mortality_surs.csv", 
                 locale = locale(encoding = "Windows-1250"),
                 skip = 1)

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

# get annual totals for last 6 years
crp.monthly %>% 
  select(year, month, deceased) %>% 
  filter(year > 2014 & year < 2021) %>% 
  group_by(year) %>% 
  summarise(deceased = sum(deceased)) -> annual

annual %>% 
  filter(year !=2020) %>% 
  summarise(mean = mean(deceased)) -> mean.5y

annual %>% 
  mutate(excess = (deceased/pull(mean.5y) - 1) * 100) -> annual
  

  
png(filename="figures/excess.annual.png", 450, 600)
old.par <- par(mar = c(0,6,0,6))
plot(rep(1,6),  annual$deceased,
     xlim = c(0, 2),
     pch = 20,
     cex = 1.3,
     col = c(rep("dodgerblue4", 5), "violetred1"),
     bty = "n",
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(20000, 25500))

abline(h = (c(-0.05, 0.05, 0.1, 0.15, 0.2) +1) * pull(mean.5y),
       col = "gray", lty = "96", lwd = 0.7)

lines(c(0, 1.15), c(mean.5y, mean.5y))
lines(c(1.85, 2), c(mean.5y, mean.5y))

axis(2, las = 2, at = seq(20000, 25000, 1000) )
axis(4, las = 2, at = (c(seq(-0.05, 0.20, 0.05)) +1) * pull(mean.5y),
     labels = paste(100 * (c(seq(-0.05, 0.20, 0.05))), "%"))

mtext(side = 2, line = 4, "skupno število smrti")
mtext(side = 4, line = 4, "presežek (primankljaj) glede na povprečje 2015-19")
text(0.85, annual[1,2], annual[1,1], cex = 0.8)
text(0.85, annual[2,2], annual[2,1], cex = 0.8)
text(0.85, annual[3,2], annual[3,1], cex = 0.8)
text(0.85, annual[4,2]-40, annual[4,1], cex = 0.8)
text(1.15, annual[5,2], annual[5,1], cex = 0.8)
text(0.85, annual[6,2], annual[6,1], cex = 0.8)
text(1.5, pull(mean.5y), "povprečje 2015-19", cex = 0.8)
dev.off()

###############################################################################
## compare surs and sledilnik
###############################################################################

surs %>% 
  mutate(month = 1:12) %>% 
  `colnames<-` (c("mesec", "surs.excess", "month")) %>% 
  select(-mesec) -> surs

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

df.m %>% 
  left_join(surs) -> df.m

write_csv(df.m, "data/excess_surs_sledilnik.csv")

png(filename="figures/excess_surs_vs_crp.png", 800, 480)
par(mar = c(4,4,2,1))
plot(df.m$month, df.m$excess, type = "l",
     bty = "n", axes = FALSE, xlab = "mesec", ylab = "presežne smrti",
     lwd = 2,
     col = "dodgerblue3")
abline(h = seq(0, 80, 20), col = "gray", lty = "96", lwd = 0.7)
lines(df.m$month, df.m$surs.excess, col = "olivedrab3", lwd = 2)
axis(1, at = 1:12)
axis(2, las = 2)

legend(2, 75,
       legend=c("presežne smrti - CRP (Sledilnik)",
                "presežne smrti - SURS"),
       col=c("dodgerblue3","olivedrab3"),
       cex=0.9, bty = "n", lwd = 2)
dev.off()

df.m %>% 
  select(excess, surs.excess) %>% 
  as.matrix() -> mat

png(filename="figures/excess_surs_vs_crp-barplot.png", 800, 480)
barplot(t(mat), beside = TRUE,
        col = c("dodgerblue3", "olivedrab3"), names.arg = 1:12,
        ylim = c(0,100),
        axes = FALSE,
        ylab = "")
axis(2, las = 2)
grid(ny = 5, nx = NA)

legend(2, 75,
       legend=c("presežne smrti - CRP (Sledilnik)",
                "presežne smrti - SURS"),
       fill=c("dodgerblue3","olivedrab3"),
       cex=0.9, bty = "n")
dev.off()

