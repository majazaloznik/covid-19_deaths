setwd("~/Google Drive/XtraWork/sledilnik/owiv-sledilnik-deaths")
library(readr)
library(dplyr)

owid <- read_csv("data/owid-covid-data.csv")

owid %>%
  filter(iso_code == "SVN") %>%
  select(date, new_deaths, new_deaths_smoothed) -> owid.slo

download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/stats.csv"),
              "data/stats.csv")
sledilnik <- read_csv("data/stats.csv")
sledilnik %>%
  mutate(deceased = c(state.deceased.todate[1], diff(state.deceased.todate))) %>%
  mutate(deceased.smoothed = rollmean(deceased, 7, na.pad = TRUE)) %>%
  select(date, deceased, deceased.smoothed) -> sledilnik.deaths


# plot
png(filename="owid-sledilnik_slovenia_deaths.png", 800, 480)
plot(owid.slo$date, owid.slo$new_deaths, type = "l",
     xlab = "",
     ylab = "",
     ylim = c(0,40),
     xlim = as.Date(c("2020-10-01", "2020-11-10")),
     axes = FALSE, lty = 5, lwd = 2, col = "darkgray")
axis.Date(1, owid.slo$date, at = seq(min(owid.slo$date), max(owid.slo$date), by="7 day"))
axis(2)
lines(sledilnik.deaths$date, sledilnik.deaths$deceased, col = "red", lwd = 2, lty = 5)
lines(sledilnik.deaths$date, sledilnik.deaths$deceased.smoothed, col = "red", lwd = 2 )
lines(owid.slo$date, owid.slo$new_deaths_smoothed, lwd = 2, col = "darkgray")
legend(as.Date("2020-10-01"), 40,
       legend=c("Sledilnik daily deaths (smoothed)", "Sledilnik daily deaths",
                "OWID (ECDC) daily deaths (smoothed)", "OWID (ECDC) daily deaths"),
       col=c("red", "red", "darkgray", "darkgray"), lty=c(1,5,1,5),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 2.5,  adj = 0, cex = 1.1,
      "Comparison of daily deaths on OWID (ECDC) and Sledilnik.org (official government data)")
mtext(side = 3, line = 1.5,  adj = 0, cex = 0.9,
      "(full line is the 7 day rolling average)")
mtext(side = 2, line = 2.5,   cex = 1,
      "daily deaths")
mtext(side = 1, line = 2.5,   cex = 1, "date")
dev.off()
