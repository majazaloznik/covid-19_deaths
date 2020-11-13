###############################################################################
# preliminaries
###############################################################################
library(readr)
library(readxl)
library(dplyr)

###############################################################################
#### import data ##############################################################

# # uncomment to download
# # sledinlnik = gov data + NIJZ daily entry data
# download.file(paste0("https://raw.githubusercontent.com/",
#                      "sledilnik/data/master/csv/stats.csv"),
#               "data/stats.csv")
# 
# # sledilnik = NIJZ daily death data from region table tb5
# download.file(paste0("https://raw.githubusercontent.com/",
#                      "sledilnik/data/master/csv/deceased-regions.csv"),
#               "data/deceased-regions.csv")
# 
# # NIJZ daily death for 3 different timestamps 
# download.file("https://www.nijz.si/sites/www.nijz.si/files/uploaded/porocilo_stevilo_potrjenih_primerov_covid-19_11112020_splet.xlsx",
#               "data/11.11.xlsx")
# download.file("https://www.nijz.si/sites/www.nijz.si/files/uploaded/porocilo_stevilo_potrjenih_primerov_covid-19_04112020_splet.xlsx",
#               "data/4.11.xlsx")
# download.file("https://www.nijz.si/sites/www.nijz.si/files/uploaded/porocilo_stevilo_potrjenih_primerov_covid-19_28102020_splet.xlsx",
#               "data/28.10.xlsx")

# import 
sledilnik <- read_csv("data/stats.csv")
nijz.regions <- read_csv("data/deceased-regions.csv")
nijz11.11 <- read_excel("data/11.11.xlsx", sheet = "tb6", range = "A3:U114",
                        col_types = c("date", rep("numeric", 20)))

nijz04.11 <- read_excel("data/4.11.xlsx", sheet = "tb6", range = "A3:V107",
                        col_types = c("date", rep("numeric", 21)))

nijz28.10 <- read_excel("data/28.10.xlsx", sheet = "tb6", range = "A3:S100",
                        col_types = c("date", rep("numeric", 18)))

###############################################################################
#### clean up #################################################################

sledilnik %>%
  mutate(deceased.gov = c(state.deceased.todate[1], diff(state.deceased.todate)),
         deceased.nijz = c(deceased.todate[1], diff(deceased.todate))) %>%
  select(date,  state.deceased.todate, deceased.todate, deceased.gov, deceased.nijz) -> df

nijz.regions %>% 
  mutate(total = rowSums(.[2:102], na.rm = TRUE),
         nijz = c(total[1], diff(total))) %>% 
  select(date, nijz)-> nijz.regions

nijz11.11 %>%  
  rename("date" = `...1`,
         "no3" = `SKUPAJ...21`) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, no3) -> no3

nijz04.11 %>%  
  rename("date" = `...1`,
         "no2" = `SKUPAJ...22`) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, no2) -> no2

nijz28.10 %>%  
  rename("date" = `...1`,
         "no1" = `SKUPAJ...19`) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, no1) -> no1

#### join tables ##############################################################
df %>%  left_join(nijz.regions) %>% 
  left_join(no1) %>% 
  left_join(no2) %>% 
  left_join(no3) %>% 
  mutate(no3 = replace(no3, is.na(no3) & date <= as.Date("2020-10-28"), 0))  -> df

###############################################################################
#### plot cumulative gov vs NIJZ ##############################################
# png(filename="figures/cumulative.png", 800, 480)
par(mar = c(5, 5, 4, 0.5) + 0.1)
plot(df$date, df$state.deceased.todate, type = "l",
     xlab = "",
     ylab = "",
     #ylim = c(0,40),
     xlim = as.Date(c("2020-07-13", "2020-11-15")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "black")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "2 week"),
          format = "%d.%m.")

axis(2, las = 2)
lines(df$date, df$deceased.todate, col = "red", lwd = 2, lty = 1)

legend(as.Date("2020-07-13"), 600,
       legend=c("Podatki vlade", "Podatki NIJZ"),
       col=c("black", "red"), lty=c(1,1),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Primerjava kumulativnega števila umrlih po podatkih NIJZ in vlade")
mtext(side = 2, line = 3,   cex = 1,"skupno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
# dev.off()



#### plot daily ###############################################################
# png(filename="figures/daily.png", 800, 480)
par(mar = c(5, 5, 4, 0.5) + 0.1)
plot(df$date, df$deceased.gov, type = "l",
     xlab = "",
     ylab = "",
     #ylim = c(0,40),
     xlim = as.Date(c("2020-07-13", "2020-11-15")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "black")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "2 week"),
          format = "%d.%m.")

axis(2, las = 2)
lines(df$date, df$deceased.nijz, col = "red", lwd = 2, lty = 1)

legend(as.Date("2020-07-13"), 40,
       legend=c("Število umrlih (vlada)", "Število vnosov (NIJZ)"),
       col=c("black", "red"), lty=c(1,1),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Primerjava dnevnega števila umrlih po podatkih NIJZ in vlade")
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
# dev.off()




#### plot daily NIJZ entry vs gov #####################################################
# png(filename="figures/dailyNIJZ.png", 800, 480)
par(mar = c(5, 5, 4, 0.5) + 0.1)
plot(df$date, df$no3, type = "l",
     xlab = "",
     ylab = "",
     ylim = c(0,40),
     xlim = as.Date(c("2020-07-13", "2020-11-11")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "black")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "2 week"),
          format = "%d.%m.")

axis(2, las = 2)
lines(df$date, df$deceased.nijz, col = "red", lwd = 2, lty = 1)

legend(as.Date("2020-07-13"), 40,
       legend=c("Število umrlih (NIJZ)", "Število vnosov (NIJZ)"),
       col=c("black", "red"), lty=c(1,1),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Primerjava dnevnega števila umrlih in dnevnega števila vnosov po podatkih NIJZ ")
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
# dev.off()


##### plot daily NIJZ three tables ############################################
# png(filename="figures/threeNIJZ.png", 800, 480)
par(mar = c(5, 5, 4, 0.5) + 0.1)
plot(no1$date, no1$no1, type = "l",
     xlab = "",
     ylab = "",
     ylim = c(0,20),
     xlim = as.Date(c("2020-10-01", "2020-11-15")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "orange")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "2 week"),
          format = "%d.%m.")

axis(2, las = 2)

lines(no2$date, no2$no2, col = "red", lwd = 2, lty = 1)
lines(no3$date, no3$no3, col = "black", lwd = 2, lty = 1)

legend(as.Date("2020-10-01"), 20,
       legend=c("Število umrlih do 28.10. (NIJZ)", "Število umrlih do 4.11. (NIJZ)",
                "Število umrlih do 11.11. (NIJZ)"),
       col=c("orange", "red", "black"), lty=c(1,1,1),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 1.5,  adj = 0, cex = 1.1,
      "Primerjava dnevnega števila umrlih vnešenih v NIJZ tabele na različne dni")
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
# dev.off()


#### plot daily NIJZ three tables + gov #######################################
# png(filename="figures/threeNIJZgov.png", 800, 480)
par(mar = c(5, 5, 4, 0.5) + 0.1)
plot(no1$date, no1$no1, type = "l",
     xlab = "",
     ylab = "",
     ylim = c(0,40),
     xlim = as.Date(c("2020-10-01", "2020-11-15")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "orange")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "2 week"),
          format = "%d.%m.")

axis(2, las = 2)

lines(no2$date, no2$no2, col = "red", lwd = 2, lty = 1)
lines(no3$date, no3$no3, col = "black", lwd = 2, lty = 1)
lines(df$date, df$deceased.gov, col = "gray", lwd = 2, lty = 5)

legend(as.Date("2020-10-01"), 20,
       legend=rev(c("Število umrlih do 28.10. (NIJZ)", "Število umrlih do 4.11. (NIJZ)",
                "Število umrlih do 11.11. (NIJZ)", "Število umrlih (vlada)")),
       col=rev(c("orange", "red", "black", "gray")), lty=rev(c(1,1,1, 5)),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 2.5,  adj = 0, cex = 1.1,
      "Primerjava dnevnega števila umrlih vnešenih v NIJZ tabele na različne dni")
mtext(side = 3, line = 1,  adj = 0, cex = 1.1,
      "IN 'TA PRAVIH' PODATKOV")
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
# dev.off()




