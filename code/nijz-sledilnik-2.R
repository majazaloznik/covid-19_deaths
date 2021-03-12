###############################################################################
# preliminaries
###############################################################################
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(googledrive)
library(zoo)


###############################################################################
#### import data ##############################################################

# # uncomment to download
# sledinlnik = gov data + NIJZ daily entry data
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/stats.csv"),
              "data/stats.csv")

df <- read_csv( "data/stats.csv")

# read old NIJZ data from googlesheet
# gs4_auth(token = drive_token())

nijz.old <- range_read(as_id("1N1qLMoWyi3WFGhIpPFzKsFmVE0IwNP3elb_c18t2DwY"),
                   sheet = "Podatki",
                   range = "A3:DA296",
                   col_names = TRUE)

# # get daily NIJZ data
# download.file(paste0("https://www.nijz.si//sites/www.nijz.si/",
#                      "files/uploaded/dnevni_prikazi_20210115.xlsx"),
#               "data/daily.nijz.xlsx")
# 
# nijz.daily <- read_xlsx("data/daily.nijz.xlsx", sheet = "Tabela 7",
#           range = "A3:D320") 

# get old weekly NIJZ data
download.file(paste0("https://www.nijz.si//sites/www.nijz.si/files/",
                     "uploaded/tedenski_prikaz_umrli202210201.xlsx"),
              "data/weekly1.nijz.xlsx")

nijz.week1 <- read_xlsx("data/weekly1.nijz.xlsx", sheet = "Tabela 3",
                        range = "B3:E337") 

# get old weekly NIJZ data
download.file(paste0("https://www.nijz.si//sites/www.nijz.si/files/",
                     "uploaded/tedenski_prikaz_umrli_20210208.xlsx"),
              "data/weekly2.nijz.xlsx")

nijz.week2 <- read_xlsx("data/weekly2.nijz.xlsx", sheet = "Tabela 3",
                          range = "B3:E344") 

# get old weekly NIJZ data
download.file(paste0("https://www.nijz.si//sites/www.nijz.si/files/",
                     "uploaded/tedenski_prikaz_umrli20210215.xlsx"),
              "data/weekly3.nijz.xlsx")

nijz.week3 <- read_xlsx("data/weekly3.nijz.xlsx", sheet = "Tabela 3",
                          range = "B3:E351") 


###############################################################################
#### clean data  ##############################################################


df %>% 
  select(date, state.deceased.todate, deceased.todate) -> df

nijz.old %>% 
  select(date, deceased.todate) %>% 
  rename("deceased.todate.old" = "deceased.todate") -> nijz.old

df %>% 
  left_join(nijz.old) -> df

# nijz.daily %>% 
#   `colnames<-`(c("date", "x", "xx", "deceased.daily")) %>% 
#   select(-x, -xx) %>% 
#   mutate(date  = as.Date(date, "%d.%m.%Y")) %>% 
#   mutate(deceased.daily = cumsum(deceased.daily)) -> nijz.daily

nijz.week1 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly1")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) %>% 
  mutate(deceased.weekly1 = cumsum(deceased.weekly1)) -> nijz.weekly1

nijz.week2 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly2")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) %>% 
  mutate(deceased.weekly2 = cumsum(deceased.weekly2)) -> nijz.weekly2


nijz.week3 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly3")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) %>% 
  mutate(deceased.weekly3 = cumsum(deceased.weekly3)) -> nijz.weekly3


df %>% 
  left_join(nijz.weekly1) -> df
df %>% 
  left_join(nijz.weekly2) -> df
df %>% 
  left_join(nijz.weekly3) -> df
         
df %>% 
  mutate(date = as.Date(date)) -> df

df %>% 
  mutate(week0 = c(0, diff(deceased.todate)),
         week1 = c(0, diff(deceased.weekly1)),
         week2 = c(0, diff(deceased.weekly2))) -> df

df %>% 
  mutate(state.daily = c(0, diff(state.deceased.todate))) -> df

df %>% 
  mutate(ratio = deceased.todate/state.deceased.todate,
         deceased.daily = c(0,diff(deceased.todate)),
         state.deceased.daily = c(0,diff(state.deceased.todate)),
         deceased7d = rollsum(deceased.daily, 7, na.pad = TRUE, align = "center"),
         state.deceased7d = rollsum(state.deceased.daily, 7, na.pad = TRUE, align = "center"),
         ratio7d = deceased7d/state.deceased7d,) -> x
                               
plot(x$date, x$ratio,
     type = "l",
     ylim = c(0,3))
grid()
lines(x$date, x$ratio7d, col = "blue")
mtext(side = 3, line = 1, 
      adj = 0,
      "ratio of NIJZ deaths (with covid) to gvmnt deaths (HOS+DSO), ratio of 7day rolling averages in blue")

###############################################################################
#### plots   ##################################################################
###############################################################################

png(filename="figures/compare-cumulative-death-by-source.png", 1400, 840)
old.par <- par(mar = c(6, 6, 4, 2))
plot(df$date, df$deceased.todate, type = "l",
     col = "purple3", lwd = 3, axes = FALSE,
     xlim = as.Date(c("2020-09-13", "2021-02-17")),
     ylim = c(0, 4000),
     xlab = "",lty = 1,
     ylab = "", 
     cex.lab = 1.4)
abline(v = seq(df$date[1]+6, df$date[length(df$date)], by = "2 week"), 
       col = "gray", lty = "96", lwd = 0.5)
abline(h = seq(0, 3500, by = 500), 
       col = "gray", lty = "96", lwd = 0.5)
lines(df$date, df$deceased.todate.old,
      col = "gray", lwd = 3)
lines(df$date, df$state.deceased.todate,
      col = "blue", lwd = 3)
lines(df$date, df$deceased.todate,
      col = "purple", lwd = 3)
lines(df$date, df$deceased.weekly3, col = "purple1", lwd = 2, lty = 3)
lines(df$date, df$deceased.weekly2, col = "purple1", lwd = 2, lty = 3)
lines(df$date, df$deceased.weekly1, col = "purple1", lwd = 2, lty = 3)

axis(2, las = 2, cex.axis = 1.4 )
axis.Date(1, at = seq(df$date[1]+6, df$date[length(df$date)], by = "2 week"),
          format = "%d.%m.", cex.axis = 1.4)
mtext(side = 3, line = 1, "Primerjava skupnega števila umrlih po podatkih vlade in NIJZ", adj = 0,
      cex = 2)
mtext(side = 2, line = 4, "skupno število umrlih", cex = 1.4)
mtext(side = 1, line = 4, "datum", cex = 1.4)

legend(as.Date("2020-09-13"), 3000,
       legend=c("število umrlih - NIJZ (tedensko poročanje)", "(prejšnji tedni)", 
                "število umrlih - vlada", "število umrlih - NIJZ (stari podatki)"),
       col=c("purple","purple", "blue", "gray"), lty= c(1,3,1,1),
       cex=1.4, bty = "n", lwd = 2)
par(old.par)
dev.off()

fi###############################################################################



plot(df$date, df$deceased.todate, type = "l",
     xlab = "",
     ylab = "",
     # ylim = c(0,60),
     xlim = as.Date(c("2020-12-01", "2021-01-18")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "purple4")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "week"),
          format = "%d.%m.")

axis(2, las = 2)
lines(df$date, df$deceased.weekly3, col = "purple1", lwd = 2, lty = 5)
lines(df$date, df$deceased.weekly2, col = "purple2", lwd = 2, lty = 5)
lines(df$date, df$deceased.weekly1, col = "purple3", lwd = 2, lty = 5)
lines(df$date, df$state.deceased.todate, col = "blue", lwd = 2, lty = 1)
lines(df$date, df$deceased.todate, col = "purple4", lwd = 2, lty = 1)





plot(df$date, df$week0, type = "l",
     xlab = "",
     ylab = "",
    # ylim = c(0,60),
     xlim = as.Date(c("2020-12-01", "2021-01-18")),
     axes = FALSE, 
     lty = 1, lwd = 2, col = "black")
axis.Date(1, at = seq(df$date[1], df$date[length(df$date)]+10, by = "week"),
          format = "%d.%m.")

axis(2, las = 2)

lines(df$date, df$week1, col = "purple4", lwd = 2, lty = 1)
lines(df$date, df$week2, col = "purple2", lwd = 2, lty = 1)
lines(df$date, df$state.daily, col = "orange", lwd = 2, lty = 5)

legend(as.Date("2020-09-15"), 50,
       legend=rev(c("Število umrlih do 11.11. (NIJZ)", "Število umrlih do 18.11. (NIJZ)",
                    "Število umrlih do 25.11. (NIJZ)", "Število umrlih (vlada)")),
       col=rev(c("blue", "blue4", "black", "red")), lty=rev(c(1,1,1, 5)),
       cex=0.9, bty = "n", lwd = 2)
mtext(side = 3, line = 0.5,  adj = 0, cex = 1.1,
      "Primerjava dnevnega števila umrlih vnešenih v NIJZ tabele na različne dni in vladnih podatkov (rdeča)")

# mtext(side = 3, line = 1,  adj = 0, cex = 1.1,
#       "in vladnih podatkov (rdeča)")
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")



## vlada vs nijz part 2 17.2.

nijz.week1 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly1")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) -> nijz.weekly1

nijz.week2 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly2")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) -> nijz.weekly2

nijz.week3 %>% 
  `colnames<-`(c("date", "x", "xx", "deceased.weekly3")) %>% 
  select(-x, -xx) %>% 
  mutate(date  = as.Date(date, "%d.%m.%Y")) -> nijz.weekly3



png(filename="figures/nijzdaily.png", 700, 420)
l <- 2
plot(df$date, df$week0,
     type = "l",
     ylim = c(0, 45),
     axes = FALSE,
     lwd = l,
     col = "#581A5D",
     xlab = "",
     ylab = "",
     xlim = as.Date(c("2021-01-01", "2021-02-18")))
lines(df$date, df$week2, lwd = l,  col = "#581A5D", lty = "63")
lines(df$date, df$week1, lwd = l, col = "#581A5D", lty = "43")

axis(2, las = 2, cex.axis = 1 )
axis.Date(1, at = seq(df$date[1]+6, df$date[length(df$date)], by = "week"),
          format = "%d.%m.", cex.axis = 1)
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
mtext(side = 3, line = 0.5,  adj = 0, cex = 1.1,
      "Dnevno število umrlih v treh zaporednih tedenskih poročilih NIJZ")

legend(as.Date("2021-02-1"), 40,
       legend=c("NIJZ poročilo 15.2.21",
                    "NIJZ poročilo 8.2.21",
                    "NIJZ poročilo 1.2.21"),
       col="#581A5D", lty=c("91", "63", "43"),
       cex=0.9, bty = "n", lwd = l)

dev.off()


png(filename="figures/nijzdailyplusgvmnt.png", 700, 420)
l <- 2
plot(df$date, df$week0,
     type = "l",
     ylim = c(0, 45),
     axes = FALSE,
     lwd = l,
     col = "#581A5D",
     xlab = "",
     ylab = "",
     xlim = as.Date(c("2021-01-01", "2021-02-18")))
lines(df$date, df$week2, lwd = l,  col = "#581A5D", lty = "63")
lines(df$date, df$week1, lwd = l, col = "#581A5D", lty = "43")
lines(df$date, df$state.daily, col = "red", lwd = 2)
axis(2, las = 2, cex.axis = 1 )
axis.Date(1, at = seq(df$date[1]+6, df$date[length(df$date)], by = "week"),
          format = "%d.%m.", cex.axis = 1)
mtext(side = 2, line = 3,   cex = 1,"dnevno število umrlih")
mtext(side = 1, line = 2.5,   cex = 1, "datum")
mtext(side = 3, line = 0.5,  adj = 0, cex = 1.1,
      "Dnevno število umrlih v treh zaporednih tedenskih poročilih NIJZ")

legend(as.Date("2021-02-1"), 40,
       legend=c("NIJZ poročilo 15.2.21",
                "NIJZ poročilo 8.2.21",
                "NIJZ poročilo 1.2.21",
                "Vladno poročanje"),
       col=c("#581A5D", "#581A5D", "#581A5D", "red"), lty=c("91", "63", "43", "91"),
       cex=0.9, bty = "n", lwd = l)

dev.off()

