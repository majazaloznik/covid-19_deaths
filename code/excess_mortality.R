###############################################################################
# preliminaries
###############################################################################
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 
# library(ggplot2)
# library(forecast)

###############################################################################
#### import data ##############################################################

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




# 
# 
# 
# ## old charts
# 
# 
# group = c(rep(1, 43), rep(2, 1))
# deaths <- ts(monthly$daily, start=c(1977, 1), end=c(2020, 10), frequency=12)
# 
# 
# greyz <- rev(paste0("gray", 26:(26+42)))
# colz <-c( rev(paste0("gray", 26:(26+42))), "red")
# 
# 
# monthly %>% 
#   select(-value) %>% 
#   spread(year, daily) %>% 
#   mutate(Means = rowMeans(.[,2:43])) -> df
# 
# 
# 
# 
# #square function
# sq<-function(x){
#   x^2
# }
# #inverse square function (square root)
# isq<-function(x){
#   print(paste("isq",x))  #debug statement
#   x<-ifelse(x<0, 0, x)
#   sqrt(x)
# }
# 
# ###############################################################################
# deaths2  <- window(deaths, start=c(1993, 1), end=c(1995, 1))
# 
# ggseasonplot(deaths2,  polar=TRUE) +
#   scale_y_continuous(
#     limits = c(0, 70),
#     trans=scales::trans_new("sq", sq, isq)) 
# 
# ggseasonplot(deaths,  polar=TRUE, size = c(rep(1, 43), rep(2, 1))) +
#   theme_bw() +
#   theme(legend.position = "none")  +
#   scale_colour_manual(values=c(rep("gray",43), "red")) 
# 
# 
# ggseasonplot(deaths, polar = TRUE) +
#   theme_bw() +
#   scale_y_continuous(
#     limits = c(0, 80)) +
#   theme(legend.position = "none")  +
#   scale_colour_manual(values=c(greyz, "red")) 
# 
# 
# 
# ggseasonplot(deaths,  polar=TRUE, size = c(rep(1, 43), rep(2, 1))) +
#   theme_bw() +
#   #geom_line( alpha = c(rep(0.5, 559), rep(1, 10))) +
#   # geom_path(size = c(rep(0.5, 559), rep(3, 10))) +
#   scale_y_continuous(
#     limits = c(0, max(deaths)),
#    trans=scales::trans_new("sq", sq, isq)) +
#   theme(legend.position = "none")  +
#   scale_colour_manual(values=c(greyz, "red")) 
# 
# ###############################################################################
# 
# plot(1:12, rep(0,12), ylim = c(0,80), type = "n",
#      axes = FALSE,
#      xlab = "",
#      ylab = "")
# axis(1, at = 1:12, labels = month.abb)
# axis(2, las =2)
# 
# for (i in 2:44){
#   lines(1:12, df[[i]] , col = colz[i-1], type = 'l')
# }
# lines(1:12, df[[45]] , col = colz[44], type = 'l', lwd = 2)
# arrows(10, pull(df[10,45]),10, pull(df[10,45])-188/31, col = "blue", lwd = 2, code = 3,
#               length=0.07,angle=30)
# 
# ###############################################################################
# 
# 
# plot(1:12, rep(0,12), ylim = c(0,80), type = "n",
#      axes = FALSE,
#      xlab = "",
#      ylab = "")
# axis(1, at = 1:12, labels = month.abb)
# axis(2, las =2)
# 
# for (i in 2:44){
#   lines(1:12, df[[i]] , col = "gray",  type = 'l')
# }
# lines(1:12, df[[46]] , col = "gray30", type = 'l', lwd = 3)
# 
# arrows(10, pull(df[10,45]),10, pull(df[10,45])-188/31, col = "black", lwd = 1.5, code = 3,
#        length=0.07,angle=90)
# 
# lines(1:12, df[[45]] , col = colz[44], type = 'l', lwd = 2.5)
# 
# 
#       