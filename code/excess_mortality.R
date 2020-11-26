###############################################################################
# preliminaries
###############################################################################
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate) 

###############################################################################
#### import data ##############################################################

monthly <- readr::read_table2("data/monthly_deaths_slovenia_1977_2020.txt",
                              col_types = c(year = col_number(),
                                            month = col_number(),
                                            value = col_number()))

st <- as.Date("1977-01-01")
en <- as.Date("2020-10-01")
ll <- seq(en, st, by = "-1 month")
t<-days_in_month(ll)

monthly %>% 
  dplyr::mutate(month = as.numeric(month),
                daily  = value / t ) -> monthly


group = c(rep(1, 43), rep(2, 1))
deaths <- ts(monthly$daily, start=c(1977, 1), end=c(2020, 10), frequency=12)


greyz <- rev(paste0("gray", 26:(26+42)))
colz <-c( rev(paste0("gray", 26:(26+42))), "red")


monthly %>% 
  select(-value) %>% 
  spread(year, daily) %>% 
  mutate(Means = rowMeans(.[,2:43])) -> df




#square function
sq<-function(x){
  x^2
}
#inverse square function (square root)
isq<-function(x){
  print(paste("isq",x))  #debug statement
  x<-ifelse(x<0, 0, x)
  sqrt(x)
}

###############################################################################
deaths2  <- window(deaths, start=c(1993, 1), end=c(1995, 1))

ggseasonplot(deaths2,  polar=TRUE) +
  scale_y_continuous(
    limits = c(0, 70),
    trans=scales::trans_new("sq", sq, isq)) 

ggseasonplot(deaths,  polar=TRUE, size = c(rep(1, 43), rep(2, 1))) +
  theme_bw() +
  theme(legend.position = "none")  +
  scale_colour_manual(values=c(rep("gray",43), "red")) 


ggseasonplot(deaths, polar = TRUE) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 80)) +
  theme(legend.position = "none")  +
  scale_colour_manual(values=c(greyz, "red")) 



ggseasonplot(deaths,  polar=TRUE, size = c(rep(1, 43), rep(2, 1))) +
  theme_bw() +
  #geom_line( alpha = c(rep(0.5, 559), rep(1, 10))) +
  # geom_path(size = c(rep(0.5, 559), rep(3, 10))) +
  scale_y_continuous(
    limits = c(0, max(deaths)),
   trans=scales::trans_new("sq", sq, isq)) +
  theme(legend.position = "none")  +
  scale_colour_manual(values=c(greyz, "red")) 

###############################################################################

plot(1:12, rep(0,12), ylim = c(0,80), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "")
axis(1, at = 1:12, labels = month.abb)
axis(2, las =2)

for (i in 2:44){
  lines(1:12, df[[i]] , col = colz[i-1], type = 'l')
}
lines(1:12, df[[45]] , col = colz[44], type = 'l', lwd = 2)
arrows(10, pull(df[10,45]),10, pull(df[10,45])-188/31, col = "blue", lwd = 2, code = 3,
              length=0.07,angle=30)

###############################################################################


plot(1:12, rep(0,12), ylim = c(0,80), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "")
axis(1, at = 1:12, labels = month.abb)
axis(2, las =2)

for (i in 2:44){
  lines(1:12, df[[i]] , col = "gray",  type = 'l')
}
lines(1:12, df[[46]] , col = "gray30", type = 'l', lwd = 3)

arrows(10, pull(df[10,45]),10, pull(df[10,45])-188/31, col = "black", lwd = 1.5, code = 3,
       length=0.07,angle=90)

lines(1:12, df[[45]] , col = colz[44], type = 'l', lwd = 2.5)


      