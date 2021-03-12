###############################################################################
# preliminaries
###############################################################################
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate) 
library(purrr)
library(ggplot2)
library(viridis)

###############################################################################
#### import data
###############################################################################

# # uncomment to download
# # CRP data on deaths by age and sex
# download.file(paste0("https://raw.githubusercontent.com/",
#                      "sledilnik/data/master/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv"),
#               "data/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")

crp <- read_csv("data/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")

###############################################################################
# clean and join data - monthly
###############################################################################

# aggregate to monthly totals and get 5 year average
crp %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year > 2014 & year < 2020) %>% 
  group_by(year, month, sex, age.group) %>% 
  summarise(deceased = sum(deceased, na.rm = TRUE)) %>% # add into months
  group_by( month, sex, age.group) %>% 
  summarise(deceased.average = mean(deceased, na.rm = TRUE)) %>%  # average between years
  mutate(deceased.average.daily = deceased.average/days_in_month(month)) %>% # get daily number
  select(-deceased.average) -> crp.age.monthly.5y

# get montlhy numbers for 2020
crp %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year > 2019) %>% 
  group_by(month) %>% 
  summarise(days = length(unique(date))) %>% 
  pull(days) -> days.no

crp %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year > 2019) %>% 
  group_by( month, sex, age.group) %>% 
  summarise(deceased = sum(deceased, na.rm = TRUE)) %>% # add into months
  mutate(days.no = days.no[month],
         deceased.current.daily = deceased / days.no) %>% # get daily number
  select(-days.no) -> crp.age.monthly.current

# merge average with current data
crp.age.monthly.5y %>% 
  ungroup() %>% 
  left_join(crp.age.monthly.current %>% 
              select(-deceased))  -> crp.age.compare

# collapse age categories 
age.g3 <- rep(c("0-41","0-41","0-41", "0-41","42-71",
      "42-71","42-71","72+"), 24)
age.g3.alt <- rep(c("0-51","0-51","0-51", "0-51","0-51",
                "52-71","52-71","72+"), 24)
age.g4 <- rep(c("0-18","19-51","19-51", "0-18","19-51",
                "52-71","52-71","72+"), 24)
age.g7 <- rep(c("0-18","19-31","32-41", "0-18","42-51",
                "52-61","62-71","72+"), 24)
crp.age.compare %>% 
  mutate(age.g = age.g3.alt) %>% 
  group_by(month, sex, age.g) %>% 
  summarise(deceased.current.daily = sum(deceased.current.daily, na.rm = TRUE),
            deceased.average.daily = sum(deceased.average.daily, na.rm = TRUE)) -> crp.age.compare.col

# get current monthly deaths for m, f and total - for bar widths
crp.age.compare %>% 
  left_join(crp.age.monthly.current)%>% 
  select(-deceased.current.daily, -deceased.average.daily) %>% 
  mutate(age.g = age.g3.alt)  %>% 
  group_by(month, sex, age.g) %>% 
  summarise(deceased = sum(deceased, na.rm = TRUE)) %>% 
  spread(sex, deceased) %>% 
  mutate(T = F + M) -> current.df
  
pal <- viridis(n=3)
# calculate excess for each age group
crp.age.compare.col %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c( month, age.g), 
              names_from = sex, 
              values_from = c("deceased.current.daily", "deceased.average.daily"),
              names_sep = ".") %>% 
  mutate(deceased.average.daily.T = deceased.average.daily.F + deceased.average.daily.M,
         deceased.current.daily.T = deceased.current.daily.F + deceased.current.daily.M,
         excess.T =  (deceased.current.daily.T / deceased.average.daily.T - 1 ) * 100 ,
         excess.F =  (deceased.current.daily.F / deceased.average.daily.F - 1 ) * 100 ,
         excess.M =  (deceased.current.daily.M / deceased.average.daily.M - 1 ) * 100 ) %>% 
  select(month, age.g, excess.F, excess.M, excess.T) -> excess.df

###############################################################################
#### line charts
###############################################################################

png(filename="figures/monthly.excess-15-19-baseline-by_3_age_groups-alt.png", 800, 480)
ggplot(excess.df) +
  theme_minimal() +
  geom_line(aes(x = month, y = excess.T, group = age.g, colour = age.g))+
  guides(colour = guide_legend(reverse=T)) + 
  scale_colour_manual(values=pal) +
  labs(title = "Excess deaths in Slovenia, 2020",
       subtitle = "Compared to average 2015-2019") +  
  scale_x_continuous(breaks=1:12, labels = month.abb) +
  labs(colour = "Age group")
dev.off()

ggplot(excess.df) +
  theme_minimal() +
  geom_line(aes(x = month, y = excess.F, group = age.g, colour = age.g))+
  guides(colour = guide_legend(reverse=T)) + 
  scale_colour_manual(values=pal) +
  scale_y_continuous(name = "Excess deaths", limits = c(-100, 100)) + 
  labs(title = "Excess deaths for women in Slovenia, 2020",
       subtitle = "Compared to average 2015-2019") + 
  scale_x_continuous(breaks=1:12, labels = month.abb) +
  labs(colour = "Age group")


ggplot(excess.df) +
  theme_minimal() +
  geom_line(aes(x = month, y = excess.T, group = age.g, colour = age.g))+
  guides(colour = guide_legend(reverse=T)) + 
  scale_colour_manual(values=pal) +
  scale_y_continuous(name = "Excess deaths", limits = c(-100, 100)) + 
  labs(title = "Excess deaths for men in Slovenia, 2020",
  subtitle = "Compared to average 2015-2019") +
  scale_x_continuous(breaks=1:12, labels = month.abb) + 
  labs(colour = "Age group")


###############################################################################
####  bar charts
###############################################################################

excess.df %>% 
  select(month, age.g, excess.T) %>% 
  spread(age.g, excess.T) %>% 
  select(-month) %>% 
  as.matrix () %>% 
  t() -> mat.T
  
widths <- current.df$T
spaces.btw <- mean(widths)/300
spaces.all <- c(rep(0,3), rep(c(spaces.btw, rep(0,2)), 11))

png(filename="figures/monthly.excess-15-19-baseline-by_7_age_groups-barplot.png", 800, 480)
old.par <- par(mar = c(0, 3, 4, 6), xpd = TRUE)
barplot(c(mat.T), beside = TRUE,
        width = widths ,
        space = spaces.all,
        col = pal,
        border = pal,
        axes = FALSE, ylim = c(-60, 100))
axis(2, las = 2)
mtext(side = 3, line = 2.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to 2015-19 baseline by age group")
mtext(side = 3, line = 1.5,  adj = 0, cex = 0.9,
      "(widths of bars are proportional to number of deaths in 2020)")
legend(920, 95, legend=rev(unique(excess.df$age.g)),
       fill=rev(pal), border = rev(pal), bty = "n", title = "Age group")
par(old.par)
dev.off()




excess.df %>% 
  select(month, age.g, excess.F) %>% 
  spread(age.g, excess.F) %>% 
  select(-month) %>% 
  as.matrix () %>% 
  t() -> mat.F

widths <- current.df$F
spaces.btw <- mean(widths)/300
spaces.all <- c(rep(0,3), rep(c(spaces.btw, rep(0,2)), 11))

png(filename="figures/monthly.excess-15-19-baseline-by_7_age_groups-barplot-F.png", 800, 480)
old.par <- par(mar = c(0, 3, 4, 6), xpd = TRUE)
barplot(c(mat.F), beside = TRUE,
        width = widths ,
        space = spaces.all,
        col = pal,
        border = pal,
        axes = FALSE, ylim = c(-60, 100))
axis(2, las = 2)
mtext(side = 3, line = 2.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to 2015-19 baseline by age group: Women")
mtext(side = 3, line = 1.5,  adj = 0, cex = 0.9,
      "(widths of bars are proportional to number of deaths in 2020)")
legend(490, 95, legend=rev(unique(excess.df$age.g)),
       fill=rev(pal), border = rev(pal), bty = "n", title = "Age group")
par(old.par)
dev.off()


excess.df %>% 
  select(month, age.g, excess.M) %>% 
  spread(age.g, excess.M) %>% 
  select(-month) %>% 
  as.matrix () %>% 
  t() -> mat.M

widths <- current.df$M
spaces.btw <- mean(widths)/300
spaces.all <- c(rep(0,3), rep(c(spaces.btw, rep(0,2)), 11))

png(filename="figures/monthly.excess-15-19-baseline-by_7_age_groups-barplot-M.png", 800, 480)
old.par <- par(mar = c(0, 3, 4, 6), xpd = TRUE)
barplot(mat.M, beside = TRUE,
        width = widths ,
        space = spaces.all,
        col = pal,
        border = pal,
        axes = FALSE, ylim = c(-60, 100))
axis(2, las = 2)
mtext(side = 3, line = 2.5,  adj = 0, cex = 1.1,
      "Monthly excess mortality relative to 2015-19 baseline by age group: Men")
mtext(side = 3, line = 1.5,  adj = 0, cex = 0.9,
      "(widths of bars are proportional to number of deaths in 2020)")
legend(490, 95, legend=rev(unique(excess.df$age.g)),
       fill=rev(pal), border = rev(pal), bty = "n", title = "Age group")
par(old.par)
dev.off()


###############################################################################
#### absolute line charts 
###############################################################################
year <- 2010:2020
month <- 1:12
sex <- c("M","F")
age.group <- unique(crp$age.group)
expand.grid(year,month,sex, age.group) %>% 
  `colnames<-`(c("year", "month", "sex", "age.group")) -> grid

# collapse into monthly data for Slovenia, add missing comninaitons, collapse into 3 age groups
# then add total for M&F and turn into long table
crp %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month, sex, age.group) %>% 
  summarise(deceased = sum(deceased, na.rm = TRUE)) %>% 
  right_join(grid) %>% 
  arrange(year, month, sex, age.group) %>% 
  ungroup() %>% 
  mutate(age.g =  rep(c("0-51","0-51","0-51", "0-51","0-51",
                                     "52-71","52-71","72+"), 11*12*2)) %>% 
  group_by(year, month, sex, age.g) %>% 
  summarise(deceased = sum(deceased, na.rm = TRUE)) %>% 
  spread(sex, deceased) %>% 
  mutate(T = M + F) %>% 
  gather(sex, deceased, 4:6) -> absolute.df


png(filename="figures/monthly_absolute_deaths_by_3_age_groups-alt.png", 800, 480)
ggplot(subset(absolute.df, sex == "T")) +
  theme_minimal() +
  geom_line(aes(x = month, y = deceased, group = interaction(year, age.g), colour = age.g))+
  guides(colour = guide_legend(reverse=T)) + 
  scale_colour_manual(values=pal) +
  labs(title = "",
       subtitle = "") +  
  scale_x_continuous(breaks=1:12, labels = month.abb) +
  labs(colour = "Age group")
dev.off()


heatmap(mat.M, Colv = NA, Rowv = NA)
heatmap(mat.F, Colv = NA, Rowv = NA)
heatmap(mat.T, Colv = NA, Rowv = NA)

