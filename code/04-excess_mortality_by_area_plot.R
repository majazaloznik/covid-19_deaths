###############################################################################
# preliminaries
###############################################################################
library(readr)
library(sf)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(tmap)
library(rgdal)
library(classInt)
# devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

###############################################################################
# metadata
###############################################################################

# shape files 
# https://www.e-prostor.gov.si/fileadmin/struktura/preberi_me.pdf
# source
# "Register prostorskih enot, Geodetska uprava Republike Slovenije, SHP podatki, 21.12.2020"
# https://egp.gu.gov.si/egp/

# population by region over 2015-2020
# https://pxweb.stat.si:443/SiStatData/sq/2822


###############################################################################
# import and prep data
###############################################################################
dso <- read_csv("data/dict-retirement_homes.csv")

# import data
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/daily_deaths_slovenia_by_age_sex_and_admin_area.csv"),
              "data/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")

df <- read_csv("data/daily_deaths_slovenia_by_age_sex_and_admin_area.csv")

# get population data for regions and ues
download.file("https://pxweb.stat.si:443/SiStatData/sq/2822", "data/pop_region.csv")
pop.reg <- read_csv("data/pop_region.csv", 
                    locale = locale(encoding = "Windows-1250"),
                    skip = 1)
download.file("https://pxweb.stat.si:443/SiStatData/sq/2824", "data/pop_ue.csv")
pop.ue <- read_csv("data/pop_ue.csv", 
                    locale = locale(encoding = "Windows-1250"),
                    skip = 1)
pop.ue %>% 
  mutate(`UPRAVNA ENOTA` = ifelse(`UPRAVNA ENOTA` == "Piran/Pirano", "Piran", 
                                  ifelse(`UPRAVNA ENOTA` == "Koper/Capodistria", "Koper",
                                         ifelse(`UPRAVNA ENOTA` == "Izola/Isola", "Izola",
                                                ifelse(`UPRAVNA ENOTA` == "Lendava/Lendva",
                                                       "Lendava",`UPRAVNA ENOTA`))))) -> pop.ue
                                                       
                                                

## regioal analysis 
# aggregate to annual data
df %>% 
  mutate(year = year(date)) %>% 
  group_by(region.name, year) %>% 
  select(year, region.name, deceased) %>% 
  summarise(deceased = sum(deceased)) -> df.agg
  

# get average for 2015-2019
df.agg %>% 
  filter(year > 2014 & year <2020) %>% 
  group_by(region.name) %>% 
  summarise(average = mean(deceased)) -> means.5y

# merge with 2020 data
df.agg %>% 
  filter(year == 2020) %>% 
  select(-year) %>% 
  right_join(means.5y) %>% 
  mutate(excess = (deceased / average -1 )*100) -> excess


# plot regions

reg <- st_read("data/SR/SR.shp")
left_join(reg, excess, by = c("SR_UIME" = "region.name")) -> reg


png("figures/excess_by_region.png", width = 1400, height = 800)
ggplot() +
  geom_sf(data=reg, aes(fill = excess)) + 
  theme_void() +
scale_fill_gradient(low="#D0A9F5", high="#581A5D", name="% Presežnih smrti") +
  labs(fill="% Presežnih smrti", 
       title="Delež presežnih smrti po statističnih regijah za 2020", 
       subtitle="v primerjavi s povprečjem 2015-2019, Vir: CRP (MNZ)") +
  theme(
    plot.title = element_text(hjust = 0.1, 
                              size=22,
                              margin=margin(t = 20, unit = "pt")),
    plot.subtitle = element_text(hjust = 0.1, 
                                 size=16,
                                 vjust = 0.5),
    legend.position = c(0.9,0.3), 
    legend.text=element_text(size=14),
    legend.title=element_text(size=14)) 
dev.off()

## 
## UE analysis 
# aggregate to annual data
df %>% 
  mutate(year = year(date)) %>% 
  group_by(ue.name, year) %>% 
  select(year, ue.name, deceased) %>% 
  summarise(deceased = sum(deceased)) -> df.ue.agg


# get average for 2015-2019
df.ue.agg %>% 
  filter(year > 2014 & year <2020) %>% 
  group_by(ue.name) %>% 
  summarise(average = mean(deceased)) -> means.ue.5y

# merge with 2020 data
df.ue.agg %>% 
  filter(year == 2020) %>% 
  select(-year) %>% 
  right_join(means.ue.5y) %>% 
  mutate(excess = (deceased / average -1 )*100) -> excess.ue


# plot UEs

ue <- st_read("data/UE/UE.shp")
left_join(ue, excess.ue, by = c("UE_UIME" = "ue.name")) -> ue

png("figures/excess_by_ue.png", width = 1400, height = 800)

ggplot() +
  geom_sf(data=ue, aes(fill = excess)) + 
  theme_void() +
  scale_fill_gradient2(low="#00bda7", mid = "white", high="#581A5D", midpoint=0,name="% Presežnih smrti") +
  labs(fill="% Presežnih smrti", 
       title="Delež presežnih smrti po upravnih enotah za 2020", 
       subtitle="v primerjavi s povprečjem 2015-2019, Vir: CRP (MNZ)") +
  theme(
    plot.title = element_text(hjust = 0.1, 
                              size=22,
                              margin=margin(t = 20, unit = "pt")),
    plot.subtitle = element_text(hjust = 0.1, 
                                 size=16,
                                 vjust = 0.5),
    legend.position = c(0.9,0.3), 
    legend.text=element_text(size=14),
    legend.title=element_text(size=14)) 
dev.off()


write_csv(excess.ue, "outputs/excess_by_UE.csv")
write_csv(excess, "outputs/excess_by_region.csv")


###############################################################################
# using crude death rates regions

pop.reg %>% 
  select(-SPOL) %>% 
  rename("region.name" = `STATISTIČNA REGIJA`) %>% 
  #filter(region.name !="SLOVENIJA") %>% 
  gather(year, pop, 2:7) %>% 
  separate(year, into = c("year", "x", "xx")) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-x, -xx) -> pop.reg.long
  
df.agg %>% 
  filter(year > 2014 & year <2021) %>% 
  group_by(year) %>% 
  summarise(deceased = sum(deceased)) %>% 
  mutate(region.name = "SLOVENIJA") %>% 
 bind_rows(df.agg %>%
             filter(year > 2014 & year <2021)) %>% 
  left_join(pop.reg.long) %>%
  mutate(cdr = deceased/pop * 1000) -> reg.cdr

# get 5 year average
reg.cdr %>% 
  filter(year != 2020) %>% 
  group_by(region.name) %>% 
  summarise(cdr.average = mean(cdr)) -> reg.cdr.5y

reg.cdr %>% 
  filter(year == 2020) %>% 
  select(region.name, cdr) %>% 
  left_join(reg.cdr.5y) %>% 
  arrange(cdr.average) %>% 
  gather(measure, cdr, 2:3) %>% 
  mutate(cdr = round(cdr,1)) %>% 
  mutate(measure = factor(measure,
                          levels = c( "cdr.average", "cdr"), 
                          labels = c("2015-19", "2020"), ordered = TRUE))  -> reg.cdr.long
write_csv(reg.cdr.long, "data/regions_cdr.csv")
png("figures/cdr_slope_reg.png", width = 1400, height = 800)
newggslopegraph(reg.cdr.long, 
                measure, cdr,
                region.name,
                Title = "Stopnja umrljivosti na 1,000 prebivalcev po regijah",
                Caption = "Vir podatkov: CRP (MNZ)",
                SubTitle = "Leto 2020 v primerjavi s 5-letnim povprečjem 2015-19",
                XTextSize = 18,
                YTextSize = 6,
                DataTextSize = 6,
                TitleTextSize = 22,
                SubTitleTextSize = 16,
                CaptionTextSize = 14)
dev.off()

###############################################################################
# using crude death rates regional map

reg.cdr %>% 
  filter(year == 2020) %>% 
  select(region.name, cdr) %>% 
  left_join(reg.cdr.5y) %>% 
  mutate(cdr.excess = (cdr/cdr.average -1) * 100) %>% 
  arrange(desc(cdr.excess)) -> reg.cdr.map

left_join(reg, reg.cdr.map, by = c("SR_UIME" = "region.name")) -> reg


reg$nudge_x <- 0
reg$nudge_y <- 0
reg$nudge_x[reg$SR_UIME == "Goriška"] <- 15000
reg$nudge_y[reg$SR_UIME == "Goriška"] <- 15000
reg$nudge_y[reg$SR_UIME == "Savinjska"] <- 5000
reg$nudge_y[reg$SR_UIME == "Savinjska"] <- 10000

reg$nudge_x[reg$SR_UIME == "Gorenjska"] <- -15000
reg$nudge_x[reg$SR_UIME == "Podravska"] <- 10000
reg$nudge_y[reg$SR_UIME == "Podravska"] <- -10000
reg$nudge_x[reg$SR_UIME == "Zasavska"] <- -5000
reg$nudge_y[reg$SR_UIME == "Zasavska"] <- -5000
reg$nudge_x[reg$SR_UIME == "Posavska"] <- 15000
reg$nudge_y[reg$SR_UIME == "Posavska"] <- -5000

reg$nudge_x[reg$SR_UIME == "Pomurska"] <- 5000
reg$nudge_x[reg$SR_UIME == "Jugovzhodna Slovenija"] <- -5000
reg$nudge_y[reg$SR_UIME == "Jugovzhodna Slovenija"] <- -10000

reg$nudge_x[reg$SR_UIME == "Obalno-kraška"] <- 15000
reg$nudge_y[reg$SR_UIME == "Obalno-kraška"] <- 0
reg$nudge_x[reg$SR_UIME == "Primorsko-notranjska"] <- 5000
reg$nudge_y[reg$SR_UIME == "Primorsko-notranjska"] <- -15000

png("figures/excess_cdr_by_region.png", width = 1400, height = 800)
ggplot() +
  geom_sf(data=reg, aes(fill = cdr.excess)) + 
  theme_void() +
  scale_fill_gradient(low="#D0A9F5", high="#581A5D", name="Povečanje stopnje \numrljivosti (%)") +
  labs(fill="% Presežnih smrti", 
       title="Sprememba v stopnji umrljivosti po statističnih regijah za 2020", 
       subtitle="v primerjavi s povprečjem 2015–2019, Vir: CRP (MNZ)") +
  theme(
    plot.title = element_text(hjust = 0.1, 
                              size=22,
                              margin=margin(t = 20, unit = "pt")),
    plot.subtitle = element_text(hjust = 0.1, 
                                 size=16,
                                 vjust = 0.5),
    legend.position = c(0.9,0.3), 
    legend.text=element_text(size=14),
    legend.title=element_text(size=14)) +
  geom_text(data = reg, aes(CEN_E, CEN_N, label = paste(round(cdr.excess,1), "%")), 
            size = 6, nudge_x = reg$nudge_x, nudge_y = reg$nudge_y) 


dev.off()


###############################################################################
# using crude death rates UEs

pop.ue %>% 
  select(-SPOL) %>% 
  rename("ue.name" = `UPRAVNA ENOTA`) %>% 
  filter(ue.name !="SLOVENIJA") %>% 
  gather(year, pop, 2:13) %>% 
  separate(year, sep = c(4,6),into = c("year", "H", "x"))%>% 
  select(-x) %>% 
  group_by(ue.name, year) %>% 
  summarise(pop = mean(pop)) %>% 
  mutate(year = as.numeric(year)) -> pop.ue.long

df.ue.agg %>% 
  filter(year > 2014 & year <2021) %>% 
  left_join(pop.ue.long) %>% 
  mutate(cdr = deceased/pop * 100000) -> ue.cdr

# get 5 year average
ue.cdr %>% 
  filter(year != 2020) %>% 
  group_by(ue.name) %>% 
  summarise(cdr.average = mean(cdr)) -> ue.cdr.5y

ue.cdr %>% 
  filter(year == 2020) %>% 
  select(ue.name, cdr) %>% 
  left_join(ue.cdr.5y) %>% 
  mutate(cdr.abs.diff  = cdr-cdr.average,
         cdr.excess = (cdr/cdr.average -1)*100) %>% 
  arrange(desc(cdr.excess)) -> ue.cdr.long




left_join(ue, ue.cdr.long, by = c("UE_UIME" = "ue.name")) -> ue

png("figures/cdr_by_ue.png", width = 1400, height = 800)
ggplot() +
  geom_sf(data=ue, aes(fill = cdr.excess)) + 
  theme_void() +
  scale_fill_gradient(low="#D0A9F5", high="#581A5D", name="Povečanje stopnje \numrljivosti (%)") +
  labs(fill="% Presežnih smrti", 
       title="Sprememba v stopnji umrljivosti po statističnih regijah za 2020", 
       subtitle="v primerjavi s povprečjem 2015-2019, Vir: CRP (MNZ)") +
  theme(
    plot.title = element_text(hjust = 0.1, 
                              size=22,
                              margin=margin(t = 20, unit = "pt")),
    plot.subtitle = element_text(hjust = 0.1, 
                                 size=16,
                                 vjust = 0.5),
    legend.position = c(0.9,0.3), 
    legend.text=element_text(size=14),
    legend.title=element_text(size=14)) 
dev.off()











#### old stuff

ggplot() +
  geom_sf(data=reg, aes(fill = excess)) + 
  theme_void() +
  scale_fill_gradient2(low="green", mid="white", high="red", #colors in the scale
                       midpoint=0,    #same midpoint for plots (mean of the range)
                       breaks=seq(-100,500,50), #breaks in the scale bar
                       limits=c(floor(rng[1]), ceiling(rng[2]))) +
  labs(fill="% Presežnih smrti", 
       title="Delež presežnih smrti po upravnih enotah za mesec {current_frame} 2020", 
       subtitle="v primerjavi s povprečjem 2015-2019, Vir: CRP (MNZ)") +
  theme(
    plot.title = element_text(hjust = 0.1),
    plot.subtitle = element_text(hjust = 0.1)) +
  geom_point(data = dso, aes(y = "geo-latitude", x = "geo-longitude")) +
  transition_manual(month) -> p
animate(p, duration = 10, fps = 15, width = 600, height = 400, 
        renderer = gifski_renderer(),end_pause = 5)
anim_save("output.gif")

ggplot() +
  geom_sf(data=ue, aes(fill = excess)) -> p
p + geom_point(data=dso, aes(y = "geo-latitude", x = "geo-longitude"), color="red", size=30, alpha=0.5)



ggplot(ue, aes(x=excess, fill = excess)) + 
  theme_minimal() +
  geom_histogram() +
  transition_manual(month) +
  geom_vline(xintercept = 0) +
  labs(fill="% Presežnih smrti", 
       title="Delež presežnih smrti po upravnih enotah za mesec {current_frame} 2020", 
       subtitle="v primerjavi s povprečjem 2015-2019, Vir: CRP (MNZ)") -> h
animate(h, duration = 10, fps = 15, width = 600, height = 400, 
        renderer = gifski_renderer(),end_pause = 5)
anim_save("hist.gif")



###############################################################################
# base graphics plot
###############################################################################

# prepare for plotting in base
shp <- readOGR(dsn = "data/UE", layer = "UE")

excess %>% 
  select(month, area, excess) %>% 
  spread(month, excess) -> df

shp@data <- left_join(shp@data, df , by = c("UE_UIME" = "area")) 


FunPalette <- function (x, bin.n=11, col.scheme="Spectral", style="pretty"){
  bins <- classIntervals(x, n=bin.n, style=style)
  brbg <- brewer.pal(11,  col.scheme)
  col <- c(colorRampPalette(c(brbg[4], brbg[6]))(2), 
            colorRampPalette(c(brbg[6], brbg[11]))(8)[-1])
  pal <- brewer.pal(length(bins$brks)-1, col.scheme)
  col <- findColours(bins, pal)
  pal.names <- paste(round(bins$brks[-length(bins$brks)],2), 
                     round(bins$brks[-1], 2), sep= " - ")
  pal.n.from <- round(bins$brks[-length(bins$brks)],2)
  pal.n.to <- round(bins$brks[-1],2)
  return(list(col, pal, pal.names, pal.n.from, pal.n.to, bins$brks, bins))
}

FunLegend <- function(palette=FunPalette){
  plot(c(0,1), c(0,1),  col = NA, ann = FALSE, axes = FALSE)
  points(rep(0.3,length(palette[[2]])), 1/length(palette[[2]])*
           seq(1, length(palette[[2]])),pch=15, col=palette[[2]], cex=2)   
  points(rep(0.3,length(palette[[2]])),1/length(palette[[2]])*
           seq(1, length(palette[[2]])),pch=0, lwd=1, cex=2)
  text(rep(0.6,length(palette[[2]])), 1/length(palette[[2]])*
         seq(1, length(palette[[2]])), palette[[3]])
}


ii <- cut(shp@data$januar, breaks =pal[[7]], max(values), len = 100, include.lowest = TRUE)

## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
colors <- colorRampPalette(c("lightblue", "blue"))(99)[ii]

pal <- FunPalette(unlist(shp@data[11:22]))
oldpar <- par(no.readonly=TRUE)
par(mar=c(2,0,0,4)) 
plot(shp, col = findColours(pal[[7]], pal[[2]]))

leftX <- 0.65 # position variable density distribution
rightX <- 1
bottomY <- 0.05
par(fig=c(leftX,rightX,bottomY, bottomY+rightX-leftX), new =TRUE,mar=c(2,1,0,0))

plot(density(shp@data$januar, na.rm=TRUE), 
     axes=FALSE, ann=FALSE, lwd=2,
     xlim = range(pal[[6]]))
axis(1)
abline(v=pal[[6]], col="red", lty=2) 

bottomY <- bottomY+rightX-leftX # position legend above dens.
par(fig=c(leftX+0.15,rightX,bottomY, bottomY+rightX-leftX),mar=c(0,0,0,0), new =TRUE)
FunLegend(pal)
par(oldpar)

