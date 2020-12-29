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

###############################################################################
# metadata
###############################################################################

# shape files 
# https://www.e-prostor.gov.si/fileadmin/struktura/preberi_me.pdf
# source
# "Register prostorskih enot, Geodetska uprava Republike Slovenije, SHP podatki, 21.12.2020"
# https://egp.gu.gov.si/egp/


###############################################################################
# import and prep data
###############################################################################
dso <- read_csv("data/dict-retirement_homes.csv")

# import data
df <- read_csv("outputs/admin_area_monthly_deaths_slovenia.csv")

# get average for 2015-2019
df %>% 
  filter(year > 2014 & year <2020) %>% 
  group_by(month) %>% 
  mutate_at(vars(Ajdovščina:Žalec), ~ (.x / days)) %>% 
  summarise_at(vars(Ajdovščina:Žalec), ~mean(.x, na.rm = TRUE)) %>% 
  gather(area, means, 2:59) %>% 
  arrange(month, area)-> means.5y

# merge with 2020 data
df %>% 
  filter(year > 2019) %>% 
  select(-year) %>% 
  mutate_at(vars(Ajdovščina:Žalec), ~ (.x / days)) %>% 
  gather(area, deceased, 2:59) %>% 
  arrange(month, area) %>% 
  right_join(means.5y) %>% 
  mutate(excess = (deceased / means -1 )*100) -> excess


# prepare for plotting
# https://stackoverflow.com/questions/34092237/applying-dplyrs-rename-to-all-columns-while-using-pipe-operator
month.names <- c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "oktober",
            "november", "december")

excess %>% 
  mutate(month= factor(month, labels = month.names)) -> excess

ue <- st_read("data/UE/UE.shp")
left_join(ue, excess, by = c("UE_UIME" = "area")) -> ue


rng = range(excess$excess)

ggplot() +
  geom_sf(data=ue, aes(fill = excess)) + 
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

