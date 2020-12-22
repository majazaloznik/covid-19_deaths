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


plot_sf(ue)

