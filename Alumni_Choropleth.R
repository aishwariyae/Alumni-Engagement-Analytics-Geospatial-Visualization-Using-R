library(tidyverse)
library(gganimate)
library(lubridate)
library(sf)
library(cartogram)
library(rayshader)
library(rgl)
library(viridis)
library(DT)
library(plotly)
library(maps)
Alumni_Data <- read.csv("Alumni_Data2.csv")
usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>% 
  sf::st_transform(crs=4236)
usa
Alumni_Data %>% mutate(ID=tolower(State)) -> Alumni_Data
alum_map_data <- left_join(x=usa, y=Alumni_Data, by="ID")
ggplot(alum_map_data) + geom_sf(aes(fill=Alumni_count)) + 
  scale_fill_viridis(option="viridis") + 
  labs(title=paste("Alumni per State")) + theme_bw()



