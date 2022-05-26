cou <- read.csv("county_statistics.csv", sep = ",", header = TRUE)
#attach(cou)
#View(cou)
cou2 <- na.omit(cou)
attach(cou2)
#View(cou2)
rownames(cou2) <- 1:nrow(cou2)
plot(percentage20_Donald_Trump, lat)
hist(lat[votes20_Donald_Trump], freq = TRUE)
lines(density(lat[votes20_Donald_Trump]))

library(tidyverse)
library(maps)
#install.packages("mapproj")
library(png)
library(grid)
library(cowplot)
library(statebins)
library(DT)

options(repr.plot.width = 17, repr.plot.height = 9)

# WORLD MAP
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon()

# US MAP 
usa <- map_data("state")
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

abb <- data.frame(state = state.name, Abb = state.abb)

# TOTAL VOTES
election <- cou2 %>% 
  group_by(state) %>% 
  summarise(votes16_Hillary_Clinton = sum(votes16_Hillary_Clinton),
            votes16_Donald_Trump = sum(votes16_Donald_Trump)) %>% 
  ungroup() %>% 
  gather(Candidate, Votes, -state) %>% 
  mutate(Party = factor(if_else(Candidate == "Clinton", "Democrat", "Republican"))) %>% 
  left_join(abb) %>% 
  mutate(state = tolower(state)) %>% 
  rename("region" = state)

election2 <- election %>% 
  group_by(region, Abb) %>% 
  summarise(Votes = max(Votes)) %>% 
  ungroup() %>% 
  inner_join(election)

# Left Join for creating map
usa_election <- left_join(usa, election2) %>% 
  filter(!is.na(Party))


# First Map
p <- ggplot(data = usa_election, aes(x = long, y = lat))+
  geom_polygon(aes(group = group, fill = Party),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p
colnames(cou2)


plot(Hispanic)
hist(Income[percentage20_Donald_Trump>0.5])
hist(Income[percentage20_Donald_Trump<0.5])

q=c(percentage20_Joe_Biden>0.5)

ggplot(cou2,aes(x=long,group=q,fill=q))+
  +     geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

ggplot(cou2,aes(x=cou2$Asian, group=q,fill=q))+
  geom_histogram(position="identity",alpha=0.5,binwidth=1)+theme_bw()
