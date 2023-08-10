setwd("C:\\Users\\carys\\Desktop\\Research Project\\Federal Election Results")
senate = read.csv("..\\Data\\1976-2018-senate.csv")
pres = read.csv("..\\Data\\1976-2016-president.csv")
house = read.csv("..\\Data\\1976-2018-house2.csv")
countyVotes = read.csv("..\\Data\\countypres_2000-2016.csv")
#FIX THIS county$FIPS = as.character(county)
library(tidyverse)
library(ggplot2)
library(tigris)
library(USAboundaries)
library(usmap)
theme_update(plot.title = element_text(hjust = 0.5)) # plot title default will now be changed from left to center
#theme_update(text = element_text(size=20))
#theme_set(theme_gray()) # restores the original ggplot settings

View(countyVotes)
countyVotes$percentVotes = (countyVotes$candidatevotes/countyVotes$totalvotes)*100
countyVotes = filter(countyVotes,countyVotes$party %in% c('democrat','republican'))

newFIPS = countyVotes %>% filter(countyVotes$party == 'democrat') %>% select(countyVotes$FIPS)
diffVotes = countyVotes$candidateVotes[countyVotes$party=='democrat'] - countyVotes$candidateVotes[countyVotes$party=='republican']
plot_usmap(data = county, values = county$percentVotes, color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

