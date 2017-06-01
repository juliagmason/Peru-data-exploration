### Manipulate and clean capture species of Peru trips ###
# Julia G Mason
# Nov 1 2016

library(stringr)
library(tidyverse)

# read in trips
trips <- read.csv("trips_main.csv")
new.trips <- read.csv ("trips_sep8.csv") %>% filter (!Trip.Code %in% trips$Trip.code) # newer dataset
colnames(new.trips) <- colnames(trips) # some capitals were changed
trips <- rbind (trips, new.trips)
trips$Fecha.de.zarpe <- as.Date(trips$Fecha.de.zarpe, format = "%m/%d/%y")

# subset catch spp
capture.spp <- select (trips, capture.species.1:Quantity.Units.7)

# look at unique spp
sort (unique (capture.spp$capture.species.1))
sort (unique (capture.spp$capture.species.2 [which (!capture.spp$capture.species.2 %in% capture.spp$capture.species.1)]))
sort (unique (capture.spp$capture.species.3 [which (!capture.spp$capture.species.3 %in% capture.spp$capture.species.1 & !capture.spp$capture.species.3 %in% capture.spp$capture.species.2)]))
sort (unique (capture.spp$capture.species.4 [which (!capture.spp$capture.species.4 %in% capture.spp$capture.species.1 & !capture.spp$capture.species.4 %in% capture.spp$capture.species.2 & !capture.spp$capture.species.4 %in% capture.spp$capture.species.3)]))
sort (unique (capture.spp$capture.species.5 [which (!capture.spp$capture.species.5 %in% capture.spp$capture.species.1 & !capture.spp$capture.species.5 %in% capture.spp$capture.species.2 & !capture.spp$capture.species.5 %in% capture.spp$capture.species.3 & !capture.spp$capture.species.5 %in% capture.spp$capture.species.4)]))
sort (unique (capture.spp$capture.species.6 [which (!capture.spp$capture.species.6 %in% capture.spp$capture.species.1 & !capture.spp$capture.species.6 %in% capture.spp$capture.species.2 & !capture.spp$capture.species.6 %in% capture.spp$capture.species.3 & !capture.spp$capture.species.6 %in% capture.spp$capture.species.4 & !capture.spp$capture.species.6 %in% capture.spp$capture.species.5)]))
sort (unique (capture.spp$capture.species.7 [which (!capture.spp$capture.species.7 %in% capture.spp$capture.species.1 & !capture.spp$capture.species.7 %in% capture.spp$capture.species.2 & !capture.spp$capture.species.7 %in% capture.spp$capture.species.3 & !capture.spp$capture.species.7 %in% capture.spp$capture.species.4 & !capture.spp$capture.species.7 %in% capture.spp$capture.species.5 & !capture.spp$capture.species.7 %in% capture.spp$capture.species.6)]))

# initial cleaning to make recoding easier. remove all instances of tollo x, for example
capture.spp <- as.data.frame (lapply (capture.spp, function(x)
  {gsub ( "tollo ", "", x, ignore.case = TRUE)} )) # remove all leading "tollos"
capture.spp <- as.data.frame (lapply (capture.spp, function(x)
{gsub ( "t. ", "", x, fixed = TRUE)} ))
capture.spp <- as.data.frame (lapply (capture.spp, function(x)
{gsub ( "T. ", "", x, fixed = TRUE)} ))
capture.spp <- as.data.frame (lapply (capture.spp, function(x)
{gsub ( "aa", "a", x, ignore.case = TRUE)} )) # a few of these typos



