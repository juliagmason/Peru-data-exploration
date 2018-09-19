# Data stats for methods 
# 9 18 2018

library (tidyverse)
library (lubridate)

# how many unique observers
trips <- read_csv ("Data_tables/cleaned_trips.csv")

unique (trips$Lugar.de.zarpe) # 22

unique (trips$boat.name)

nrow (trips)

# just gn, don't need to say how 
gn_trips <- read_csv("Data_tables/probable_gillnets.csv") # 14
gn_sets <- read_csv ("Data_tables/probable_gn_sets.csv")

unique (gn_trips$Lugar.de.zarpe) # 14 ports

summary (year(gn_trips$Fecha.de.zarpe), na.rm = TRUE)

# avg time to deploy and retrieve nets
sets <- read_csv ("Data_tables/cleaned_sets.csv")

net_time <- sets %>%
  filter (Lance.code %in% gn_sets$Lance.code) %>%
  mutate (hour.diff.1 = Hour.2 - Hour.1, 
          hour.diff.2 = Hour.4 - Hour.3) 
