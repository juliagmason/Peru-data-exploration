## Swordfish for alexis jackson

library (tidyverse)
library (lubridate)
trips <- read_csv ("./Data_tables/cleaned_trips.csv")
trips

sf.trips <- filter (trips, 
                    capture.species.1 == "espada"|
                    capture.species.2 == "espada"|
                    capture.species.3 == "espada"|
                    capture.species.4 == "espada"|
                    capture.species.5 == "espada"|
                    capture.species.6 == "espada") # 64 trips


trips.new <- read_csv ("./Data_tables/trips_feb_2018.csv")
colnames (trips.new) <- colnames (trips)
# THERE'S SOME KIND OF WEIRD CHARACTER BEFORE TRI CODE
#colnames(trips.new)[1] <- "Trip.code"
trips.new <- filter (trips.new, Trip.code > 1544)  # 233

sf.trips.new <- filter(trips.new,
                 capture.species.1 == "espada"|
            capture.species.2 == "espada" |
            capture.species.3 == "espada" |
            capture.species.4 == "espada" |
            capture.species.4 == "Espada" |
            capture.species.5 == "espada" |
            capture.species.5 == "Espada" |
            capture.species.6 == "espada" |
            capture.species.6 == "Espada" |
            capture.species.7 == "espada"
            ) # 28

sf.trips <- rbind (sf.trips, sf.trips.new) # 92
   

sf.obj.trips <- filter (trips,
                        Objective.species == "espada"|
                        Objective.species.2 == "espada"|
                          grepl ("espada", Objective.species.3))

# 7 trips, only 1 not in sf.trips, 1375

sf.sets <- read_csv ("./Data_tables/cleaned_sets.csv") %>%
  filter (Trip.code %in% sf.trips$Trip.code) # 525 sets

sf.sets.new <- read_csv ("./Data_tables/lances_feb_2018.csv")
colnames (sf.sets.new)[1] <- "Trip.code"
sf.sets.new$Fecha <- as.Date (sf.sets.new$Fecha, format = "%d/%m/%Y", origin = "1970-01-01")

sf.sets.new <- sf.sets.new %>%
  filter (Trip.code %in% sf.trips.new$Trip.code) %>%
  mutate ("Lance.code" = paste (Trip.code, `Lance #`, sep = "_"),
          "MY" = paste (year(Fecha), month (Fecha), "01", sep = "-")) %>%
  select (Trip.code, Lance.code, Fecha, MY) %>%
  rename ("Date" = Fecha)

#year(sf.sets.new$Fecha[which (sf.sets.new$Lance.code == "1748_2")]) <- 2017 not an issue

sf.sets.old.lite <- select (sf.sets, Trip.code, Lance.code, Date, MY) 

sf.sets <- rbind (sf.sets.old.lite, sf.sets.new)
# gear breakdown
length (which (sf.trips$hooks > 0)) # 20
length (which (sf.trips$linea.madre.net.depth == "profundidad")) #1

png (filename = "./SF_months_by_year.png")
sf.sets %>%
  group_by (MY) %>%
  summarise (count = n()) %>%
  ggplot () +
  geom_line(aes (x = month (MY), y = count)) +
 facet_wrap (~year(MY)) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month") +
  ggtitle ("Observed fishing sets with swordfish catch")
dev.off()

png (filename = "./SF_months_hist.png")
sf.sets %>%
  ggplot () +
  geom_histogram(aes (x = month(Date)), binwidth = 1) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month") +
  ggtitle ("Observed fishing sets with swordfish catch")
dev.off()



sf.sets %>%
  ggplot () +
  geom_histogram(aes (x = month(Date), fill = year(Date)), binwidth = 1) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month")

# make into time series??
sf.sets.ts <- ts (data = sf.sets$Date, start = min (sf.sets$Date), end = max (sf.sets$Date))


# separate gillnets and longlines
# mutate by hooks > 0 then fill by ll
sf.trips.gear <- sf.trips %>%
  mutate ("Gear" = ifelse (! is.na(hooks), "LL", "GN")) %>%
  select (Trip.code, Gear)


png (filename = "./SF_months_by_year_gear.png")
sf.sets %>%
  left_join (sf.trips.gear, by = "Trip.code") %>%
  group_by (MY, Gear) %>%
  summarise (count = n()) %>%
  ggplot () +
  geom_line(aes (x = month (MY), y = count, col = Gear)) +
  facet_wrap (~year(MY)) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month") +
  ggtitle ("Observed fishing sets with swordfish catch")
dev.off()

png (filename = "./SF_months_hist_gear.png")
sf.sets %>%
  left_join (sf.trips.gear, by = "Trip.code") %>%
  ggplot () +
  geom_bar(aes (x = month(Date), fill = Gear), binwidth = 1) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month") +
  ggtitle ("Observed fishing sets with swordfish catch")
dev.off()

png (filename = "./SF_months_line_gear.png")
sf.sets %>%
  left_join (sf.trips.gear, by = "Trip.code") %>%
  group_by (month(Date), Gear) %>%
  summarize (count = n()) %>%

  ggplot () +
  geom_line(aes (x = `month(Date)`, y  = count, col = Gear), binwidth = 1) +
  scale_x_continuous (breaks = c (1:12)) +
  labs (x = "Month") +
  ggtitle ("Observed fishing sets with swordfish catch")
dev.off()

# how many sets??
sf.sets %>%
  left_join (sf.trips.gear, by = "Trip.code") %>%
  group_by (Gear) %>%
  summarize (count = n())

