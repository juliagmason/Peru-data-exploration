## Make csvs for sharks in Salaverry and San Jose
# May 29, 2017

library (tidyverse)

gn.trips <- read.csv ("./Data_tables/probable_gillnets.csv")
gn.trips$Fecha.de.zarpe <- as.Date (gn.trips$Fecha.de.zarpe, format = "%Y-%m-%d")

# all.gn <- filter (trips, !Net.depth.category %in% "Fondo", Sistema %in% c("cortina", "manual", "red", "rayera", "mechanico","mixed nets"), !is.na(trips$panes) | !is.na(trips$pane.1), linea.madre.net.depth != "profundidad", Lugar.de.zarpe != "constante") # use a couple data columns in case. if there's hooks, that's longlines

svry.trip <- filter (gn.trips, Lugar.de.zarpe == "Salaverry", Fecha.de.zarpe >= "2005-01-01")
svry.trip.extra <- filter (gn.trips, Trip.code %in% c(264, 564, 1433))
svry.trip <- rbind (svry.trip, svry.trip.extra) # 289 trips
write.csv (svry.trip, file = "./Data_tables/svry_trip.csv")


svry.sharks <- filter (sharks, trip.code %in% svry.trip$Trip.code) # 8615 sharks
write.csv (svry.sharks, file = "../PDsharks/Salaverry_sharks.csv")

sj.trip <- filter (gn.trips, Lugar.de.zarpe == "San Jose" | Lugar.de.zarpe == "Bayovar" | Lugar.de.zarpe == "bayovar")
sj.trip.extra <- filter (gn.trips, Trip.code == 599) # ? this doesn't exist anymore
sj.trip <- rbind (sj.trip, sj.trip.extra) # 94 trips


sj.sharks <- filter (sharks, trip.code %in% sj.trip$Trip.code) # 3776 sharks

write.csv (sj.sharks, file = "../PDsharks/SanJose_sharks.csv")
