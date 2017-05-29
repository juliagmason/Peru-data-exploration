## Make Gillnet sharks
# May 29, 2017

all.gn <- filter (trips, !Net.depth.category %in% "Fondo", Sistema %in% c("cortina", "manual", "red", "rayera", "mechanico","mixed nets"), !is.na(trips$panes) | !is.na(trips$pane.1), linea.madre.net.depth != "profundidad", Lugar.de.zarpe != "constante") # use a couple data columns in case. if there's hooks, that's longlines

svry.trip <- filter (all.gn, Lugar.de.zarpe == "Salaverry")
svry.trip.extra <- filter (all.gn, Trip.code %in% c(264, 564, 1433))
svry.trip <- rbind (svry.trip, svry.trip.extra) # 289 trips


svry.sharks <- filter (sharks, trip.code %in% svry.trip$Trip.code) # 8615 sharks
write.csv (svry.sharks, file = "../PDsharks/Salaverry_sharks.csv")

sj.trip <- filter (all.gn, Lugar.de.zarpe == "San Jose" | Lugar.de.zarpe == "Bayovar" | Lugar.de.zarpe == "bayovar")
sj.trip.extra <- filter (all.gn, Trip.code == 599) # ? this doesn't exist anymore
sj.trip <- rbind (sj.trip, sj.trip.extra) # 94 trips


sj.sharks <- filter (sharks, trip.code %in% sj.trip$Trip.code) # 3776 sharks

write.csv (sj.sharks, file = "../PDsharks/SanJose_sharks.csv")
