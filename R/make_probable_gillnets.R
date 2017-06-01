### Create a dataframe of just trips that I believe are gillnets, with a few important variables to calculate fishing effort ####


library(tidyverse)

trips <- read.csv("./Data_tables/cleaned_trips.csv", na.strings = "NA")
sets <- read.csv ("./Data_tables/cleaned_sets.csv")

nets <- filter(trips, (!is.na(trips$panes)))  # 671
unique(nets$Lugar.de.zarpe) # going to say that's okay, they're all in north
gillnet.prob <- filter(nets,!Net.depth.category %in% "Fondo" & !Trip.code %in% 1332) # remove bottom set nets and trip 1332, which is a huge mesh size outlier. 607 trips
unique(gillnet.prob$Sistema)  # doesn't include red de fondo

# standardize gillnet mesh size and length units
gillnet.prob$pane.1.length.std <- numeric(nrow(gillnet.prob))
for (i in 1:nrow(gillnet.prob)) {
  if (gillnet.prob$pane.size.units[i] == "b") {
    gillnet.prob$pane.1.length.std[i] = gillnet.prob$pane.size.length[i]/1.8288
  }
  else {gillnet.prob$pane.1.length.std[i] = gillnet.prob$pane.size.length[i]}
}
summary(gillnet.prob$pane.1.length.std)

gillnet.prob$pane.1.depth.std <- numeric(nrow(gillnet.prob))
for (i in 1:nrow(gillnet.prob)) {
  if (gillnet.prob$pane.size.units[i] == "b") {
    gillnet.prob$pane.1.depth.std[i] = gillnet.prob$pane.size.depth[i]/1.8288
  }
  else {gillnet.prob$pane.1.depth.std[i] = gillnet.prob$pane.size.depth[i]}
}
summary(gillnet.prob$pane.1.depth.std)

gillnet.prob$mesh.size.std <- numeric(nrow(gillnet.prob))
for (i in 1:nrow(gillnet.prob)) {
  if (gillnet.prob$mesh.size.units[i] == "p") {
    gillnet.prob$mesh.size.std[i] = gillnet.prob$mesh.size[i] * 2.54
  }
  else {gillnet.prob$mesh.size.std[i] = gillnet.prob$mesh.size[i]}
}
summary(gillnet.prob$mesh.size.std)

gillnet.prob$net.depth.std <- numeric(nrow(gillnet.prob))
for (i in 1:nrow(gillnet.prob)) {
  if (gillnet.prob$Net.depth.units[i] == "b") {
    gillnet.prob$net.depth.std[i] = gillnet.prob$Net.depth[i] / 1.8288
  }
  else {gillnet.prob$net.depth.std[i] = gillnet.prob$Net.depth[i]}
}
summary(gillnet.prob$net.depth.std)

# for a preliminary net size, just assume panes are uniform and multiply # panes by net length by net depth

gillnet.prob$Simple.net.size <- gillnet.prob$panes * gillnet.prob$pane.1.length.std * gillnet.prob$pane.1.depth.std
summary(gillnet.prob$Simple.net.size) # net area in meters

# is it worth adding info about panes 2 and 3?
summary(gillnet.prob$pane.2)
length(which(gillnet.prob$pane.2 > 0))  # 53
summary(gillnet.prob$pane.3)
length (which (gillnet.prob$pane.3 > 0))  # 2, 459 and 460, both 23, both san jose
gillnet.prob$pane.3[460]
summary(gillnet.prob$pane.3.units) # both b. as are pane 2

# pare down dataset
gillnet.prob.lite <- select(gillnet.prob, Trip.code, boat.name, Vessel.capacity, Vessel.length, Lugar.de.zarpe, crew, Fecha.de.zarpe,Sistema, Objective.species, puerto.de.llegada, panes, pane.1, pane.1.length.std, pane.1.depth.std, mesh.size.std, net.depth.std, pane.2, pane.2.length, pane.3, pane.3.length, Simple.net.size, capture.species.1, capture.species.2, capture.species.3, capture.species.4, capture.species.5, capture.species.6, capture.species.7, Quantity.1, Quantity.2, Quantity.3, Quantity.4, Quantity.5, Quantity.6, Quantity.7, Quantity.Units.1, Quantity.Units.2, Quantity.Units.3, Quantity.Units.4, Quantity.Units.5, Quantity.Units.6, Quantity.Units.7,cost.of.operation, Estimada.ganacia)


write.csv(gillnet.prob.lite, file = "./Data_tables/probable_gillnets.csv")

gn.sets <- filter (sets, Trip.code %in% gillnet.prob$Trip.code)
write.csv (gn.sets, file = "./Data_tables/probable_gn_sets.csv")


## Make data tables of species catch

gn.shark <- filter (sharks, Trip.code %in% gillnet.prob$Trip.code)
write.csv (gn.shark, file = "./Data_tables/gn_shark.csv")

gn.mamm <- filter (mammals, Trip.code %in% gillnet.prob$Trip.code)
write.csv (gn.mamm, file = "./Data_tables/gn_mamm.csv")

gn.turt <- filter (turtles, Trip.code %in% gillnet.prob$Trip.code)
write.csv (gn.turt, file = "./Data_tables/gn_turt.csv")

gn.bird <- filter (birds, Trip.code %in% gillnet.prob$Trip.code)
write.csv (gn.bird, file = "./Data_tables/gn_bird.csv")
