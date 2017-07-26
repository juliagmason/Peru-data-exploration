##### PD data km day effort #########
# Julia G Mason
# 11/10/2016 (edited 7/10/17)
library (tidyverse)

gn.trips <- read_csv ("./Data_tables/probable_gillnets.csv")
gn.sets <- read_csv ("./Data_tables/probable_gn_sets.csv")


## Catch per unit effort, defined as km/day

# first I have to figure out the net length
# first, have all pane net length in meters. then multiply number of panes by length, and then add all the different panes

# sizes have been standardized in make_probable_gillnets. 

# pane.1 notation starts at gn.trips[434], Trip.Code 1181

gn.total.pane.length <- rep (0, nrow (gn.trips))
for ( i in 1:nrow (gn.trips)) {
  if (gn.trips$Trip.code[i] < 1180) {
    pane1 <- gn.trips$panes[i] * gn.trips$pane.1.length.std[i]
  } else (pane1 <- gn.trips$pane.1[i] * gn.trips$pane.1.length.std[i])
  pane2 <- gn.trips$pane.2[i] * gn.trips$pane.2.length[i]
  pane3 <- gn.trips$pane.3[i] * gn.trips$pane.3.length[i]

  gn.total.pane.length[i] <- sum(pane1, pane2, pane3, na.rm = TRUE)/1000
}

# if all three were NA, the sum becomes 0. I want that to be NA. 

gn.total.pane.length[which (gn.total.pane.length == 0)] <- NA

# I should be able to do this without a for loop but this isn't working...
# gn.km.day <- gn.trips %>%
#   mutate (net.km = sum (ifelse (Trip.code < 1180, panes * pane.1.length.std, pane.1 * pane.1.length.std), pane.2 * pane.2.length, pane.3 * pane.3.length, na.rm = TRUE))


# append trip codes to net km length
net.km <- as.data.frame(cbind (gn.trips$Trip.code, gn.total.pane.length))
colnames(net.km) <- c("Trip.code", "net.km")
net.km$Trip.code <- as.integer(net.km$Trip.code)

# join to sets
gn.sets <- left_join (gn.sets, net.km, by = "Trip.code")

# calculate km day
gn.sets$Km.day <- gn.sets$Soak.time * gn.sets$net.km / 24


# separate effort df for appending to animals
gn.km.day <- as.data.frame(cbind(gn.sets$Lance.code, gn.sets$Km.day))
colnames(gn.km.day) <- c("Lance.code", "Km.day")
gn.km.day$Km.day <- as.numeric(as.character(gn.km.day$Km.day))

write.csv (gn.km.day, file = "./Data_tables/gn_km_day.csv", row.names = FALSE)

