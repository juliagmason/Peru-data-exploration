##### PD Data Cleaning #####
#rm(list=ls())

library (tidyverse)
library (lubridate)


# Bring in original data, January 2016, append sep 2016 data

trips <- read.csv("./Raw_data/trips_main_fixed.csv")
new.trips <- read.csv ("./Raw_data/trips_sep8.csv") %>% filter (!Trip.Code %in% trips$Trip.code)
colnames(new.trips) <- colnames(trips) # some capitals were changed
trips <- rbind (trips, new.trips)
trips$Fecha.de.zarpe <- as.Date(trips$Fecha.de.zarpe, format = "%m/%d/%y")

write.csv (trips, file = "./Data_tables/cleaned_trips.csv")


sets <- read.csv ("./Raw_data/Lances_sep8_16_fixed.csv") 
sets$Fecha.de.inicio <- as.character (sets$Fecha.de.inicio) #, format = "%m/%d/%y")
sets$fecha.de.recojo <- as.character (sets$fecha.de.recojo) #, format = "%m/%d/%y")
sets$Latitud.inicial <- as.character (sets$Latitud.inicial)
sets$latitide.final <- as.character (sets$latitide.final)
sets$longitude.inicial <- as.character (sets$longitude.inicial)
sets$longitude.final <- as.character (sets$longitude.final)
sets$Lat.1 <- as.character(sets$Lat.1)
sets$Lat.2 <- as.character(sets$Lat.2)
sets$Lat.3 <- as.character(sets$Lat.3)
sets$lat.4 <- as.character(sets$lat.4)
sets$Long.1 <- as.character(sets$Long.1)
sets$Long.2 <- as.character(sets$Long.2)
sets$Long.3 <- as.character(sets$Long.3)
sets$Long.4 <- as.character(sets$Long.4)

sets$Lance.code <- paste (sets$Trip.code, sets$Lance.., sep = ".")



# fix dates and horas that excel added in

# On May 29, 2017: 246.8, 362.1, 376.4, 590.2 were all gillnets with negative soak times. 
sets$fecha.de.recojo[which (sets$Lance.code == 246.8)] <- "5/15/06"
sets$Fecha.de.inicio[which (sets$Lance.code == 362.1)] <- "9/27/07"
 #"2007-09-28"
sets$fecha.de.recojo[which (sets$Lance.code == 376.4)] <- "11/11/07" # "2007-11-10"
sets$fecha.de.recojo[which (sets$Lance.code == 590.2)] <- "1/16/11" #"2011-01-16"

# fix hora
sets$Hora.de.inicio <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$Fecha.de.inicio), gsub (".* ", "", sets$Hora.de.inicio), sep = " "), format = "%m/%d/%y %H:%M:%S"))
sets$hora.de.recojo <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$fecha.de.recojo), gsub (".* ", "", sets$hora.de.recojo), sep = " "), format = "%m/%d/%y %H:%M:%S"))
sets$Hour.1 <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$Fecha.de.inicio), sets$Hour.1, sep = " "), format = "%m/%d/%y %H:%M"))
sets$Hour.2 <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$Fecha.de.inicio), sets$Hour.2, sep = " "), format = "%m/%d/%y %H:%M"))
sets$Hour.3 <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$fecha.de.recojo), sets$Hour.3, sep = " "), format = "%m/%d/%y %H:%M"))
sets$Hour.4 <- as.character(as.POSIXct(paste(gsub(" .*$", "", sets$fecha.de.recojo), sets$Hour.4, sep = " "), format = "%m/%d/%y %H:%M"))
sets$Soak.time <- as.numeric(difftime (sets$hora.de.recojo, sets$Hora.de.inicio, units = "hours"))


sets$Fecha.de.inicio <- as.Date (sets$Fecha.de.inicio, format = "%m/%d/%y")
sets$fecha.de.recojo <- as.Date (sets$fecha.de.recojo, format = "%m/%d/%y")


# fix lat and long for sets

# in lances sep 8 manually fixed 43 which (lat.dec < -90)
# also had to fix in regular sets!
# for those that are four digit numbers, try to search for doesn't have a .?
#grepl(".", sets$Latitud.inicial[1])
# x <- as.character(1234)
# gsub ( "^(.{2})(.*)$", "\\1'\\2", x)

lat.i.dec <- numeric(length(sets$Latitud.inicial))
for (i in 1:length(sets$Latitud.inicial)) { 
  if(grepl("\\.", sets$Latitud.inicial[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Latitud.inicial[i])
  } else {tmpA <- sets$Latitud.inicial[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.i.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.i.dec[i] = -1* (as.numeric(tmpA)) }
}
# sets$Lance.code [ which (is.na (lat.i.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.i.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Latitud.inicial == "")])]

# look for NAs introduced
#options (warn = 2)



lat.f.dec <- numeric(length(sets$latitide.final))
for (i in 1:length(sets$latitide.final)) { 
  if(grepl("\\.", sets$latitide.final[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$latitide.final[i])
  } else {tmpA <- sets$latitide.final[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.f.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.f.dec[i] = -1* (as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lat.f.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.f.dec) == TRUE)] %in% sets$Lance.code [ which (sets$latitide.final == "")])]



lon.i.dec <- numeric(length(sets$longitude.inicial))
for (i in 1:length(sets$longitude.inicial)) { 
  if(grepl("\\.", sets$longitude.inicial[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$longitude.inicial[i])
  } else {tmpA <- sets$longitude.inicial[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.i.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.i.dec[i] = -1*(as.numeric(tmpA)) }
}


# sets$Lance.code [ which (is.na (lon.i.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.i.dec) == TRUE)] %in% sets$Lance.code [ which (sets$longitude.inicial == "")])]

lon.f.dec <- numeric(length(sets$longitude.final))
for (i in 1:length(sets$longitude.final)) { 
  if(grepl("\\.", sets$longitude.final[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$longitude.final[i])
  } else {tmpA <- sets$longitude.final[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.f.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.f.dec[i] = -1*(as.numeric(tmpA)) }
}


# sets$Lance.code [ which (is.na (lon.f.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.f.dec) == TRUE)] %in% sets$Lance.code [ which (sets$longitude.final == "")])]

lat.1.dec <- numeric(length(sets$Lat.1))
for (i in 1:length(sets$Lat.1)) { 
  if(grepl("\\.", sets$Lat.1[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Lat.1[i])
  } else {tmpA <- sets$Lat.1[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.1.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.1.dec[i] = -1* (as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lat.1.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.1.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Lat.1 == "")])]

lat.2.dec <- numeric(length(sets$Lat.2))
for (i in 1:length(sets$Lat.2)) { 
  if(grepl("\\.", sets$Lat.2[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Lat.2[i])
  } else {tmpA <- sets$Lat.2[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.2.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.2.dec[i] = -1* (as.numeric(tmpA)) }
}

 # sets$Lance.code [ which (is.na (lat.2.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.2.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Lat.2 == "")])]


lat.3.dec <- numeric(length(sets$Lat.3))
for (i in 1:length(sets$Lat.3)) { 
  if(grepl("\\.", sets$Lat.3[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Lat.3[i])
  } else {tmpA <- sets$Lat.3[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.3.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.3.dec[i] = -1* (as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lat.3.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.3.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Lat.3 == "")])]


lat.4.dec <- numeric(length(sets$lat.4))
for (i in 1:length(sets$lat.4)) { 
  if(grepl("\\.", sets$lat.4[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Lat.1[i])
  } else {tmpA <- sets$lat.4[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.4.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.4.dec[i] = -1* (as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lat.4.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lat.4.dec) == TRUE)] %in% sets$Lance.code [ which (sets$lat.4 == "")])]

lon.1.dec <- numeric(length(sets$Long.1))
for (i in 1:length(sets$Long.1)) { 
  if(grepl("\\.", sets$Long.1[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Long.1[i])
  } else {tmpA <- sets$Long.1[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.1.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.1.dec[i] = -1*(as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lon.1.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.1.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Long.1 == "")])]

lon.2.dec <- numeric(length(sets$Long.2))
for (i in 1:length(sets$Long.2)) { 
  if(grepl("\\.", sets$Long.2[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Long.2[i])
  } else {tmpA <- sets$Long.2[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.2.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.2.dec[i] = -1*(as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lon.2.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.2.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Long.2 == "")])]

lon.3.dec <- numeric(length(sets$Long.3))
for (i in 1:length(sets$Long.3)) { 
  if(grepl("\\.", sets$Long.3[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Long.3[i])
  } else {tmpA <- sets$Long.3[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.3.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.3.dec[i] = -1*(as.numeric(tmpA)) }
}

# sets$Lance.code [ which (is.na (lon.3.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.3.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Long.3 == "")])]

lon.4.dec <- numeric(length(sets$Long.4))
for (i in 1:length(sets$Long.4)) { 
  if(grepl("\\.", sets$Long.4[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets$Long.4[i])
  } else {tmpA <- sets$Long.4[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.4.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.4.dec[i] = -1*(as.numeric(tmpA)) }
}


coords<- data.frame(sets$Lance.code, lat.i.dec, lat.1.dec, lon.i.dec, lon.1.dec, lat.f.dec, lat.2.dec, lon.f.dec, lon.2.dec,lat.3.dec, lon.3.dec, lat.4.dec, lon.4.dec)
colnames(coords) = c("Lance.code", "Lat.I", "Lat.1", "Lon.I", "Lon.1", "Lat.F", "Lat.2","Lon.F", "Lon.2","Lat.3", "Lon.3", "Lat.4", "Lon.4")
# sets$Lance.code [ which (is.na (lon.4.dec) == TRUE)] [ ! (sets$Lance.code [ which (is.na (lon.4.dec) == TRUE)] %in% sets$Lance.code [ which (sets$Long.4 == "")])]
#options(warn = 0)

sets$Lat.I <- lat.i.dec
sets$Lat.F <- lat.f.dec
sets$Lon.I <- lon.i.dec
sets$Lon.F <- lon.f.dec

# eventually append date late lon
set.DLL <- select(sets, Lance.code, Fecha.de.inicio, Lat.I, Lon.I, Lat.F, Lon.F) 
set.DLL$Fecha.de.inicio <- as.Date(set.DLL$Fecha.de.inicio, format = "%Y-%m-%d") 
colnames(set.DLL)[2] <- "Date"
# add month-yr column for looking month by month
set.DLL$MY <- as.Date(
  paste( year(set.DLL$Date), 
    ifelse( month(set.DLL$Date) < 10, 
           paste(0, month(set.DLL$Date), sep = ""), 
           month(set.DLL$Date)), 
           "01", sep = "-"), 
    format = "%Y-%m-%d")

set.DLL$Year <- year(set.DLL$Date)

set.DLL$Lance.code <- as.character( (set.DLL$Lance.code))

write.csv (set.DLL, file = "./Data_tables/set_DLL.csv")

sets <- cbind (sets, set.DLL$Date, set.DLL$MY)
colnames(sets)[84:85] <- c("Date", "MY")

write.csv (sets, file = "./Data_tables/cleaned_sets.csv")

turtles <- read.csv("./Raw_data/Turtles_main.csv")

new.turt <- read.csv ("./Raw_data/turtles_sep8.csv") %>% filter (!Trip.code %in% turtles$Trip.code)

colnames(new.turt) <- colnames(turtles)
turtles <- rbind (turtles, new.turt)
turtles$Lance.code <- paste(turtles$Trip.code, turtles$Lance, sep = ".")
turtles <- left_join(turtles, set.DLL, by = "Lance.code")
# replace species codes with names. use sci name?
turtles$Species <- factor (turtles$Species, levels = c (1, 2, 3, 4, 5, 6), labels = c ("D_coriacea", "E_imbricata", "C_caretta", "C_mydas", "L_olivacea", "Unk"))

sharks <- read.csv("./Raw_data/sharks_main.csv") # messed with shark_main already
sharks$Lance.code <- paste(sharks$trip.code, sharks$lance, sep = ".")
sharks <- left_join(sharks, set.DLL, by = "Lance.code") %>%
  rename (Species = species, Trip.code = trip.code) # fix capitals to match other data frames

mammals <- read.csv("./Raw_data/Mammals_Main.csv")
new.mamm <- read.csv("./Raw_data/mammals_sep8.csv") %>% filter (!Trip.code %in% mammals$Trip.code)

colnames(new.mamm) <- colnames(mammals)
mammals <- rbind (mammals, new.mamm)
mammals$Lance.code <- paste(mammals$Trip.code, mammals$Lance.., sep = ".")
mammals <- left_join(mammals, set.DLL, by = "Lance.code")
# this was all I played with thus far
# Change species from integer to spp name. 
# Replace species codes with factor species name instead of integer. 
mammals$Species <- factor (mammals$Species, levels = c (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), labels = c ("D_capensis", "P_spinipinnus", "L_oscurus", "T_truncatus", "otro", "unk", "Globicephala_spp", "O_flavescens", "G_griseus", "lobo_chusco", "A_australis"))

birds <- read.csv("./Raw_data/seabirds_sep8.csv") %>% filter (When.captured. != 3) # remove just sightings. 
birds$Lance.code <- paste(birds$Trip.code, birds$Lance.., sep = ".")
birds <- left_join (birds, set.DLL, by = "Lance.code")

other <- read.csv("./Raw_data/Other_species_sep8.csv") 
other$Lance.code <- paste(other$Trip.code, other$Lance.., sep = ".")
other <- left_join (other, set.DLL, by = "Lance.code")

mola <- read.csv("./Raw_data/sunfish_sep8.csv") ## remember to filter for just sightings
mola$Lance.code <- paste(mola$trip, mola$set, sep = ".")
mola <- left_join (mola, set.DLL, by = "Lance.code")

seahorse <- read.csv("./Raw_data/seahorses_sep8.csv")
seahorse$Lance.code <- paste(seahorse$trip, seahorse$set, sep = ".")
seahorse <- left_join(seahorse, set.DLL, by = "Lance.code")

