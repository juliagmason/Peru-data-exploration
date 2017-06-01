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
set.DLL$Fecha.de.inicio <- as.Date(set.DLL$Fecha.de.inicio, format = "%m/%d/%y") 
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

sets <- cbind (sets, set.DLL$Date, set.DLL$MY)
colnames(sets)[84:85] <- c("Date", "MY")

write.csv (sets, file = "./Data_tables/cleaned_sets.csv")

turtles <- read.csv("./Raw_data/Turtles_main.csv")

new.turt <- read.csv ("./Raw_data/turtles_sep8.csv") %>% filter (!Trip.code %in% turtles$Trip.code)

colnames(new.turt) <- colnames(turtles)
turtles <- rbind (turtles, new.turt)
turtles$Lance.code <- paste(turtles$Trip.code, turtles$Lance, sep = ".")
turtles <- left_join(turtles, set.DLL, by = "Lance.code")

sharks <- read.csv("./Raw_data/sharks_main.csv") # messed with shark_main already
sharks$Lance.code <- paste(sharks$trip.code, sharks$lance, sep = ".")
#sharks <- left_join(sharks, set.DLL, by = "Lance.code")

mammals <- read.csv("./Raw_data/Mammals_Main.csv")
new.mamm <- read.csv("./Raw_data/mammals_sep8.csv") %>% filter (!Trip.code %in% mammals$Trip.code)

colnames(new.mamm) <- colnames(mammals)
mammals <- rbind (mammals, new.mamm)
mammals$Lance.code <- paste(mammals$Trip.code, mammals$Lance.., sep = ".")
mammals <- left_join(mammals, set.DLL, by = "Lance.code")
# this was all I played with thus far

birds <- read.csv("./Raw_data/seabirds_sep8.csv", stringsAsFactors=FALSE, fileEncoding="latin1") %>% filter (When.captured. != 3) # remove just sightings. Changed fileEncoding to encoding on windows
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



##########################################
### Look for outliers


# 
# dotchart(gn.all.coords$Lat.4)
# gn.all.sets$Lance.code[which(gn.all.coords$Lat.4 < -60)]
# dotchart(gn.all.coords$Lon.4)
# gn.all.sets$Lance.code[which(gn.all.coords$Lon.4 > -77)]
# 
# gn.all.coords$Lance.code[which(gn.all.coords$Lat.I != gn.all.coords$Lat.1)]
# gn.all.coords$Lance.code[which(gn.all.coords$Lat.F != gn.all.coords$Lat.2)]
# 
# gn.all.coords$Lance.code[which(gn.all.coords$Lon.I != gn.all.coords$Lon.1)]
# gn.all.coords$Lance.code[which(gn.all.coords$Lon.F != gn.all.coords$Lon.2)]
# 
# 
# #dotchart(sets$Lat.I)
# dotchart(gn.10.sets$Lat.I)
# gn.10.sets$Lance.code[which(gn.10.sets$Lat.I < -20)]
# 
# dotchart(gn.all.sets$Lat.I)
# 
# dotchart(gn.10.sets$Lat.F)
# #gn.10.sets$Lance.code[which(gn.10.sets$Lat.F < -15)]
# 
# dotchart(gn.10.sets$Lon.I)
# gn.10.sets$Lance.code[which(gn.10.sets$Lon.I > -75)]
# gn.10.sets$Lance.code[which(gn.10.sets$Lon.I < -85)]
# 
# dotchart(gn.10.sets$Lon.F)
# gn.10.sets$Lance.code[which(gn.10.sets$Lon.F > -20)]
# gn.10.sets$Lance.code[which(gn.10.sets$Lon.F < -81)]
# 
# # plot spatial extent
# gn.map <- get_map(location = c(lon = mean(gn.all.sets$Lon.I, na.rm = TRUE), lat = mean(gn.all.sets$Lat.I, na.rm = TRUE)), zoom = 5, maptype = "satellite", scale = 2)
# 
# gn.map.zoom <- get_map(location = c(lon = mean(gn.all.sets$Lon.I, na.rm = TRUE), lat = mean(gn.all.sets$Lat.I, na.rm = TRUE)), zoom = 6, maptype = "satellite", scale = 2)
# 
# gn.map.zoom2 <- get_map(location = c(lon = mean(gn.all.sets$Lon.I, na.rm = TRUE), lat = mean(gn.all.sets$Lat.I, na.rm = TRUE)), zoom = 7, maptype = "satellite", scale = 2)
# 
# ggmap(gn.map) + geom_point(data = gn.all.sets, aes(x=Lon.I, y=Lat.I, alpha = 0.5, size =0.5))
# 
# ggmap(gn.map.zoom2) + geom_point(data = gn.all.sets, aes(x=Lon.I, y=Lat.I, alpha = 0.5, size =1))
# 
# # still have some on land :( 
# gn.all.sets$Lance.code[which(gn.all.sets$Lon.I > -80.5 & gn.all.sets$Lat.I > -6.5)]
# 
# 
# #soak.time
# #dotchart(gn.10.sets$Soak.time)
# 
# dotchart(gn.all.sets$Soak.time)
# 
# gn.all.sets$Lance.code[which(gn.all.sets$Soak.time > 34)]
# gn.all.sets$Lance.code[which(gn.all.sets$Soak.time < 0)]
# 
# gn.10.sets$Lance.code[which(gn.10.sets$Soak.time <= 0)]
# 
# # horas
# gn.all.sets$Lance.code[which(gn.all.sets$Hour.1 != gn.all.sets$Hora.de.inicio)]
# gn.all.sets$Lance.code[which(gn.all.sets$Hour.3 != gn.all.sets$hora.de.recojo)]
# 
# dotchart(as.numeric(difftime (gn.all.sets$Hour.2, gn.all.sets$Hour.1, units = "hours")))
# tend <- as.numeric(difftime (gn.all.sets$Hour.2, gn.all.sets$Hour.1, units = "hours"))
# gn.all.sets$Lance.code[which(as.numeric(difftime (gn.all.sets$Hour.2, gn.all.sets$Hour.1, units = "hours")) <= 0)]
# 
# 
# # net size?
# dotchart(gn.10$.panes)
# gn.10$Trip.Code[which(gn.10$..panes > 70)] # 1370, appears to be real
# dotchart(gn.10$Pane.1..) # not sure why it's stratified
# dotchart(gn.10$Pane.size...Depth)
# gn.10$Trip.Code[which(gn.10$Pane.size...Depth > 40)] # 748 1238 1442
# dotchart(gn.10$Pane.size...Length)
# gn.10$Trip.Code[which(gn.10$Pane.size...Length > 90)] # 1246 1247 1248 1249 1250 1252 1323 1324 ok
# dotchart(gn.10$Mesh.size) # 1332
# dotchart(gn.10$Mesh.size[-which(gn.10$Trip.Code == 1332)])
# gn.10$Trip.Code[which(gn.10$Mesh.size > 15)] # 739, 20 cm ok
# dotchart(gn.10$Net.depth)
# dotchart(gn.10$Pane.2..)
# dotchart(gn.10$Pane.2.length)
# dotchart(gn.10$Pane.2.depth)
# gn.10$Trip.Code[which(gn.10$Pane.2.depth > 35)] # 1295 1332
# dotchart(gn.10$Pane.2.water.depth, main = "Pane 2 water depth, gillnets")
# dotchart(gn.10$Pane.2.mesh.size[-which(gn.10$Trip.Code == 1332)])
# dotchart(gn.10$Pane.3..)
# dotchart(gn.10$Pane.3.length)
# dotchart(gn.10$Pane.3.depth)
# dotchart(gn.10$Pane.3.mesh.size)
# dotchart(gn.10$Pane.3.water.depth)
# dotchart(gn.10$mixed.....panes) # there's only 1 so whatever
# 
# dotchart(all.gn$panes)
# all.gn$Trip.code[which(all.gn$panes > 70)] # 1370, appears to be real
# dotchart(all.gn$pane.1) 
# dotchart(all.gn$pane.size.depth)
# all.gn$Trip.code[which(all.gn$pane.size.depth > 20)] # 1465 ok (m)
# dotchart(all.gn$pane.size.length)
# all.gn$Trip.code[which(all.gn$pane.size.length > 100)] # 1494 ok (m)
# dotchart(all.gn$mesh.size) 
# dotchart(all.gn$Net.depth)
# dotchart(all.gn$pane.2)
# dotchart(all.gn$pane.2.length)
# dotchart(all.gn$pane.2.depth)
# dotchart(all.gn$pane.2.water.depth, main = "Pane 2 water depth, gillnets")
# dotchart(all.gn$pane.2.mesh.size)
# dotchart(all.gn$pane.3)
# dotchart(all.gn$pane.3.length)
# dotchart(all.gn$pane.3.depth)
# dotchart(all.gn$pane.3.mesh.size)
# dotchart(all.gn$Pane.3.water.depth)
# dotchart(all.gn$mixed.panes) 
# 
# # catch summaries
# dotchart(gn.10$Quantity.1)
# gn.10$Trip.Code[which(gn.10$Quantity.1 > 8000)] # 1249
# dotchart(gn.10$Quantity.2)
# dotchart(gn.10$Quantity.3)
# dotchart(gn.10$Quantity.4)
# dotchart(gn.10$Quantity.5)
# gn.10$Trip.Code[which(gn.10$Quantity.5 > 15000)] #603
# dotchart(gn.10$Quantity.6)
# dotchart(gn.10$Quantity.7)
# dotchart(gn.10$Cost.of.operation[-which(gn.10$Trip.Code == 1243)])
# gn.10$Trip.Code[which(gn.10$Cost.of.operation > 15000)] # 1233
# dotchart(gn.10$Estimada.ganacia)
# gn.10$Trip.Code[which(gn.10$Estimada.ganacia > 30000)] # 1293
# 
# dotchart(all.gn$Quantity.1)
# all.gn$Trip.code[which(all.gn$Quantity.1 > 8000)] # 1249 ok?
# dotchart(all.gn$Quantity.2)
# dotchart(all.gn$Quantity.3)
# dotchart(all.gn$Quantity.4)
# dotchart(all.gn$Quantity.5)
# all.gn$Trip.code[which(all.gn$Quantity.5 > 15000)] #603
# dotchart(all.gn$Quantity.6)
# dotchart(all.gn$Quantity.7)
# dotchart(all.gn$cost.of.operation[-which(all.gn$Trip.code == 1243)])
# all.gn$Trip.code[which(all.gn$cost.of.operation > 15000)] # 539, 1243
# dotchart(all.gn$Estimada.ganacia)
# 
# 
# # shark dataset
# unique(gn.10.sharks$Lights)
# unique(gn.10.sharks$species)
# dotchart(gn.10.sharks$fork.length)
# gn.10.sharks$Lance.code[which(gn.10.sharks$fork.length > 500)] # "1468.3" "1543.1"
# dotchart(gn.10.sharks$total.length)
# gn.10.sharks$Lance.code[which(gn.10.sharks$total.length > 15000)] # 1484.5
# dotchart(gn.10.sharks$LA)
# dotchart(gn.10.sharks$Dorsal.length)
# dotchart(gn.10.sharks$disc.width..Rays.)
# dotchart(gn.10.sharks$LC)
# gn.10.sharks$Lance.code[which(gn.10.sharks$LC > 150)] # "1475.5"
# dotchart(gn.10.sharks$..crias)
# 
# unique(gn.sharks$species)
# dotchart(gn.sharks$fork.length)
# dotchart(gn.sharks$total.length)
# dotchart(gn.sharks$LA)
# dotchart(gn.sharks$Dorsal.length)
# dotchart(gn.sharks$disc.width..Rays.)
# dotchart(gn.sharks$LC)
# dotchart(gn.sharks$..crias)
# 
# # turtle dataset
# unique(gn.10.turt$Fate)
# dotchart(gn.10.turt$LMC)
# gn.10.turt$Lance.code[which(gn.10.turt$LMC > 600)] # "1324.3" "1324.3"  # why twice??
# dotchart(gn.10.turt$LC)
# gn.10.turt$Lance.code[which(gn.10.turt$LC > 100)] # "739.2" "739.2"
# gn.10.turt$Lance.code[which(gn.10.turt$LC < 40)] # "1161.6" "1536.4" "1544.9"  # these are all fine
# dotchart(gn.10.turt$AC)
# dotchart(as.numeric(gn.10.turt$Tail.length..a.))
# dotchart(as.numeric(gn.10.turt$Tail.length..b.))
# 
# dotchart(gn.turt$LMC)
# dotchart(gn.turt$LC)
# dotchart(gn.turt$AC)
# gn.turt$Lance.code[which(gn.turt$AC > 100)]
# dotchart(as.numeric(gn.turt$Tail.length..a.))
# dotchart(as.numeric(gn.turt$Tail.length..b.))
# 
# # mammal dataset
# unique(gn.10.mamm$Lights)
# length(which(gn.10.mamm$Lights == "SUP"))
# unique(gn.10.mamm$Sample)
# dotchart(gn.10.mamm$Length)
# gn.10.mamm$Lance.code[which(gn.10.mamm$Length > 300)] # 1468.6
# gn.10.mamm$Lance.code[which(gn.10.mamm$Length < 50)] # "1275.1" "1296.5" "1347.5" "1357.4"
# dotchart(gn.10.mamm$Young...length)
# 
# dotchart(gn.mamm$Length)
# dotchart(gn.mamm$Young...length)
# 
# # bird dataset
# unique(gn.10.bird$Use)
# 
# # other
# dotchart(as.numeric(other$Length))
# other$Lance.code[which(as.numeric(other$Length) > 800)] # 1315.7
# 
# # mola
# unique(mola$sample)
# dotchart(mola$total.length)
# 
# # seahorse
# unique(seahorse$use)
# dotchart(seahorse$pouch.length)
