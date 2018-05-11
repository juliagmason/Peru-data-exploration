## Cleaning most recent year of data: 1621 sets
# March 2 2018


library (tidyverse)
library (lubridate)
library (ggmap)

# Trips starting with 1545

# tried to fix mixed dates format in excel
# 2 missing
# https://onlinejournalismblog.com/2016/07/19/how-to-fix-spreadsheet-dates-that-are-in-both-us-and-uk-formats/
# didn't work. Want DD/MM/YYYY
#trips I fixed manually:
# 1545, 1624, 1634, 1639, 1651, 1653, 1656, 1679
# excel interpreting some differently despite being in correct format, so made a new column "Fecha" all text


# also manually switched 1571_1 ,1589_4 lon inicial and lat final

sets_18 <- read_csv ("Data_tables/lances_feb_2018.csv",
                     col_types = cols (
                       "Fecha" = col_date(format = "%d/%m/%Y")
                     )) %>%
  filter (`Trip code` > 1544) %>%
  mutate ("Lance.code" = paste (`Trip code`, `Lance #`, sep = "_"))

colnames (sets_18) <- make.names (colnames (sets_18))

# > sets_18$Lance.code[which (year(sets_18$Fecha) > 2018)]
# "1748_2"

year(sets_18$Fecha[which (sets_18$Lance.code == "1748_2")]) <- 2017

# starting with lat i ----

# 1602_1 - 3 lat and lon with . instead of '
# https://stackoverflow.com/questions/34269466/replace-first-occurrence-of-but-not-second-in-r
sets_18$Latitud.inicial[which (sets_18$Trip.code == "1602")] <- sub('\\.', '\'', sets_18$Latitud.inicial[which (sets_18$Trip.code == "1602")])

# this is also a problem with long i
sets_18$longitude.inicial[which (sets_18$Trip.code == "1602")] <- sub('\\.', '\'', sets_18$longitude.inicial[which (sets_18$Trip.code == "1602")])

#1603_9 lat i is duplicated
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1603_9")] <- "12'15.344"

# 1680_9 lat i confirm 11'3 number: should be 33
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1680_9")] <- "11'33.543"

# 1693_3 lat i is a decimal. may be safe to keep based on 1592_4--similar jump between 11 and 10.5. also keeping based on lat 2
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1693_3")] <- "10'57.03"

# 1740_5 lat i remove plus
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1740_5")] <- "06'20.126"

# 1742_13 lat i comma instead of .
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1742_13")] <- "11'10.247"

# 1780_4 lat i space after 03
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1780_4")] <- "03'22.100"


# as going through lat.i.dec
# 1589 (whole trip) has ` instead of ' 
sets_18$Latitud.inicial <- gsub ("´", "'", sets_18$Latitud.inicial) 

# from ArcGIS online land points
# changing 91 to 41
sets_18$Latitud.inicial[which (sets_18$Lance.code == "1549_2")] <- "03'41.040"


# look for NAs introduced
#options (warn = 2)

lat.i.dec <- numeric(length(sets_18$Latitud.inicial))
for (i in 1:length(sets_18$Latitud.inicial)) { 
  if(grepl("\\.", sets_18$Latitud.inicial[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets_18$Latitud.inicial[i])
  } else {tmpA <- sets_18$Latitud.inicial[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lat.i.dec[i] =  -1* (tmp2 + tmp3)
  } else { lat.i.dec[i] = -1* (as.numeric(tmpA)) }
}


# lon initial ----
sets_18$longitude.inicial <- gsub ("´", "'", sets_18$longitude.inicial) 

# 1738_2 lon initial 381 instead of 81
sets_18$longitude.inicial[which (sets_18$Lance.code == "1738_2")] <- "81'03.149"

# # 1785_10 lon i is a lat, also problem with lon 1. fixed from original sheet
sets_18$longitude.inicial[which (sets_18$Lance.code == "1785_10")] <- "80'02.220"

# iterate from lon.i.dec
#which (lon.i.dec > -70) # 181, 315, 964
#sets_18$Lance.code[964] # 80'19.065
sets_18$longitude.inicial[which (sets_18$Lance.code == "1686_2")] <- "80'19.065"

#which (lon.i.dec > -75) # 85, 926
# sets_18$Lance.code[85]
# sets_18$Lance.code[926]

# changing 71 to 79
sets_18$longitude.inicial[which (sets_18$Lance.code == "1558_4")] <- "79'38.645"

# changing 70 to 78
sets_18$longitude.inicial[which (sets_18$Lance.code == "1680_7")] <- "78'39.500"

#land points from ArcGIS online: 1590_10, 1551_8, 1601_6, 1549_2

# changing 75 to 77 (match lon final)
sets_18$longitude.inicial[which (sets_18$Lance.code == "1590_10")] <- "77'50.336"

# changing 80 to 81 (match lon final)
sets_18$longitude.inicial[which (sets_18$Lance.code == "1551_8")] <- "81'08.066"

# changing 80.30 to 80.36
sets_18$longitude.inicial[which (sets_18$Lance.code == "1601_6")] <- "80'36.327"

lon.i.dec <- numeric(length(sets_18$longitude.inicial))
for (i in 1:length(sets_18$longitude.inicial)) { 
  if(grepl("\\.", sets_18$longitude.inicial[i]) == FALSE) {
    tmpA <- gsub ( "^(.{2})(.*)$", "\\1'\\2", sets_18$longitude.inicial[i])
  } else {tmpA <- sets_18$longitude.inicial[i]}
  if (grepl("\\'", tmpA) == TRUE) {
    tmp1 <- strsplit(tmpA, "'")
    tmp2 <- as.numeric(sapply(tmp1, "[", 1))
    tmp3 <- as.numeric(sapply(tmp1, "[", 2))/60
    lon.i.dec[i] =  -1*(tmp2 + tmp3)
  } else { lon.i.dec[i] = -1*(as.numeric(tmpA)) }
}

sets_18$Lat <- lat.i.dec
sets_18$Lon <- lon.i.dec

## Map
sets_18_map <- get_map (location = c (min (sets_18$Lon, na.rm = TRUE), min (sets_18$Lat), max (sets_18$Lon, na.rm = TRUE), max (sets_18$Lat)), maptype = "terrain", zoom = 6)

# not working

sets_18_map <- get_map (location = c (-85, -14, -70, -3), maptype = "terrain", zoom = 6)

ggmap (sets_18_map) +
  geom_point (data = sets_18, aes (x = Lon, y = Lat))


library (maps)
library (mapdata)
library (rworldmap)

# http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
perumap <- getMap (resolution = "low")

plot (perumap, xlim = c (-85.5, -69.5), ylim = c (-14.5, -2.5), asp = 1)
points (sets_18$Lon, sets_18$Lat)

# a few outliers, but tricky ones. try plotly

library (plotly)

peru_map <- get_map (location = c(-83, -19, -72, -2), zoom = 8, maptype = "satellite") # not working!!!

pmap <- ggmap (peru_map) +
  geom_point (data = rivers_P, aes (x = long, y = lat), cex = 0.5) +
  geom_point (data = juv.sz.PA, aes (x = Lon, y = Lat, col = Presence, shape = Target))

ggplotly (pmap)



# good enough??
sets_18 %>% select (Lance.code, Lon, Lat) %>% write.csv (file = "Data_tables/DLL_2018.csv", row.names = FALSE)


#ArcGIS online: 1590_10, 1551_8, 1601_6, 1549_2


## Sharks ----
sharks_18 <- read.csv ("Data_tables/sharks_feb_2018.csv") # read_csv not working

colnames (sharks_18) <- make.names (colnames (sharks_18))

sharks_18 <- sharks_18 %>% 
  filter (trip.code > 1544) %>%
  mutate (Lance.code = paste (trip.code, lance.., sep = "_")) %>%
  select (Lance.code, trip.code, species, fork.length, total.length, sex, pregnant.) %>%
  rename (pregnant = pregnant.)

skim (sharks_18)

sort (unique (sharks_18$species))

hammers_18 <- filter (sharks_18, species == "Martillo")
skim (hammers_18)

hist (hammers_18$fork.length)
hist (hammers_18$total.length)

# do the trip codes correspond to trip catch
sort (unique (hammers_18$trip.code))

# need trip codes that don't have hammerheads. how many of these have hammerheads in trip level catch statistics?
sort (unique (sets_18$Trip.code[which (! sets_18$Trip.code %in% hammers_18$trip.code)]))
# 
# [1] 1545 1547 1548 1550 1551 1552 1553 1554 1555 1556 1557 1561 1562 1563
# [15] 1564 1565 1566 1568 1570 1571 1573 1574 1575 1576 1577 1579 1580 1581
# [29] 1583 1585 1586 1587 1588 1590 1591 1592 1597 1598 1599 1600 1603 1604
# [43] 1605 1606 1607 1608 1609 1610 1611 1612 1613 1614 1615 1616 1617 1618
# [57] 1619 1620 1622 1623 1628 1632 1634 1635 1636 1640 1643 1645 1646 1647
# [71] 1648 1649 1650 1653 1657 1662 1664 1665 1669 1670 1672 1673 1680 1681
# [85] 1684 1685 1688 1690 1693 1699 1700 1701 1702 1703 1704 1711 1715 1716
# [99] 1717 1718 1721 1722 1723 1725 1726 1729 1730 1731 1732 1733 1734 1735
# [113] 1737 1739 1742 1743 1744 1745 1746 1747 1748 1749 1750 1751 1752 1753
# [127] 1754 1755 1757 1758 1759 1761 1762 1764 1767 1769 1770 1771 1773 1774
# [141] 1780 1781 1782 1783 1784 1786 1787

# 1548 has cruzetas. for example though 1559 has no martillo, although they caught a comparable amt to the other ones. 1568, 1570, 1571 cruzeta. 1580, 1581 martillo. 1591 MARTILLO, 1592 martillo, 1597 martillo 1598 martillo, 1599, 1600, 1603, 1604 martillo. 1628 Martillo. 1640 Martillo.. 1643 martillo. 1665 martillo, 1680, 1681 Martillo, 1688 Martillo, 1693 martillo, 1699 martillo, 1726 martillo, 1767 martillo

# 25 / 147 trips, representing 229 sets

mart_uncertain_trips <- c (1548, 1568, 1570, 1571, 1580, 1581, 1591, 1592, 1597, 1598, 1599, 1600, 1603, 1504, 168, 1640, 1643, 1665, 1680, 1681, 1688, 1693, 1699, 1726, 1767)

sets_18 %>% filter (Trip.code %in% mart_uncertain_trips) %>% nrow()

# gillnets?
trips_18 <- read_csv ("Data_tables/trips_feb_2018.csv") %>%
  filter (`Trip Code` > 1544)
colnames (trips_18) <- make.names (colnames (trips_18))

nets_18 <- filter(trips_18, (!is.na(trips_18$..panes)))  # 229
unique(nets_18$Lugar.de.zarpe) # no mancora
gillnet_18 <- filter(nets_18,!Net.depth.category %in% "Fondo") # remove bottom set nets  146 trips
unique(gillnet_18$Sistema)  #

gn_sets_18 <- filter (sets_18, Trip.code %in% gillnet_18$Trip.Code)
# abbreviated check for sneaky martillo trips, not going to worry about juvenile vs adult for now. just trips that have martillo in catch, but not measured. 
marts_18 <- read_csv ("Data_tables/sharks_feb_2018.csv") %>% 
  filter (`trip code` > 1544, species == "Martillo" | species == "S. zygaena") %>%
  mutate ("Lance.code" = paste (`trip code`, `lance #`, sep = "_"))

colnames (marts_18) <- make.names( colnames (marts_18))

mart_trips_18 <- unique (marts_18$trip.code) #86


# absence trips
no.sz.trips_18 <- filter (gillnet_18, ! Trip.Code %in% mart_trips_18) # 84

# what's in capture spp
sort (unique (gillnet_18$Capture.species.1)) # cruzetas, martillo, Martillo, T. Martillo
sort (unique (gillnet_18$Capture.species.2)) # cruzeta, martillo, Martillo, t. martillo
sort (unique (gillnet_18$Capture.species.3)) # martillo, Martillo
sort (unique (gillnet_18$Capture.species.4)) # cruzeta, martillo, MARTILLO, martilo
sort (unique (gillnet_18$Capture.species.5)) # cruzeta, martillo, Martillo, Tiburon martillo
sort (unique (gillnet_18$Capture.species.6)) # cruceta, martillo
sort (unique (gillnet_18$Capture.species.7)) # cruzeta, martillo, Martillo

sneaky_sz_trips_18 <- gillnet_18 %>%
  filter (Trip.Code %in% no.sz.trips_18$Trip.Code & (grepl ("martillo", Capture.species.1, ignore.case = TRUE) | grepl ("cruzetas", Capture.species.1) | grepl ("T. Martillo", Capture.species.1) | grepl ("martillo", Capture.species.2, ignore.case =  TRUE) | grepl ("cruzeta", Capture.species.2) | grepl ("t. martillo", Capture.species.2) | grepl ("martillo", Capture.species.3, ignore.case = TRUE) | grepl ("martillo", Capture.species.4, ignore.case = TRUE) | grepl ("cruzeta", Capture.species.4) | grepl ("martilo", Capture.species.4) | grepl ("martillo", Capture.species.5, ignore.case = TRUE) | grepl ("cruzeta", Capture.species.5) | grepl ("martillo", Capture.species.6) | grepl ("cruzeta", Capture.species.6) | grepl ("martillo", Capture.species.7, ignore.case =TRUE) | grepl ("cruzeta", Capture.species.7))) # 23 trips

# 1548-- Zorritos 16, 1 mobula japanica measured. cruzetas, mobula, pampano
# 1568: Zorritos 2016, vela mobula perico cruzeta mantequero espada, no shark data
# 1571: Zorritos 2016, mobula, vela, barracuda, tuno, cruzeta, 3 mobula measured
# 1580 Ancon 2016, perico, atun, espada, barracuda, martillo, vela, diamante, no shark data
# 1581 Ancon 2016, perico, atun, barracuda, martillo, diamante, espada, no shark data
# 1591 Ancon 2016 perico atun vela martillo, barracuda diamante espada, no shark data
# 1592 Ancon 2016 espada perico vela atun barracuda martillo diamante, no shark data
# 1597 Ancon 2016 perico atun barracuda martilo espada vela
# 1598 Ancon 2016 perico atun espada barracuda martillo azul, no shark data
# 1599 Ancon 2016 perico atun barracuda, zorro azul diamante martillo
# 1600 Ancon 2016 perico atun barracuda diamante azul espada martillo, no shark data
# 1603 Ancon 2016 perico atun martillo espada diamante azul barracuda, no shark data
# 1604 Ancon 2016 perico atun baracuda diamante martillo vela, no shark data
# 1628 Ancon 2017 perico martillo atun barracuda diamante espada no shark data
# 1640 Ancon 2017 perico tuno diamante azul martillo merlin vela, no shark data
# 1643 Ancon 2017 PPA perico vela espada tuno martillo barracuda merlin, no shark data
# 1665 San Jose 2017 martillo, tons of tollo mama and raya aguila
# 1680 Ancon 2017 PPA perico tuno martillo vela zorro azul diamante, no shark data
# 1681 Ancon 2017 PPA perico barracuda vela tuno martillo azul diamante, no shark data
# 1688 Zorritos 2017 tuno martillo, no shark data
# 1693 Ancon 2017 PPA martillo azul gata raya, no shark data
# 1699 Ancon 2017 PPA diamante azul vela martillo merlin barracuda, no shark data
# 1767 Ancon 2017 PPA martillo azul bonito, no shark data

