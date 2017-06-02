### Time series of shared spp in Peru and CA ###
# Julia  G Mason
# 11/2/2016

peru.sharks <- read.csv ("./Raw_data/Sharks_sep8.csv")

# DGN data

DN.catch <- read.csv ("DN_catch.csv")
DN.km.day <- read.csv ("DN_km_day.csv")
DN.catch <- left_join (DN.catch, DN.km.day, by = "Set.Code") %>% mutate (CPUE = TotCat / Km.Day)
DN.catch$Date <- as.Date (DN.catch$Date, format = "%Y-%m-%d")

DN.shark <- read.csv("Shark.csv")
DN.shark$Set.Code <- paste(DN.shark$TripNumber, paste(ifelse(DN.shark$Set < 10, 0, ""), DN.shark$Set, sep = ""), sep = ".")

DN.fish <- read.csv("Fish.csv")
DN.fish$Set.Code <- paste(DN.fish$TripNumber, paste(ifelse(DN.fish$Set < 10, 0, ""), DN.fish$Set, sep = ""), sep = ".")

# need a vector of the total effort for each month, not specific to species
DN.eff.mo <- summarise (group_by (DN.catch, year(Date), month (Date)), Eff = sum (Km.Day, na.rm = TRUE))
colnames (DN.eff.mo)[1:2]<- c("Year", "Month")

##### blue shark #######
DN.blue <- filter (DN.catch, SpCd == 167)

ggplot (DN.blue, aes (x = Date, y = CPUE)) + geom_line() + scale_x_date() + xlab("") + geom_smooth(se = FALSE) + ggtitle ("Prionace glauca caught in CA DGN, Ind/km day")

# CPUE averaged by month
# DN.blue.mo <- summarise (group_by (DN.blue, month(Date)), CPUE = mean(CPUE, na.rm = TRUE))
# colnames(DN.blue.mo)[1] <- "Month"
# 
# ggplot (DN.blue.mo, aes (x = Month, y = CPUE)) + geom_line() + xlab("") + ggtitle ("Monthly mean of Prionace glauca caught in CA DGN, Ind/km day")

# is this problematic to do a mean because I should have zeroes for all the sets that didn't catch blue sharks?

# summarize catch of individuals, summed by month
DN.blue.y.m <- as.data.frame(summarise (group_by (DN.blue, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.blue.y.m)[1:2] <- c("Year", "Month")

# add vector of total effort each month
DN.blue.y.m <- left_join(DN.blue.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

# do  I need to put it into a time series data frame, with months as columns and years as rows?

DN.blue.ts <- select (DN.blue.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) #%>% replace_na(DN.blue.ts = 0)
# replace NA with 0
DN.blue.ts[is.na(DN.blue.ts)] <- 0

colnames(DN.blue.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.blue.ts <- as.data.frame (append (DN.blue.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January
#plot.ts(DN.blue.ts)

# that's not quite right. I maybe need a vector that I then set as a ts object?
DN.blue.ts.vec <- as.vector (t (select (DN.blue.ts, -Year)))
DN.blue.ts <- ts (DN.blue.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.blue.ts, main = "P.glauca CPUE, DGN")

# decompose seasonal signal
DN.blue.components <- decompose (DN.blue.ts)

plot (DN.blue.components)

# raw number of individuals
ggplot (DN.blue, aes (x = Date, y = TotCat)) + geom_line() + ggtitle ("P. glauca in DGN, individuals") + abline()

DN.blue.ct.ts <- select (DN.blue.y.m, Year, Month, count) %>% spread (Month, count) #%>% replace_na(DN.blue.ts = 0)
# replace NA with 0
DN.blue.ct.ts[is.na(DN.blue.ct.ts)] <- 0

colnames(DN.blue.ct.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.blue.ct.ts <- as.data.frame (append (DN.blue.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months 
DN.blue.ct.ts.vec <- as.vector (t (select (DN.blue.ct.ts, -Year)))
DN.blue.ct.ts <- ts (DN.blue.ct.ts.vec, frequency = 12, start = c(1990, 1))
plot.ts (DN.blue.ct.ts, main = "P.glauca inds, DGN")

DN.blue.ct.components <- decompose (DN.blue.ct.ts)
plot (DN.blue.ct.components)

# average latitude caught?
DN.blue.lat <- summarise (group_by (DN.blue, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.blue.lat)[1] <- "Year"
ggplot (DN.blue.lat, aes (x = Year, y = Lat)) + geom_line() + ggtitle ("Mean latitude of P. Glauca catch, DGN")

# is this any different than mean lat of all sets?
DN.lat <- summarise (group_by (DN.set, year(Date)), Lat = mean(Lat1, na.rm = TRUE))
colnames(DN.lat)[1] <- "Year"

ggplot (DN.blue.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of P. Glauca catch, DGN")

# average FL?


Blue.FL <- summarise ( group_by (filter (DN.shark, SpCd == 167), Set.Code), mean.FL = mean(FrkLen, na.rm = TRUE))

DN.blue <- left_join (DN.blue, Blue.FL, by = "Set.Code")

ggplot (DN.blue, aes (x = Date, y = mean.FL)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean FL, P. glauca, DGN")


######### Shortfin mako ############

DN.mako <- filter (DN.catch, SpCd == 151)

DN.mako.y.m <- as.data.frame(summarise (group_by (DN.mako, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.mako.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.mako.y.m <- left_join(DN.mako.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.mako.ts <- select (DN.mako.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 
# replace NA with 0
DN.mako.ts[is.na(DN.mako.ts)] <- 0

colnames(DN.mako.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.mako.ts <- as.data.frame (append (DN.mako.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

# set as vector that I then set as a ts object
DN.mako.ts.vec <- as.vector (t (select (DN.mako.ts, -Year)))
DN.mako.ts <- ts (DN.mako.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.mako.ts, main = "I. oxyrinchus CPUE, DGN")

# decompose seasonal signal
DN.mako.components <- decompose (DN.mako.ts)

plot (DN.mako.components)


# raw number of individuals
ggplot (DN.mako, aes (x = Date, y = TotCat)) + geom_line() + ggtitle ("I. oxyrinchus in DGN, individuals") + abline()

DN.mako.ct.ts <- select (DN.mako.y.m, Year, Month, count) %>% spread (Month, count) 
# replace NA with 0
DN.mako.ct.ts[is.na(DN.mako.ct.ts)] <- 0

colnames(DN.mako.ct.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.mako.ct.ts <- as.data.frame (append (DN.mako.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months 
DN.mako.ct.ts.vec <- as.vector (t (select (DN.mako.ct.ts, -Year)))
DN.mako.ct.ts <- ts (DN.mako.ct.ts.vec, frequency = 12, start = c(1990, 1))
plot.ts (DN.mako.ct.ts, main = "I. oxyrinchus inds, DGN")

DN.mako.ct.components <- decompose (DN.mako.ct.ts)
plot (DN.mako.ct.components)

# average latitude caught:
DN.mako.lat <- summarise (group_by (DN.mako, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.mako.lat)[1] <- "Year"

ggplot (DN.mako.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of I. oxyrinchus catch, DGN")

# average FL?
mako.FL <- summarise ( group_by (filter (DN.shark, SpCd == 151), Set.Code), mean.FL = mean(FrkLen, na.rm = TRUE))

DN.mako <- left_join (DN.mako, mako.FL, by = "Set.Code")

ggplot (DN.mako, aes (x = Date, y = mean.FL)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean FL, I. oxyrinchus, DGN")

######### Thresher ############

DN.thresh <- filter (DN.catch, SpCd == 155)

DN.thresh.y.m <- as.data.frame(summarise (group_by (DN.thresh, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.thresh.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.thresh.y.m <- left_join(DN.thresh.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.thresh.ts <- select (DN.thresh.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 
# replace NA with 0
DN.thresh.ts[is.na(DN.thresh.ts)] <- 0

colnames(DN.thresh.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.thresh.ts <- as.data.frame (append (DN.thresh.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

# set as vector that I then set as a ts object
DN.thresh.ts.vec <- as.vector (t (select (DN.thresh.ts, -Year)))
DN.thresh.ts <- ts (DN.thresh.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.thresh.ts, main = "A. vulpinus CPUE, DGN")

# decompose seasonal signal
DN.thresh.components <- decompose (DN.thresh.ts)

plot (DN.thresh.components)


# raw number of individuals
DN.thresh.ct.ts <- select (DN.thresh.y.m, Year, Month, count) %>% spread (Month, count) 
# replace NA with 0
DN.thresh.ct.ts[is.na(DN.thresh.ct.ts)] <- 0

colnames(DN.thresh.ct.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.thresh.ct.ts <- as.data.frame (append (DN.thresh.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months 
DN.thresh.ct.ts.vec <- as.vector (t (select (DN.thresh.ct.ts, -Year)))
DN.thresh.ct.ts <- ts (DN.thresh.ct.ts.vec, frequency = 12, start = c(1990, 1))
plot.ts (DN.thresh.ct.ts, main = "A. vulpinus inds, DGN")

DN.thresh.ct.components <- decompose (DN.thresh.ct.ts)
plot (DN.thresh.ct.components)

# average latitude caught:
DN.thresh.lat <- summarise (group_by (DN.thresh, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.thresh.lat)[1] <- "Year"

ggplot (DN.thresh.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of A. vulpinus catch, DGN")

# average FL?
thresh.FL <- summarise ( group_by (filter (DN.shark, SpCd == 155), Set.Code), mean.FL = mean(FrkLen, na.rm = TRUE))

DN.thresh <- left_join (DN.thresh, thresh.FL, by = "Set.Code")

ggplot (DN.thresh, aes (x = Date, y = mean.FL)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean FL, A. vulpinus, DGN")


######### Swordfish ############

DN.SF <- filter (DN.catch, SpCd == 91)

DN.SF.y.m <- as.data.frame(summarise (group_by (DN.SF, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.SF.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.SF.y.m <- left_join(DN.SF.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.SF.ts <- select (DN.SF.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 
# replace NA with 0
DN.SF.ts[is.na(DN.SF.ts)] <- 0

colnames(DN.SF.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.SF.ts <- as.data.frame (append (DN.SF.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

# set as vector that I then set as a ts object
DN.SF.ts.vec <- as.vector (t (select (DN.SF.ts, -Year)))
DN.SF.ts <- ts (DN.SF.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.SF.ts, main = "X. gladius CPUE, DGN")

# decompose seasonal signal
DN.SF.components <- decompose (DN.SF.ts)

plot (DN.SF.components)


# raw number of individuals
DN.SF.ct.ts <- select (DN.SF.y.m, Year, Month, count) %>% spread (Month, count) 
# replace NA with 0
DN.SF.ct.ts[is.na(DN.SF.ct.ts)] <- 0

colnames(DN.SF.ct.ts)[2:10] <- c("Jan", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.SF.ct.ts <- as.data.frame (append (DN.SF.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months 
DN.SF.ct.ts.vec <- as.vector (t (select (DN.SF.ct.ts, -Year)))
DN.SF.ct.ts <- ts (DN.SF.ct.ts.vec, frequency = 12, start = c(1990, 1))
plot.ts (DN.SF.ct.ts, main = "X. gladius inds, DGN")

DN.SF.ct.components <- decompose (DN.SF.ct.ts)
plot (DN.SF.ct.components)

# average latitude caught:
DN.SF.lat <- summarise (group_by (DN.SF, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.SF.lat)[1] <- "Year"

ggplot (DN.SF.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of X. gladius catch, DGN")

######### Hammer shark ############

DN.Szyg <- filter (DN.catch, SpCd == 158)

DN.Szyg.y.m <- as.data.frame(summarise (group_by (DN.Szyg, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Szyg.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Szyg.y.m <- left_join(DN.Szyg.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Szyg.ts <- select (DN.Szyg.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 


colnames(DN.Szyg.ts)[2:5] <- c( "July", "Aug", "Sep", "Oct")
DN.Szyg.ts <- as.data.frame (append (DN.Szyg.ts, list("Jan" = 0, "Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "June" = 0), after = 1)) 
DN.Szyg.ts <- as.data.frame(append (DN.Szyg.ts, list("Nov" = 0, "Dec" = 0), after = 10))

# missing a lot of years. 
allYears <- as.data.frame(1990:2015)
colnames(allYears) <- "Year"


DN.Szyg.ts <- full_join (DN.Szyg.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Szyg.ts[is.na(DN.Szyg.ts)] <- 0

# set as vector that I then set as a ts object
DN.Szyg.ts.vec <- as.vector (t (select (DN.Szyg.ts, -Year)))
DN.Szyg.ts <- ts (DN.Szyg.ts.vec, frequency = 12, start = c(1992, 1))

plot.ts (DN.Szyg.ts, main = "S. zygaena CPUE, DGN")

# decompose seasonal signal
DN.Szyg.components <- decompose (DN.Szyg.ts)

plot (DN.Szyg.components)


# raw number of individuals 
DN.Szyg.ct.ts <- select (DN.Szyg.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Szyg.ct.ts)[2:5] <- c( "July", "Aug", "Sep", "Oct")
DN.Szyg.ct.ts <- as.data.frame (append (DN.Szyg.ct.ts, list("Jan" = 0, "Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "June" = 0), after = 1)) 
DN.Szygct.ct.ts <- as.data.frame(append (DN.Szyg.ct.ts, list("Nov" = 0, "Dec" = 0), after = 10))

DN.Szyg.ct.ts <- full_join (DN.Szyg.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Szyg.ct.ts[is.na(DN.Szyg.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Szyg.ct.ts.vec <- as.vector (t (select (DN.Szyg.ct.ts, -Year)))
DN.Szyg.ct.ts <- ts (DN.Szyg.ct.ts.vec, frequency = 12, start = c(1992, 1))

plot.ts (DN.Szyg.ct.ts, main = "S. zygaena inds, DGN")

# decompose seasonal signal
DN.Szyg.ct.components <- decompose (DN.Szyg.ct.ts)

plot (DN.Szyg.ct.components)


# average latitude caught:
DN.Szyg.lat <- summarise (group_by (DN.Szyg, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Szyg.lat)[1] <- "Year"

ggplot (DN.Szyg.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of S. zygaena catch, DGN")

# average FL?
Szyg.FL <- summarise ( group_by (filter (DN.shark, SpCd == 158), Set.Code), mean.FL = mean(FrkLen, na.rm = TRUE))

DN.Szyg <- left_join (DN.Szyg, Szyg.FL, by = "Set.Code")

ggplot (DN.Szyg, aes (x = Date, y = mean.FL)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean FL, S. zygaena, DGN")


######### Pelagic thresher ############
DN.Apel <- filter (DN.catch, SpCd == 148) # 42

DN.Apel.y.m <- as.data.frame(summarise (group_by (DN.Apel, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Apel.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Apel.y.m <- left_join(DN.Apel.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Apel.ts <- select (DN.Apel.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Apel.ts)[2:5] <- c( "Aug", "Sep", "Oct", "Nov")
DN.Apel.ts <- as.data.frame (append (DN.Apel.ts, list("Jan" = 0, "Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "June" = 0, "July" = 0), after = 1)) 
DN.Apel.ts <- as.data.frame(append (DN.Apel.ts, list("Dec" = 0), after = 10))

DN.Apel.ts <- full_join (DN.Apel.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Apel.ts[is.na(DN.Apel.ts)] <- 0

# set as vector that I then set as a ts object
DN.Apel.ts.vec <- as.vector (t (select (DN.Apel.ts, -Year)))
DN.Apel.ts <- ts (DN.Apel.ts.vec, frequency = 12, start = c(1992, 1))

plot.ts (DN.Apel.ts, main = "A. pelagicus CPUE, DGN")

# decompose seasonal signal
DN.Apel.components <- decompose (DN.Apel.ts)

plot (DN.Apel.components)


# raw number of individuals 
DN.Apel.ct.ts <- select (DN.Apel.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Apel.ct.ts)[2:5] <- c( "Aug", "Sep", "Oct", "Nov")
DN.Apel.ct.ts <- as.data.frame (append (DN.Apel.ct.ts, list("Jan" = 0, "Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "June" = 0, "July" = 0), after = 1)) 
DN.Apel.ct.ts <- as.data.frame(append (DN.Apel.ct.ts, list("Dec" = 0), after = 10))

DN.Apel.ct.ts <- full_join (DN.Apel.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Apel.ct.ts[is.na(DN.Apel.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Apel.ct.ts.vec <- as.vector (t (select (DN.Apel.ct.ts, -Year)))
DN.Apel.ct.ts <- ts (DN.Apel.ct.ts.vec, frequency = 12, start = c(1992, 1))

plot.ts (DN.Apel.ct.ts, main = "A. pelagicus inds, DGN")

# decompose seasonal signal
DN.Apel.ct.components <- decompose (DN.Apel.ct.ts)

plot (DN.Apel.ct.components)


# average latitude caught:
DN.Apel.lat <- summarise (group_by (DN.Apel, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Apel.lat)[1] <- "Year"

ggplot (DN.Apel.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of A. pelagicus catch, DGN")

# average FL?
Apel.FL <- summarise ( group_by (filter (DN.shark, SpCd == 148), Set.Code), mean.FL = mean(FrkLen, na.rm = TRUE))

DN.Apel <- left_join (DN.Apel, Apel.FL, by = "Set.Code")

ggplot (DN.Apel, aes (x = Date, y = mean.FL)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean FL, A. pelagicus, DGN")

######### Other sharks ############

DN.Ggal <- filter (DN.catch, SpCd == 159) # 7
DN.Saca <- filter (DN.catch, SpCd == 152) # 5
DN.Ncep <- filter (DN.catch, SpCd == 162) # 3

DN.manta <- filter (DN.catch, SpCd == 129) # 8

######## Other fish ############
DN.Chip <- filter (DN.catch, SpCd == 914) # 2
DN.Dgig <- filter (DN.catch, SpCd == 717) # 4
DN.Mind <- filter (DN.catch, SpCd == 90) # 2
DN.Sexi <- filter (DN.catch, SpCd == 476) # 2

#### A. rochei, bullet mackerel ###

DN.Aroc <- filter (DN.catch, SpCd == 19)

DN.Aroc.y.m <- as.data.frame(summarise (group_by (DN.Aroc, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Aroc.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Aroc.y.m <- left_join(DN.Aroc.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Aroc.ts <- select (DN.Aroc.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Aroc.ts)[2:8] <- c("Jan", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Aroc.ts <- as.data.frame (append (DN.Aroc.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "Jun" = 0), after = 2)) # add other months after January

DN.Aroc.ts <- full_join (DN.Aroc.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Aroc.ts[is.na(DN.Aroc.ts)] <- 0

# set as vector that I then set as a ts object
DN.Aroc.ts.vec <- as.vector (t (select (DN.Aroc.ts, -Year)))
DN.Aroc.ts <- ts (DN.Aroc.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Aroc.ts, main = "A. rochei CPUE, DGN")

# decompose seasonal signal
DN.Aroc.components <- decompose (DN.Aroc.ts)
plot (DN.Aroc.components)


# raw number of individuals
DN.Aroc.ct.ts <- select (DN.Aroc.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Aroc.ct.ts)[2:8] <- c("Jan", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Aroc.ct.ts <- as.data.frame (append (DN.Aroc.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "Jun" = 0), after = 2)) # add other months after January

DN.Aroc.ct.ts <- full_join (DN.Aroc.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Aroc.ct.ts[is.na(DN.Aroc.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Aroc.ct.ts.vec <- as.vector (t (select (DN.Aroc.ct.ts, -Year)))
DN.Aroc.ct.ts <- ts (DN.Aroc.ct.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Aroc.ct.ts, main = "A. rochei inds, DGN")

# decompose seasonal signal
DN.Aroc.ct.components <- decompose (DN.Aroc.ct.ts)
plot (DN.Aroc.ct.components)

# average latitude caught:
DN.Aroc.lat <- summarise (group_by (DN.Aroc, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Aroc.lat)[1] <- "Year"

ggplot (DN.Aroc.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of A. rochei catch, DGN")

### K. pelamis, skipjack tuna ####
DN.Kpel <- filter (DN.catch, SpCd == 2)

DN.Kpel.y.m <- as.data.frame(summarise (group_by (DN.Kpel, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Kpel.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Kpel.y.m <- left_join(DN.Kpel.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Kpel.ts <- select (DN.Kpel.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Kpel.ts)[2:8] <- c("Jan", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Kpel.ts <- as.data.frame (append (DN.Kpel.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "Jun" = 0), after = 2)) # add other months after January

DN.Kpel.ts <- full_join (DN.Kpel.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Kpel.ts[is.na(DN.Kpel.ts)] <- 0

# set as vector that I then set as a ts object
DN.Kpel.ts.vec <- as.vector (t (select (DN.Kpel.ts, -Year)))
DN.Kpel.ts <- ts (DN.Kpel.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Kpel.ts, main = "K. pelamis CPUE, DGN")

# decompose seasonal signal
DN.Kpel.components <- decompose (DN.Kpel.ts)
plot (DN.Kpel.components)

# raw number of individuals
DN.Kpel.ct.ts <- select (DN.Kpel.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Kpel.ct.ts)[2:8] <- c("Jan", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Kpel.ct.ts <- as.data.frame (append (DN.Kpel.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0, "May" = 0, "Jun" = 0), after = 2)) # add other months after January

DN.Kpel.ct.ts <- full_join (DN.Kpel.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Kpel.ct.ts[is.na(DN.Kpel.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Kpel.ct.ts.vec <- as.vector (t (select (DN.Kpel.ct.ts, -Year)))
DN.Kpel.ct.ts <- ts (DN.Kpel.ct.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Kpel.ct.ts, main = "K. pelamis inds, DGN")

# decompose seasonal signal
DN.Kpel.ct.components <- decompose (DN.Kpel.ct.ts)
plot (DN.Kpel.ct.components)

# average latitude caught:
DN.Kpel.lat <- summarise (group_by (DN.Kpel, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Kpel.lat)[1] <- "Year"

ggplot (DN.Kpel.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of K. pelamis catch, DGN")

# average length
Kpel.L <- summarise ( group_by (filter (DN.fish, SpCd == 2), Set.Code), mean.L = mean(Leng, na.rm = TRUE))

DN.Kpel <- left_join (DN.Kpel, Kpel.L, by = "Set.Code")

ggplot (DN.Kpel, aes (x = Date, y = mean.L)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean length, K. pelamis, DGN")

### S. chiliensis, bonito ####
DN.Schi <- filter (DN.catch, SpCd == 3) # 291

DN.Schi.y.m <- as.data.frame(summarise (group_by (DN.Schi, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Schi.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Schi.y.m <- left_join(DN.Schi.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Schi.ts <- select (DN.Schi.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Schi.ts)[2:10] <- c("Jan","May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Schi.ts <- as.data.frame (append (DN.Schi.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

DN.Schi.ts <- full_join (DN.Schi.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Schi.ts[is.na(DN.Schi.ts)] <- 0

# set as vector that I then set as a ts object
DN.Schi.ts.vec <- as.vector (t (select (DN.Schi.ts, -Year)))
DN.Schi.ts <- ts (DN.Schi.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Schi.ts, main = "S. chiliensis CPUE, DGN")

# decompose seasonal signal
DN.Schi.components <- decompose (DN.Schi.ts)
plot (DN.Schi.components)

# raw number of individuals
DN.Schi.ct.ts <- select (DN.Schi.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Schi.ct.ts)[2:10] <- c("Jan","May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Schi.ts <- as.data.frame (append (DN.Schi.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

DN.Schi.ct.ts <- full_join (DN.Schi.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Schi.ct.ts[is.na(DN.Schi.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Schi.ct.ts.vec <- as.vector (t (select (DN.Schi.ct.ts, -Year)))
DN.Schi.ct.ts <- ts (DN.Schi.ct.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Schi.ct.ts, main = "S. chiliensis inds, DGN")

# decompose seasonal signal
DN.Schi.ct.components <- decompose (DN.Schi.ct.ts)
plot (DN.Schi.ct.components)

# average latitude caught:
DN.Schi.lat <- summarise (group_by (DN.Schi, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Schi.lat)[1] <- "Year"

ggplot (DN.Schi.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of S. chiliensis catch, DGN")

# average length
Schi.L <- summarise ( group_by (filter (DN.fish, SpCd == 3), Set.Code), mean.L = mean(Leng, na.rm = TRUE))

DN.Schi <- left_join (DN.Schi, Schi.L, by = "Set.Code")

ggplot (DN.Schi, aes (x = Date, y = mean.L)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean length, S. chiliensis, DGN")


### S. japonicus, pacific mackerel ###
DN.Sjap <- filter (DN.catch, SpCd == 51) # 801

DN.Sjap.y.m <- as.data.frame(summarise (group_by (DN.Sjap, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Sjap.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Sjap.y.m <- left_join(DN.Sjap.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Sjap.ts <- select (DN.Sjap.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Sjap.ts)[2:10] <- c("Jan","May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Sjap.ts <- as.data.frame (append (DN.Sjap.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

DN.Sjap.ts <- full_join (DN.Sjap.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Sjap.ts[is.na(DN.Sjap.ts)] <- 0

# set as vector that I then set as a ts object
DN.Sjap.ts.vec <- as.vector (t (select (DN.Sjap.ts, -Year)))
DN.Sjap.ts <- ts (DN.Sjap.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Sjap.ts, main = "S. japonicus CPUE, DGN")

# decompose seasonal signal
DN.Sjap.components <- decompose (DN.Sjap.ts)
plot (DN.Sjap.components)

# raw number of individuals
DN.Sjap.ct.ts <- select (DN.Sjap.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Sjap.ct.ts)[2:10] <- c("Jan","May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Sjap.ts <- as.data.frame (append (DN.Sjap.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January

DN.Sjap.ct.ts <- full_join (DN.Sjap.ct.ts, allYears, by = "Year") %>% arrange (Year)
# replace NA with 0
DN.Sjap.ct.ts[is.na(DN.Sjap.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Sjap.ct.ts.vec <- as.vector (t (select (DN.Sjap.ct.ts, -Year)))
DN.Sjap.ct.ts <- ts (DN.Sjap.ct.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Sjap.ct.ts, main = "S. japonicus inds, DGN")

# decompose seasonal signal
DN.Sjap.ct.components <- decompose (DN.Sjap.ct.ts)
plot (DN.Sjap.ct.components)

# average latitude caught:
DN.Sjap.lat <- summarise (group_by (DN.Sjap, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Sjap.lat)[1] <- "Year"

ggplot (DN.Sjap.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of S. japonicus catch, DGN")
# average length
Sjap.L <- summarise ( group_by (filter (DN.fish, SpCd == 51), Set.Code), mean.L = mean(Leng, na.rm = TRUE))

DN.Sjap <- left_join (DN.Sjap, Sjap.L, by = "Set.Code")

ggplot (DN.Sjap, aes (x = Date, y = mean.L)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean length, S. japonicus, DGN")

#### T alalungus, albacore ###

DN.Tala <- filter (DN.catch, SpCd == 5) # 2313

DN.Tala.y.m <- as.data.frame(summarise (group_by (DN.Tala, year(Date), month(Date)), count = sum(TotCat)))
colnames(DN.Tala.y.m)[1:2] <- c("Year", "Month")

# add total km.day to calculate CPUE
DN.Tala.y.m <- left_join(DN.Tala.y.m, DN.eff.mo, by = c("Year" = "Year", "Month" = "Month")) %>% mutate (CPUE = count / Eff)

DN.Tala.ts <- select (DN.Tala.y.m, Year, Month, CPUE) %>% spread (Month, CPUE) 

colnames(DN.Tala.ts)[2:8] <- c("Jan","May", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Tala.ts <- as.data.frame (append (DN.Tala.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January
DN.Tala.ts <- as.data.frame (append (DN.Tala.ts, list("June" = 0, "July" = 0), after = 6))

# replace NA with 0
DN.Tala.ts[is.na(DN.Tala.ts)] <- 0

# set as vector that I then set as a ts object
DN.Tala.ts.vec <- as.vector (t (select (DN.Tala.ts, -Year)))
DN.Tala.ts <- ts (DN.Tala.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Tala.ts, main = "T. alalunga CPUE, DGN")

# decompose seasonal signal
DN.Tala.components <- decompose (DN.Tala.ts)
plot (DN.Tala.components)

# raw number of individuals
DN.Tala.ct.ts <- select (DN.Tala.y.m, Year, Month, count) %>% spread (Month, count) 

colnames(DN.Tala.ct.ts)[2:8] <- c("Jan","May", "Aug", "Sep", "Oct", "Nov", "Dec")
DN.Tala.ct.ts <- as.data.frame (append (DN.Tala.ct.ts, list("Feb" = 0, "Mar" = 0, "Apr" = 0), after = 2)) # add other months after January
DN.Tala.ct.ts <- as.data.frame (append (DN.Tala.ct.ts, list("June" = 0, "July" = 0), after = 6))

# replace NA with 0
DN.Tala.ct.ts[is.na(DN.Tala.ct.ts)] <- 0

# set as vector that I then set as a ts object
DN.Tala.ct.ts.vec <- as.vector (t (select (DN.Tala.ct.ts, -Year)))
DN.Tala.ct.ts <- ts (DN.Tala.ct.ts.vec, frequency = 12, start = c(1990, 1))

plot.ts (DN.Tala.ct.ts, main = "T. alalunga inds, DGN")

# decompose seasonal signal
DN.Tala.ct.components <- decompose (DN.Tala.ct.ts)
plot (DN.Tala.ct.components)

# average latitude caught:
DN.Tala.lat <- summarise (group_by (DN.Tala, year(Date)), Lat = weighted.mean (Lat1, TotCat, na.rm = TRUE)) # weight by # inds caught
colnames (DN.Tala.lat)[1] <- "Year"

ggplot (DN.Tala.lat, aes (x = Year, y = Lat)) + geom_line() + geom_line(data = DN.lat, aes (x = Year, y = Lat), col = "red") + ggtitle ("Mean latitude of T. alalunga catch, DGN")
# average length
Tala.L <- summarise ( group_by (filter (DN.fish, SpCd == 5), Set.Code), mean.L = mean(Leng, na.rm = TRUE))

DN.Tala <- left_join (DN.Tala, Tala.L, by = "Set.Code")

ggplot (DN.Tala, aes (x = Date, y = mean.L)) + geom_line() + geom_smooth(se = FALSE) + ggtitle ("Mean length, T. alalunga, DGN")
