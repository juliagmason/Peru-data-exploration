#### Calculate CPUE in the DGN
# Julia G Mason
# 11/2/2016

# calculate km day for each set. DN_set$Soak is soak time in hours, and NetLength is length in fathoms. 

DN.set <- read.csv ("DN_set.csv")
DN.set$Set.Code <- paste(DN.set$TripNumber, paste(ifelse(DN.set$Set < 10, 0, ""), DN.set$Set, sep = ""), sep = ".")

DN.net <- read.csv ("Net.csv")
DN.net$Set.Code <- paste(DN.net$TripNumber, paste(ifelse(DN.net$Set < 10, 0, ""), DN.net$Set, sep = ""), sep = ".")
DN.net <- filter (DN.net, Set.Code %in% DN.set$Set.Code) # only DGN

names (DN.net) [names(DN.net)== "Lengh"] <- "Length" # misspelled 


# definite error in DN-SD-1320.1, section 1. 800, whereas this is 80 in every other section.This is row 14129 in excel. 
DN.net[14128, "Length"] <- 80

# nets have multiple sections, recorded as their own row. I don't care about the rest of the information for now, so I'm just going to summarise by lance.code and sum the lengths. 

net.km <- summarise (group_by(DN.net, Set.Code), Length = sum (Length) * 0.0018288) # convert from fathoms to km

# dotchart(net.km$Length)
# View (net.km[ which (net.km$Length > 1.9),])

DN.km.day <- select (DN.set, Set.Code, Soak) %>% left_join (net.km, by = "Set.Code") %>% mutate (Km.Day = Soak * Length / 24)

write.csv (DN.km.day, file = "DN_km_day.csv")
