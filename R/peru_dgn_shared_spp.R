### PD data processing for time series of species overlapping with CA DGN ###
# Julia G Mason 10/31/2016


library(tidyverse)

# which species are caught in peru and also the CA DGN?

peru.catch <- read.csv("./Raw_data/trips_main_fixed.csv")
peru.new <- read.csv("./Raw_data/trips_sep8.csv") %>%  filter (!Trip.Code %in% peru.catch$Trip.code)
colnames(peru.new) <- colnames(peru.catch) # some capitals were changed
peru.catch <- rbind (peru.catch, peru.new)

# created table of scientific names I could find
peru.sci.names <- read.csv ("./Data_tables/Peru_species_sci_names.csv")

# merge genus and spp column 
peru.sci.names <- unite (peru.sci.names, ScientificName, Sci_genus, Sci_spp, sep = " ", remove = FALSE)

# california
dgn.catch <- read.csv("DN_catch.csv")
CA.spp <- read.csv("SpList.csv")

# not all of the spp in SpList are caught in DGN. 
dgn.spp <- filter (CA.spp, SpCd %in% dgn.catch$SpCd)

# which species are in both?
spp.overlap <- filter (dgn.spp, ScientificName %in% peru.sci.names$ScientificName)

########
#bycatch -- turtles and mammals and birds

dgn.mtb <- read.csv ("Life.csv")
dgn.mtb.spp <- filter (CA.spp, SpCd %in% dgn.mtb$SpCd)

# still need to add peru species names, maybe just in a vector once I have them. Written in coding notes document. 
