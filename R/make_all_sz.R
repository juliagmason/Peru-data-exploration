#### All hammerheads, with locations?

source ("./R/PD data cleaning.R")

gn_km_day <- read_csv ("./Data_tables/gn_km_day.csv", 
                       col_types = cols (Lance.code = col_character())) # character lance code to match sharks

all.sz <- filter (sharks, Species == "Martillo") %>%
  left_join (select (trips, Lugar.de.zarpe, puerto.de.llegada, Objective.species, Objective.species.2, Objective.species.3, Trip.code), by = "Trip.code") %>%
  left_join (gn_km_day, by = "Lance.code")

## what do I need to add to this? lugar de zarpe, puerto de llegada, objective spp, effort

# could pare down, but let's do that in the sharks project?

write.csv (all.sz, file = "../PDsharks/Data_tables/all_sz.csv", row.names = FALSE)


# btw, how does martillo rank overall?

sharks %>%
  group_by (Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count))
