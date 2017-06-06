## Make annual time series catch data.sheet to perform DFA. 

## Want all species, with SpCd and seasons as columns. 

## First, bring all species together?

library (tidyverse)

gn.shark <- read.csv ("./Data_tables/gn_shark.csv")
gn.mamm <- read.csv ("./Data_tables/gn_mamm.csv")
gn.turt <- read.csv ("./Data_tables/gn_turt.csv")
gn.bird <- read.csv ("./Data_tables/gn_bird.csv")

gn.catch <- rbind (select (gn.shark, Species, Year, Lance.code), 
                   select (gn.mamm, Species, Year, Lance.code),
                   select (gn.turt, Species, Year, Lance.code),
                   select (gn.bird, Species, Year, Lance.code)) 
  

# Group by set, to calculate "cpue"
gn.catch.set <- gn.catch %>%
  group_by (Lance.code, Species) %>%
  summarise (TotCat = n(), Year = mean(Year, na.rm = TRUE)) %>%
  arrange (Year)

# Define top species. How about all known turtles and mammals, top 10 birds, top 10 sharks?

top.gn.sharks <- group_by (gn.shark, Species) %>%
  summarise (count = n()) %>%
  arrange (desc(count)) %>%
  top_n(10) # There's a reasonable drop off between 10 and 11--367 to 198.

top.gn.birds <- group_by (gn.bird, Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (4) # only 4 have more than 10

top.gn.mamm <- filter (gn.mamm, !Species %in% c(NA, "otro", "unk")) %>%
  group_by (Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (5)

top.gn.turt <- filter (gn.turt, !Species %in% c(NA, "Unk")) %>%
  group_by (Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (4)

gn.top.catch.set <- filter (gn.catch.set, Species %in% top.gn.sharks$Species | Species %in% top.gn.mamm$Species | Species %in% top.gn.turt$Species | Species %in% top.gn.birds$Species)


# Now group by year to make time series
gn.top.mn.ts <- group_by (gn.top.catch.set, Species, Year) %>%
  filter (Year > 2004) %>% # filter out NaN, start in 2005
  summarise (CPUE = mean (TotCat, na.rm = TRUE)) %>%
  spread (Year, CPUE) %>%
  mutate_all (funs (replace (., is.na(.), 0)))

write.csv (gn.top.mn.ts, file = "./Data_tables/Top_gn_DFA_ts.csv", row.names = FALSE)


###################################
# Also with just Salaverry

svry.trip <- read.csv ("./Data_tables/svry_trip.csv")

svry.shark <- filter (gn.shark, Trip.code %in% svry.trip$Trip.code)
svry.mamm <- filter (gn.mamm, Trip.code %in% svry.trip$Trip.code)
svry.turt <- filter (gn.turt, Trip.code %in% svry.trip$Trip.code)
svry.bird <- filter (gn.bird, Trip.code %in% svry.trip$Trip.code)

svry.catch <- rbind (select (svry.shark, Species, Year, Lance.code),
                     select (svry.mamm, Species, Year, Lance.code), 
                     select (svry.turt, Species, Year, Lance.code),
                     select (svry.bird, Species, Year, Lance.code))

svry.catch.set <- group_by (svry.catch, Species, Lance.code) %>%
  summarise (TotCat = n(), Year = mean (Year, na.rm = TRUE)) %>%
  filter (Year > 0)

top.svry.shark <- group_by (svry.shark, Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (6)

top.svry.mamm <- group_by (svry.mamm, Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (4)

top.svry.turt <- group_by (svry.turt, Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (4)

top.svry.bird <- group_by (svry.bird, Species) %>%
  summarise (count = n()) %>%
  arrange (desc (count)) %>%
  top_n (2)

svry.top.catch.set <- filter (svry.catch.set, Species %in% top.svry.shark$Species | Species %in% top.svry.mamm$Species | Species %in% top.svry.bird$Species | Species %in% top.svry.turt$Species) 

# Time series with species as rows and season as columns

svry.top.mn.ts <- group_by (svry.top.catch.set, Species, Year) %>%
  summarise (CPUE = mean (TotCat, na.rm = TRUE)) %>%
  spread (Year, CPUE) %>%
  mutate_all (funs (replace (., is.na(.), 0)))

write.csv (svry.top.mn.ts, file = "./Data_tables/Top_svry_DFA_ts.csv", row.names = FALSE)


###################################################
### RUN DFA

library (MARSS)

# try one sample
# 2 trends
# tmp.model.list <- list (m = 2, R = "equalvarcov")
# tmp.DFA.2 <- MARSS (dat.z.gn, model = tmp.model.list, z.score = TRUE, form = "dfa")
# tmp.fitname <- paste ("tmp.kemz", tmp.model.list$m, tmp.model.list$R, sep = ".")
# tmp.best.fit <- get (tmp.fitname)

cntl.list <- list (minit = 200, maxit = 5000, allow.degen = FALSE)

levels.R <- c ("diagonal and equal",
               "diagonal and unequal", 
               "equalvarcov", 
               "Unconstrained")

model.data.top.gn <- data.frame()

dat.z.gn <- as.matrix (gn.top.mn.ts[,-1]) # remove spp column
#write.csv (dat.z.gn, file = "./Data_tables/Top_gn_DFA_matrix.csv")

for (R in levels.R) { 
  for (m in 1:5) { # don't try more than 5 common trends
  dfa.model  <-  list (A = "zero", R = R, m = m)
  kemz <- MARSS (dat.z.gn, model = dfa.model, 
                control = cntl.list, form = "dfa", z.score = TRUE)
  model.data.top.gn  <-  rbind (model.data.top.gn,
                      data.frame (R = R, m = m,
                                  logLik = kemz$logLik,
                                  K = kemz$num.params,
                                  AICc = kemz$AICc,
                                  stringsAsFactors = FALSE))
  assign (paste ("kemz", m, R, sep = "."), kemz)
} # end m loop
} # end R loop

# make model table to summarize results

# Calculate delta-AICc (difference between model AICc and lowest AICc)
model.data.top.gn$delta.AICc = model.data.top.gn$AICc - min (model.data.top.gn$AICc)

# Calculate Akaike weights
wt = exp ( -0.5 * model.data.top.gn$delta.AICc)
model.data.top.gn$Ak.wt = wt / sum (wt)

# sort results
model.tbl.top.gn <- model.data.top.gn [ order (model.data.top.gn$AICc), -4]
# drop AICc from table

# calculate cumulative wts
model.tbl.top.gn$Ak.wt.cum <- cumsum (model.tbl.top.gn$Ak.wt)
model.tbl.top.gn <- model.tbl.top.gn[, -4]

# get the "best" model
best.model.top.gn  <-  model.tbl.top.gn[1,]
fitname.top.gn <- paste ("kemz", best.model.top.gn$m, best.model.top.gn$R, sep = ".")
best.fit.top.gn <- get (fitname.top.gn)


save (best.fit.top.gn, file = "./Output/DFA_top_gn_best_fit.RData")
save (best.model.top.gn, file = "./Output/DFA_top_gn_best_model.RData")
save (model.tbl.top.gn, file = "./Output/DFA_top_gn_model_table.RData")



#### Now for Salaverry

model.data.top.svry <- data.frame()

dat.z.svry <- as.matrix (svry.top.mn.ts[,-1]) # remove spp column

for (R in levels.R) { 
  for (m in 1:5) { # don't try more than 5 common trends
    dfa.model  <-  list (A = "zero", R = R, m = m)
    kemz <- MARSS (dat.z.svry, model = dfa.model, 
                   control = cntl.list, form = "dfa", z.score = TRUE)
    model.data.top.svry  <-  rbind (model.data.top.svry,
                                  data.frame (R = R, m = m,
                                              logLik = kemz$logLik,
                                              K = kemz$num.params,
                                              AICc = kemz$AICc,
                                              stringsAsFactors = FALSE))
    assign (paste ("kemz", m, R, sep = "."), kemz)
  } # end m loop
} # end R loop

# make model table to summarize results

# Calculate delta-AICc (difference between model AICc and lowest AICc)
model.data.top.svry$delta.AICc = model.data.top.svry$AICc - min (model.data.top.svry$AICc)

# Calculate Akaike weights
wt = exp ( -0.5 * model.data.top.svry$delta.AICc)
model.data.top.svry$Ak.wt = wt / sum (wt)

# sort results
model.tbl.top.svry <- model.data.top.svry [ order (model.data.top.svry$AICc), -4]
# drop AICc from table

# calculate cumulative wts
model.tbl.top.svry$Ak.wt.cum <- cumsum (model.tbl.top.svry$Ak.wt)
model.tbl.top.svry <- model.tbl.top.svry[, -4]

# get the "best" model
best.model.top.svry  <-  model.tbl.top.svry[1,]
fitname.top.svry <- paste ("kemz", best.model.top.svry$m, best.model.top.svry$R, sep = ".")
best.fit.top.svry <- get (fitname.top.svry)


save (best.fit.top.svry, file = "./Output/DFA_top_svry_best_fit.RData")
save (best.model.top.svry, file = "./Output/DFA_top_svry_best_model.RData")
save (model.tbl.top.svry, file = "./Output/DFA_top_svry_model_table.RData")
