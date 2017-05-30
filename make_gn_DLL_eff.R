#Make a file that has date, location, and "effort" for each set. 
# May 29 2017

### First, effort.

# Net characteristics calculated at trip level. I've standardized them (m) in probable_gillnets. 

gn.trip.eff <- read_csv ("probable_gillnets.csv") %>%
  mutate (net.length.est = panes * pane.1.length.std) %>%
  select (Trip.code, net.length.est)
## NOTE: this is assuming all panes are uniform; there are 53 trips for which this is not true. Units in m. Attempts to be more accurate below

# May 29: couldn't write csv of gn.sets. re run from make_probable_gillnets; current has errors in soak.time. 
gn.dll.eff <- gn.sets %>%
  select (Lance.code, Trip.code, Date, MY, Lat.I, Lon.I, Soak.time) %>%
  left_join (gn.trip.eff, by = "Trip.code") %>%
  mutate (km.day = net.length.est * Soak.time / 1000 / 24)








#### THIS ISN'T WORKING. Trying to calculate total length, with accurate panes, but it's only capturing the two trips with 3 pane types. 
gn.trip.eff <- select (gn.trip, 
                       Trip.code, pane.1, pane.1.length.std, pane.2, pane.2.length, pane.3, pane.3.length) %>% mutate (net.total.length = ifelse (pane.3 > 0, 
                                     pane.1 * pane.1.length.std + pane.2 * pane.2.length / 1.8288 + pane.3 * pane.3.length/1.8288, # convert to m bc they're all in b
                                     ifelse (pane.2 > 0,  
                                             pane.1 * pane.1.length.std + pane.2 * pane.2.length / 1.8288, 
                                             pane.1 * pane.1.length.std)))



gn.trip <- read_csv ("probable_gillnets.csv") %>%
  mutate (net.total.length =   ifelse (pane.2 > 0,  
                                       pane.1 * pane.1.length.std + pane.2 * pane.2.length / 1.8288, 
                                       pane.1 * pane.1.length.std))

with (gn.trip,
      if (gn.trip$pane.3 > 0) { 
        gn.trip$net.total.length ==  gn.trip$pane.1 * gn.trip$pane.1.length.std + gn.trip$pane.2 * gn.trip$pane.2.length / 1.8288 + gn.trip$pane.3 * gn.trip$pane.3.length/1.8288}
      else if (gn.trip$pane.2 > 0) {
        gn.trip$net.total.length == gn.trip$pane.1 * gn.trip$pane.1.length.std + gn.trip$pane.2 * gn.trip$pane.2.length / 1.8288}
      else gn.trip$net.total.length ==  gn.trip$pane.1 * gn.trip$pane.1.length.std
      )

