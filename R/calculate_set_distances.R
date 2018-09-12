## Calculate set distance
# 9 11 2018

library (geosphere)

gn.sets <- read.csv ("Data_tables/probable_gn_sets.csv")

# missed one in lat final, 440_4. should be 6'17.2533333
gn.sets$Lat.F[which (gn.sets$Lance.code == "440_4")] <- -1 * (6 + 17.2533333/60)
 

p1 <- matrix (cbind (gn.sets$Lon.I, gn.sets$Lat.I), ncol = 2)
p2 <- matrix (cbind (gn.sets$Lon.F, gn.sets$Lat.F), ncol = 2)

dist <- distGeo (p1, p2)

summary (dist /110567)
