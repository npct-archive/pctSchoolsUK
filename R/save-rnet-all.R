rnet_dutch = readRDS("/tmp/rnet_dutch.Rds")
rnet_govtarget = readRDS("/tmp/rnet_govtarget.Rds")
rnet_total = readRDS("/tmp/rnet_total.Rds")
rnet = readRDS("/tmp/rnet.Rds")

rnet$total = rnet_total$TOTAL
rnet$govtarget_slc = rnet_govtarget$govtarget_slc
rnet$dutch_slc = rnet_dutch$dutch_slc
summary(rnet)

plot(rnet, lwd = sqrt(rnet$dutch_slc / mean(rnet$bicycle)))
plot(rnet, lwd = sqrt(rnet$govtarget_slc / mean(rnet$bicycle)), add = T, col = "white")
plot(rnet, lwd = sqrt(rnet$bicycle / mean(rnet$bicycle)), add = T, col = "red")
summary(rnet$bicycle)
rnet = rnet[rnet$bicycle > 5,] # cut if needs be
saveRDS(rnet, "pctSchoolsApp/rf_leeds_schools_all.Rds")
