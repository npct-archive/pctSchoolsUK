# Aim: subset the data to a region

source("R/analysis-sld.R")
las = readRDS("../pct-bigdata/las.Rds")
leeds_la = las[las$NAME == "Leeds",]
plot(leeds_la)
proj4string(sld_sp) = proj4string(las)
sld_leeds = sld_sp[leeds_la,]
points(sld_leeds)
saveRDS(sld_leeds, "private_data/sld_leeds.Rds")


