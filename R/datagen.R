# Aim: generate data for the schools app

source("setup.R")

l = readRDS("private_data/l_leeds.Rds")
rf = readRDS("private_data/leeds_rf.Rds")
rq = readRDS("private_data/leeds_rq.Rds")
schools = readRDS("private_data/sld_leeds.Rds")
las = readRDS("private_data/las_2011.Rds")
cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")
cents_lsoa_leeds = cents_lsoa[grepl("Leeds", cents_lsoa$LSOA11NM),]
las_leeds = las[cents_lsoa_leeds,]
# l = readRDS("private_data/Data_to_be_subset_for_Shiny_app.Rds")
# rf@data = cbind(l@data, rf@data)

rnet = overline(sldf = rf, attrib = "bicycle", fun = sum)
rnet_govtarget = overline(sldf = rf, attrib = "govtarget_slc", fun = sum)
rnet_dutch = overline(sldf = rf, attrib = "dutch_slc", fun = sum)
rnet$govtarget_slc = rnet_govtarget$govtarget_slc
rnet$dutch_slc = rnet_dutch$dutch_slc

plot(rnet, lwd = rnet$dutch_slc / mean(rnet$bicycle))
plot(rnet, lwd = rnet$govtarget_slc / mean(rnet$bicycle), add = T, col = "white")
plot(rnet, lwd = rnet$bicycle / mean(rnet$bicycle), add = T, col = "red")
summary(rnet$bicycle)
rnet = rnet[rnet$bicycle > 5,] # cut if needs be
saveRDS(rnet, "pctSchoolsApp/rf_leeds_schools.Rds")

# use points method
devtools::install_github("robinlovelace/stplanr", ref = "sfr-dep")
rnet_points_current = stplanr::line_sample(l = rf, n = 10000, weights = rf$CYCLE) # needs fixing
rnet_points_bicycle = stplanr::line_sample(l = rf, n = 10000, weights = rf$CYCLE) # needs fixing
rnet_points_govtarget = stplanr::line_sample(l = rf, n = 10000, weights = rf$CYCLE) # needs fixing
rnet_points_godutch = stplanr::line_sample(l = rf, n = 10000, weights = rf$CYCLE) # needs fixing
length(rnet_points)
plot(rnet_points)
sel = sample(nrow(l), 100) # test data
plot(rf[sel,])
plot(l[sel,], add = T)


# # subset schools
# sel = l$SCHOOLNAME_SPR11 %in% schools$LEA11_SchoolName
# l = l[sel,]
# rf = rf[sel,]
# bbox(rf)
# bbox(l)
# plot(l)
# plot(rf, add = T)

# # Add scenario numbers (random atm)
# rf$bicycle = runif(n = nrow(rf), min = 0, max = 5)
# rf$govtarget_slc = rf$bicycle * 2
# rf$govtarget_slc = rf$bicycle * 10

# # generate dummy data
rf$bicycle = round(runif(n = nrow(rf), min = 0, max = 3))
rf$govtarget_slc = rf$bicycle * 2
rf$dutch_slc = rf$bicycle * 10
# rf = rf[rf$TOTAL > 5,] # cut at 5 for fast processing
proj4string(rf)
