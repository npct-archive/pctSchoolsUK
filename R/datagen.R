# Aim: generate data for the schools app

source("setup.R")

rf = readRDS("private_data/leeds_rf.Rds")
rq = readRDS("private_data/leeds_rq.Rds")
schools = readRDS("private_data/sld_leeds.Rds")
las = readRDS("private_data/leeds_rq.Rds")
l = readRDS("private_data/leeds_uptakes.Rds")
rf@data = cbind(l@data, rf@data)

# rf = rf[rf$TOTAL > 5,] # cut at 5 for fast processing
# rnet = overline(sldf = rf, attrib = "bicycle", fun = sum)
# plot(rnet, lwd = rnet$bicycle / mean(rnet$bicycle))
# rnet = rnet[rnet$bicycle > 5,] # cut if needs be

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
