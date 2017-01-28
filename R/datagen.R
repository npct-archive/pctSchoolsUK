# Aim: generate data for the schools app

source("setup.R")

l_nat = readRDS("private_data/l_national.Rds")
l = readRDS("private_data/l_leeds.Rds")
rf = readRDS("private_data/leeds_rf.Rds")
rq = readRDS("private_data/leeds_rq.Rds")
sel = l$TOTAL > 50

# # subset for testing
# sum(sel) # 115 lines
# l = l[sel,]
# rf = rf[sel,]
# rq = rq[sel,]

schools = readRDS("private_data/sld_leeds.Rds")
las = readRDS("private_data/las_2011.Rds")
cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")
cents_lsoa_leeds = cents_lsoa[grepl("Leeds", cents_lsoa$LSOA11NM),]
names(cents_lsoa_leeds) = c("geo_code",                 "geo_label")
saveRDS(cents_lsoa_leeds, "pctSchoolsApp/c.Rds")

las_leeds = las[cents_lsoa_leeds,]
download.file("http://geoportal.statistics.gov.uk/datasets/da831f80764346889837c72508f046fa_3.zip",
              "lsoas.zip")
unzip("lsoas.zip", exdir = "private_data/")
lsoas = raster::shapefile("private_data/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
lsoas = spTransform(lsoas, proj4string(cents_lsoa))
summary(cents_lsoa_leeds$geo_code %in% lsoas$lsoa11cd)
lsoas@data = rename(lsoas@data, geo_code = lsoa11cd)
lsoas = lsoas[grep("E", lsoas$geo_code),]

# get 2001 lsoas
u2001 = "http://geoportal.statistics.gov.uk/datasets/90e15daaaeef4baa877f4bffe01ebef0_2.zip"
download.file(u2001, "lsoa2001.zip")
unzip("lsoa2001.zip", exdir = "private_data/")
lsoas_2001 = raster::shapefile("private_data/Lower_Layer_Super_Output_Areas_December_2001_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
lsoas_2001@data = rename(lsoas_2001@data, geo_code = lsoa01cd)
lsoas_2001 = lsoas_2001[grep("E", lsoas$geo_code),]

# subset by cents code name
lsoas_leeds = lsoas[lsoas$geo_code %in% cents_lsoa_leeds$geo_code,]
# subset by name
lsoas_leeds = lsoas[grepl("Leeds", lsoas$lsoa11nm),]

mapview::mapview(lsoas_leeds)
ldf = l_nat@data
lsoa_data = ldf %>% group_by(LSOA) %>% 
  summarise(
    all = sum(TOTAL),
    bicycle = sum(CYCLE),
    car = sum(CAR),
    foot = sum(WALK),
    govtarget_slc = sum(govtarget_slc),
    dutch_slc = sum(dutch_slc)
  )

lsoa_data = rename(lsoa_data, geo_code = LSOA)
# convert matrices to numerics
unmatrix_df = function(df){
  for(i in 1:ncol(df)){
    df[[i]] = c(df[[i]])
  }
  df
}
lsoa_data = unmatrix_df(lsoa_data)
lsoa_newdat = left_join(lsoas_leeds@data, lsoa_data)
nrow(lsoa_newdat) # left join keeps all rows
summary(lsoas_leeds$geo_code == lsoa_newdat$geo_code)
lsoas_leeds@data = lsoa_newdat
qtm(lsoas_leeds, "all")

# explore missing LSOAs
(lsoa_missing = lsoas_leeds$geo_code[is.na(lsoas_leeds$bicycle)]) 
lsoa_missing %in% l_nat$LSOA
# check in original data
s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
summary({lmissing_orig = lsoas_leeds$geo_code %in% s11$LLSOA_SPR11}) # 42 missing
summary(lsoas$geo_code %in% s11$LLSOA_SPR11)
summary(lsoas_2001$geo_code %in% s11$LLSOA_SPR11)
(lsoas_leeds$geo_code[lmissing_orig & is.na(lsoas_leeds$all)])
saveRDS(lsoas_leeds, "pctSchoolsApp/z.Rds")

# l = readRDS("private_data/Data_to_be_subset_for_Shiny_app.Rds")
rf@data = cbind(l@data, rf@data)
rf$bicycle = rf$CYCLE



rnet = overline(sldf = rf, attrib = "bicycle", fun = sum)
rnet_total = overline(sldf = rf, attrib = "TOTAL")
rnet_govtarget = overline(sldf = rf, attrib = "govtarget_slc", fun = sum)
rnet_dutch = overline(sldf = rf, attrib = "dutch_slc", fun = sum)
rnet$govtarget_slc = rnet_govtarget$govtarget_slc
rnet$dutch_slc = rnet_dutch$dutch_slc
rnet$total = rnet_total$TOTAL
summary(rnet)

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
