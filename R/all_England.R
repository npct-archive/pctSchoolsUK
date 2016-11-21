# Aim: subset the data to a region

# Read in and pre-process the data
if(!exists("sld11"))
  source("R/analysis-sld.R")

# Read in UK Local Authorities, subset Leeds only
#las = readRDS("../pct-bigdata/las.Rds") ## 2001 LSOAs
if(!file.exists("private_data/las_2011.Rds")){
  # Simplified LSOA boundaries
  las = shapefile("private_data/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales_SIMPLIFIED/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp")
  # Full LSOA boundaries
  #las = shapefile("Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp")
  las = spTransform(las, CRS("+init=epsg:4326"))
  saveRDS(las, "private_data/las_2011.Rds")
} else{
  las = readRDS("private_data/las_2011.Rds")
}

#sld_test = sld_sp
#sld_test@coords = sld_sp[(sld_sp@coords[,1] != 654558) & (sld_sp@coords[,2] != 653613),]

#leeds_la = las[las$NAME == "Leeds",]
#plot(leeds_la)
#plot(las)
# Match CRS of spatial data to that of Local Authorities data
#proj4string(sld_sp) = CRS("+init=epsg:27700")
#proj4string(sld_sp) = proj4string(las)
#sld_sp = spTransform(sld_sp, CRSobj = proj4string(las))
# Subset schools data to Leeds
#schools = sld_sp[leeds_la,]
#points(schools)

#schools = spTransform(schools, CRSobj = )
if(!file.exists("private_data/sld_england.Rds")){
  schools = spTransform(sld_sp, CRSobj = proj4string(las))
  saveRDS(schools, "private_data/sld_england.Rds")
} else{
  schools = readRDS("private_data/sld_england.Rds")
}
# subset all ods that go to these schools

# Get the centroids for UK LSOAs
#cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
if(!file.exists("private_data/cents_lsoa_2011.Rds")){
  cents_lsoa = shapefile("private_data/lower_layer_super_output_areas_(e+w)_2011_population_weighted_centroids_v2/LSOA_2011_EW_PWC.shp")
  #cents_lsoa = spTransform(cents_lsoa, CRS("+init=epsg:4326"))
  cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))
  #proj4string(cents_lsoa) = proj4string(las)
  saveRDS(cents_lsoa, "private_data/cents_lsoa_2011.Rds")
} else{
  cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")
}


# Match CRS of LSOAs to be consistent with las and schools

#plot(cents_lsoa)
#bbox(cents_lsoa)


#Subset centroids of LSOAs to Leeds only
#cents_lsoa = cents_lsoa[leeds_la,]

# subset the flow (Origin-Destination) data
# select only LSOA centroids in the Leeds-only centroid data
#sel_od = s11$LLSOA_SPR11 %in% cents_lsoa$LSOA11CD
#s = s11[sel_od,]

nrow(s11)
s = s11[!is.na(s11$LLSOA_SPR11),] # drop flows with no LSOA data
nrow(s)
s = s[s$URN_SPR11 %in% schools$URN,] # drop flows with no school data
nrow(s)

# we want to plot LSOA centroid --> school desire lines
# the stplanr::od2line() function needs matching column titles to join
# between the zones (origins), flows (origin-destination), and destination columns
# If no matching column names are found origin is matched to first column of flows
# and destination is matched to second column of flows.
# Rearrage our dataframes to match this
# origins (LSOAs) matched to flows on LSOA, flows matched to destinations (schools) on URN
names(cents_lsoa)
names(s)
names(schools)
names(cents_lsoa)[names(cents_lsoa)=="LSOA11CD"] <- "LSOA"
names(s)[names(s)=="LLSOA_SPR11"] <- "LSOA"
names(s)[names(s)=="URN_SPR11"] <- "URN"
#names(schools)[names(schools)=="LEA11_URN"] <- "URN"

#names(cents_lsoa)[1] = names(s)[1]
#names(schools)[2] = names(s)[3]

# Some LSOA centroids are wrong and are in the ocean. Remove the ones not within LSOAs
cents_lsoa = cents_lsoa[las,]
nrow(cents_lsoa)
schools = schools[las,]
nrow(schools[las,])

summary(s$LSOA %in% cents_lsoa$LSOA)
summary(s$URN %in% schools$URN)

s = s[s$LSOA %in% cents_lsoa$LSOA,]
s = s[s$URN %in% schools$URN,]

#nrow(cents_lsoa)
#nrow(schools)
#cents_lsoa = cents_lsoa[cents_lsoa$LSOA %in% s$LSOA,]
#schools = schools[schools$URN %in% s$URN,]
#nrow(cents_lsoa)
#nrow(schools)

#plot(las)
#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)

summary(s$TOTAL)
nrow(s)
s_cut = s[s$TOTAL >= 10,]
nrow(s_cut)

# plot(las); plot(schools, col="red", pch='.', cex=2, add=T)
# remove year as 1st column
AcademicYear = schools$AcademicYear
schools$AcademicYear = NULL
schools@data = cbind(schools@data, AcademicYear)
rm(AcademicYear)

# Test matching
zone_code = s_cut$LSOA
origin_code = cents_lsoa$LSOA
dest_code = s_cut$URN
zone_code_d = schools$URN
summary(zone_code %in% origin_code)
summary(dest_code %in% zone_code_d)


if(!file.exists("private_data/england_flows.Rds")){
  starttime <- proc.time()
  flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools,
                 zone_code = "LSOA", origin_code = "LSOA", dest_code = "URN",
                 zone_code_d = "URN")
  print(proc.time() - starttime)
  saveRDS(flow, "private_data/england_flows.Rds")
} else{
  flow = readRDS("private_data/england_flows.Rds")
}

unique(schools$`LA (name)`)

# # Use the spatial dataframe with the full set of schools to do the spatial subsetting
# # (to get all of Leeds, not just LSOAs where there are Secondary schools)
# leeds_met_area = c("Leeds", "Wakefield","Kirklees","Calderdale","Bradford","Barnsley","Craven","Harrogate","Selby","York")
# summary(unique(sldfull_sp$`LA (name)`) %in% leeds_met_area)
# schools_leeds = sldfull_sp[sldfull_sp$`LA (name)` %in% leeds_met_area, ]
# #schools_leeds = sldfull_sp[sldfull_sp$`LA (name)`=="Leeds", ]
# schools_leeds = spTransform(schools_leeds, CRSobj = proj4string(las))
# las_leeds = las[schools_leeds,]
# cents_lsoa_leeds = cents_lsoa[schools_leeds,]

#######################################################################
### JUST BRADFORD

cents_lsoa_brad = cents_lsoa[grepl("Bradford", cents_lsoa$LSOA11NM),]
las_brad = las[cents_lsoa_brad,]
schools_brad = schools[las_brad,]

# TODO: fix this - not working! # ILAN: This works for me! Weird..
sel_flow_brad = gWithin(flow, las_brad, byid = TRUE)
bbox_brad = stplanr::bb2poly(bb = bbox(las_brad))
proj4string(bbox_brad) = proj4string(flow)
flow_brad = flow[bbox_brad,]
flow_brad = flow_brad[las_brad,]
plot(las_brad); lines(flow_brad, pch='.', col="blue", cex=5); points(schools_brad, pch='.', col="red", cex=5)


#######################################################################
### JUST CAMBRIDGE


cents_lsoa_cam = cents_lsoa[grepl("\\<Cambridge\\>", cents_lsoa$LSOA11NM),]
las_cam = las[cents_lsoa_cam,]
schools_cam = schools[las_cam,]

# TODO: fix this - not working! # ILAN: This works for me! Weird..
sel_flow_cam = gWithin(flow, las_cam, byid = TRUE)
bbox_cam = stplanr::bb2poly(bb = bbox(las_cam))
proj4string(bbox_cam) = proj4string(flow)
flow_cam = flow[bbox_cam,]
flow_cam = flow_cam[las_cam,]

#plot(las_cam); lines(flow_cam, pch='.', col="blue", cex=5); points(schools_cam, pch='.', col="red", cex=5)


######################################################################
# LEEDS

cents_lsoa_leeds = cents_lsoa[grepl("Leeds", cents_lsoa$LSOA11NM),]
las_leeds = las[cents_lsoa_leeds,]
schools_leeds = schools[las_leeds,]

# TODO: fix this - not working! # ILAN: This works for me! Weird..
sel_flow_leeds = gWithin(flow, las_leeds, byid = TRUE)
bbox_leeds = stplanr::bb2poly(bb = bbox(las_leeds))
proj4string(bbox_leeds) = proj4string(flow)
flow_leeds = flow[bbox_leeds,]
flow_leeds = flow_leeds[las_leeds,]

#plot(las_leeds); lines(flow_leeds, pch='.', col="blue", cex=5); points(schools_leeds, pch='.', col="red", cex=5)

if(!file.exists("private_data/rf_leeds_schools.Rds") | !file.exists("private_data/rq_leeds_schools.Rds")){
  fast_routes_leeds = line2route(l = flow_leeds, route_fun = route_cyclestreet, plan = "fastest")
  quiet_routes_leeds = line2route(l = flow_leeds, route_fun = route_cyclestreet, plan = "quietest")
  saveRDS(fast_routes_leeds, "private_data/rf_leeds_schools.Rds")
  saveRDS(quiet_routes_leeds, "private_data/rq_leeds_schools.Rds")
} else{
  fast_routes_leeds = readRDS("private_data/rf_leeds_schools.Rds")
  quiet_routes_leeds = readRDS("private_data/rq_leeds_schools.Rds")
}

plot(las_leeds); plot(flow_leeds, col="blue", add=T); plot(fast_routes_leeds, col="red", add = T); plot(quiet_routes_leeds, col="green", add = T)

summary(fast_routes_leeds$length)
summary(quiet_routes_leeds$length)

mean(fast_routes_leeds$length)
sd(fast_routes_leeds$length)
IQR(fast_routes_leeds$length)

mean(quiet_routes_leeds$length)
sd(quiet_routes_leeds$length)
IQR(quiet_routes_leeds$length)

boxplot(quiet_routes_leeds$length, fast_routes_leeds$length)

violindf1 = data.frame(Type="Quiet", Length=quiet_routes_leeds$length)
violindf2 = data.frame(Type="Fast", Length=fast_routes_leeds$length)
violindf = rbind(violindf1, violindf2)
violindf

# Mode = function(x) {
#   ux = unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

hist(quiet_routes_leeds$length)
hist(fast_routes_leeds$length)

library(ggplot2)
ggplot(violindf, aes(Type, Length)) + geom_violin(trim=T, fill="blue", alpha=0.4) + geom_boxplot(width=0.1, outlier.size = 2, outlier.color = "red", outlier.shape = 21) + stat_summary(fun.y=mean, geom="point", shape=18, color="blue", size=3) #+ ylim(0, 20000)#+ stat_summary(fun.data = mean_sdl, geom="pointrange", color="red", fun.args=list(mult=1))#+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y=median, geom="point", shape=23, color="red")#+ geom_hline(aes(yintercept=Mode(quiet_routes_leeds$length)), col="red")

# To plot the distance decay curves we need the route length from e.g. fast_routes_leeds
#   but we also need the flow numbers from flow_leeds.
# These two dataframes have the same nrow() and CycleStreets should have processed them 
#   in order, so just do a cbind() to do this join

decaydf = cbind(flow_leeds, fast_routes_leeds)@data
decaydf$pcycle = decaydf$WALK/decaydf$TOTAL

ggplot(decaydf, aes(length, pcycle)) + geom_point(alpha=0.5, size=1, shape=21, na.rm = T) + geom_smooth(na.rm = T) + xlim(0,15000)

# data(flowlines)
# l = flowlines[2:5,]
# fast_routes = line2route(l = l, route_fun = route_cyclestreet, plan = "fastest")
# quiet_routes = line2route(l = l, route_fun = route_cyclestreet, plan = "quietest")
# plot(l); plot(fast_routes, add = T); plot(quiet_routes, add = T)

# TODO: fix - giving this error: > flow_cam = flow[las_cam,]
# Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) : 
#   IllegalArgumentException: point array must contain 0 or >1 elements
#cents_lsoa_cam = cents_lsoa[grepl("Cambridge", cents_lsoa$LSOA11NM),]  #This is all of Cambridgeshire
# cents_lsoa_cam = cents_lsoa[grepl("\\<Cambridge\\>", cents_lsoa$LSOA11NM),] # This is exclusively Cambridge
# las_cam = las[cents_lsoa_cam,]
# schools_cam = schools[las_cam,]
# 
# flow_cam = flow[las_cam,]
# plot(las_cam); lines(flow_cam, pch='.', col="blue", cex=5); points(schools_cam, pch='.', col="red", cex=5)
# 
# 
# sld11[grepl("Cambridge",sld11$`LA (name)`), c("SchoolName","Phase", "PhaseOfEducation (name)")]
# sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$Phase=="Secondary", "SchoolName"]
# sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$`PhaseOfEducation (name)`=="Secondary", "SchoolName"]


#starttime <- proc.time()
#flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools)
#proc.time() - starttime

#plot(las); lines(flow, col = 'blue')  

#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)
