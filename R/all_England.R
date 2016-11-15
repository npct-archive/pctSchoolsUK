# Aim: subset the data to a region

# Read in and pre-process the data
setwd("/home/geoif/pct/pctSchoolsUK")
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

#plot(las); plot(schools, col="red", pch='.', cex=2, add=T)

if(!file.exists("private_data/england_flows.Rds")){
  starttime <- proc.time()
  flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools)
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

flow_brad = flow[las_brad,]
plot(las_brad); lines(flow_brad, pch='.', col="blue", cex=5); points(schools_brad, pch='.', col="red", cex=5)


#######################################################################
### JUST CAMBRIDGE

#cents_lsoa_cam = cents_lsoa[grepl("Cambridge", cents_lsoa$LSOA11NM),]  #This is all of Cambridgeshire
cents_lsoa_cam = cents_lsoa[grepl("\\<Cambridge\\>", cents_lsoa$LSOA11NM),] # This is exclusively Cambridge
las_cam = las[cents_lsoa_cam,]
schools_cam = schools[las_cam,]

flow_cam = flow[las_cam,]
plot(las_cam); lines(flow_cam, pch='.', col="blue", cex=5); points(schools_cam, pch='.', col="red", cex=5)


sld11[grepl("Cambridge",sld11$`LA (name)`), c("SchoolName","Phase", "PhaseOfEducation (name)")]
sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$Phase=="Secondary", "SchoolName"]
sld11[grepl("Cambridge",sld11$`LA (name)`) & sld11$`PhaseOfEducation (name)`=="Secondary", "SchoolName"]


#starttime <- proc.time()
#flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools)
#proc.time() - starttime

#plot(las); lines(flow, col = 'blue')  

#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)
