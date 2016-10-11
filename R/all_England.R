# Aim: subset the data to a region

# Read in and pre-process the data
source("R/analysis-sld.R")

# Read in UK Local Authorities, subset Leeds only
las = readRDS("../pct-bigdata/las.Rds")
#leeds_la = las[las$NAME == "Leeds",]
#plot(leeds_la)
plot(las)
# Match CRS of spatial data to that of Local Authorities data
proj4string(sld_sp) = proj4string(las)
# Subset schools data to Leeds
#schools = sld_sp[leeds_la,]
schools = sld_sp
#points(schools)

#schools = spTransform(schools, CRSobj = proj4string(las))

saveRDS(schools, "private_data/sld_england.Rds")

# subset all ods that go to these schools

# Get the centroids for UK LSOAs
cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
if(!exists("schools"))
  schools = readRDS("private_data/sld_england.Rds")

# Match CRS of LSOAs to be consistent with las and schools
cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))
plot(cents_lsoa)
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
names(schools)[names(schools)=="LEA11_URN"] <- "URN"
#names(cents_lsoa)[1] = names(s)[1]
#names(schools)[2] = names(s)[3]
summary(s$LSOA %in% cents_lsoa$LSOA)
summary(s$URN %in% schools$URN)

s = s[s$LSOA %in% cents_lsoa$LSOA,]
s = s[s$URN %in% schools$URN,]

nrow(cents_lsoa)
nrow(schools)
cents_lsoa = cents_lsoa[cents_lsoa$LSOA %in% s$LSOA,]
schools = schools[schools$URN %in% s$URN,]
nrow(cents_lsoa)
nrow(schools)

#plot(las)
#points(cents_lsoa)
#points(schools, col = 'red', pch = 4)

summary(s$TOTAL)
nrow(s)
s_cut = s[s$TOTAL >= 10,]
nrow(s_cut)

starttime <- proc.time()
flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools)
#flow = flow[flow$TOTAL > 10,]
proc.time() - starttime

plot(las)
points(cents_lsoa)
points(schools, col = 'red', pch = 4)
lines(flow, col = 'blue')
