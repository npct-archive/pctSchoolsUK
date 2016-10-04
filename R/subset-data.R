# Aim: subset the data to a region

source("R/analysis-sld.R")
las = readRDS("../pct-bigdata/las.Rds")
leeds_la = las[las$NAME == "Leeds",]
plot(leeds_la)
proj4string(sld_sp) = proj4string(las)
schools = sld_sp[leeds_la,]
points(schools)
saveRDS(schools, "private_data/sld_leeds.Rds")

# subset all ods that go to these schools

# lsoa
cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
schools = readRDS("private_data/sld_leeds.Rds")
plot(cents_lsoa)
bbox(cents_lsoa)
cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))
cents_lsoa = cents_lsoa[leeds_la,]

# subset the od data
sel_od = s11$LLSOA_SPR11 %in% cents_lsoa$LSOA11CD
s = s11[sel_od,]
s = s[!is.na(s$LLSOA_SPR11),]
s = s[s$URN_SPR11 %in% schools$LEA11_URN,]
names(s)
names(cents_lsoa)[1] = names(s)[1]
names(schools)[2] = names(s)[3]
summary(s$LLSOA_SPR11 %in% cents_lsoa$LLSOA_SPR11)
summary(s$URN_SPR11 %in% schools$URN_SPR11)

plot(leeds_la)
points(cents_lsoa)
flow = od2line(flow = s, zones = cents_lsoa, destinations = schools)
flow = flow[flow$TOTAL > 10,]
plot(flow)
