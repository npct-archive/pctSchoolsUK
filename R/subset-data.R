# Aim: subset the data to a region

source("R/analysis-sld.R")
las = readRDS("../pct-bigdata/las.Rds")
leeds_la = las[las$NAME == "Leeds",]
plot(leeds_la)
proj4string(sld_sp) = proj4string(las)
sld_leeds = sld_sp[leeds_la,]
points(sld_leeds)
saveRDS(sld_leeds, "private_data/sld_leeds.Rds")

# subset all ods that go to these schools

# lsoa
cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
plot(cents_lsoa)
bbox(cents_lsoa)
cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))

# subset the od data
sel_od = s11$LLSOA_SPR11 %in% cents_lsoa$LSOA11CD 
s = s11[sel_od,]
s = s[!is.na(s$LLSOA_SPR11),]
names(s)

cents_lsoa = cents_lsoa[leeds_la,]
plot(leeds_la)
points(cents_lsoa)
flow = od2line(flow = s, zones = cents_lsoa, destinations = sld_leeds)

numcoords = merge(x = s, y = sld_leeds, by.x = "URN_SPR11", by.y = "LEA11_URN", all.x = TRUE)
numcoords

numcoords1 = merge(x = numcoords, y = cents_lsoa, by.x = "LLSOA_SPR11", by.y = "LSOA11CD", all.x = TRUE)
nrow(numcoords1)

numcoords1 = numcoords1[,c("LLSOA_SPR11","URN_SPR11","SCHOOLNAME_SPR11","Secondary","TOTAL","CAR","CYCLE","WALK","OTHER","UNKNOWN","coords.x1.x","coords.x1.y","coords.x2.x","coords.x2.y")]
