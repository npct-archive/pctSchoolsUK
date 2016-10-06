# Aim: analyse and process the school-level data
source("setup.R")
if(!exists("sld11")){
  source("R/load-all-data.R")
}

# Here we analyse the 2011 data. 2015 can be done exactly analogously.

# Subset the dataset
countvars_ft = grep(pattern = "FT", names(sld11))
# Compute number full-time students
sld11$ft = rowSums(sld11[,countvars_ft])
# hist(sld11$ft)
# summary(sld11$ft)
# Get total number of Secondary students (male and female)
vname_secondary_f = paste0("LEA11_FT_Girls_", 11:18)
vname_secondary_m = paste0("LEA11_FT_Boys_", 11:18)
vname_secondary = c(vname_secondary_f, vname_secondary_m)
sld11$Secondary = rowSums(sld11[,vname_secondary])
# Filter out schools with less than 100 Secondary-level students
nrow(sld11)
sld = sld11[sld11$Secondary >= 100,]
nrow(sld)
# Get rid of schools for which we have no coordinates
sld = sld[!is.na(sld$Northing) & !is.na(sld$Easting),]
nrow(sld) # 5 schools removed
# locations = ggmap::geocode(sld$LEA11_SchoolName[1:2000])
# good chance to test the validity of ggmap geocode results
sum(sld$Secondary)
# Get top 100 schools with most Secondary-level students
sld11_100 = top_n(x = sld11, n = 100, wt = sld11$Secondary)  %>%
  arrange(desc(Secondary))
# saveRDS(sld11_100, "private_data/sld11_100.Rds")

# make the data spatial
coords = cbind(as.numeric(sld$Easting), as.numeric(sld$Northing))
summary(coords)
sld_sp = SpatialPointsDataFrame(coords = coords, data = sld)
plot(sld_sp) # all the schools in England


