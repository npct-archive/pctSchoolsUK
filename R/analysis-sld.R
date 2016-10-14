# Aim: analyse and process the school-level data
source("setup.R")
if(!exists("sld11")){
  source("R/load-all-data.R")
}

# Here we analyse the 2011 data. 2015 can be done exactly analogously.

# sld11[, names(sld11)[!grepl(pattern = "LEA11_", names(sld11))]] # Non "LEA11_" columns
# sld11[, names(sld11)[grepl(pattern = "Desc", names(sld11))]] # Columns providing description of other non-Desc column
# column LEA11_Gender_Desc describes gender of students (mixed, girls, boys)

# Subset the dataset
#countvars_ft = grep(pattern = "FT", names(sld11))
# Compute number full-time students
#sld11$ft = rowSums(sld11[,countvars_ft])
names(sld11) = gsub("^LEA11_","", names(sld11))
#names(sld11)[names(sld11) == "Headcount_FT_Pupils"] = "ft"
# hist(sld11$ft)
# summary(sld11$ft)
# Get total number of Secondary students (male and female)
#vname_secondary_f = paste0("LEA11_FT_Girls_", 11:18)
#vname_secondary_m = paste0("LEA11_FT_Boys_", 11:18)
#vname_secondary = c(vname_secondary_f, vname_secondary_m)
#sld11$Secondary = rowSums(sld11[,vname_secondary])
#######
## sudo apt-get install default-jdk libpcre3-dev bzip2 liblzma-dev libbz2-dev
## sudo R CMD javareconf
## install.packages("XLConnect", dependencies=TRUE)

#install.packages("gdata", dependencies = TRUE)
# gdata::installXLSXsupport()

# https://www.whatdotheyknow.com/request/list_of_all_schools_in_england_w
phase_edu = gdata::read.xls("phase_of_education.xls")
#names(phase_edu)
#names(sld11)

vname_secondary_f = paste0("FT_Girls_", 11:18)
vname_secondary_m = paste0("FT_Boys_", 11:18)
vname_secondary = c(vname_secondary_f, vname_secondary_m)
sld11$Headcount_Secondary = rowSums(sld11[,vname_secondary])

nrow(sld11)
sld11 = dplyr::inner_join(sld11, phase_edu, by = c("URN" = "URN"))
nrow(sld11)

unique(sld11$Phase)
nrow(sld11)
sld = sld11[(sld11$Phase == "Secondary") | (sld11$Headcount_Secondary > 0), ]
nrow(sld)
  
#school_levels = unique(sld11[, c("Form7_School_Type","Form7_School_Type_Desc")])
#school_levels
##write.csv(school_levels, "school_levels.csv", row.names=FALSE)

#sld = sld11[sld11$Form7_School_Type %in% c(7, 8, 21, 39, 41, 46),]
#nrow(sld11)
#nrow(sld)
# Filter out schools with less than 100 Secondary-level students
#nrow(sld)
sld = sld[sld$Headcount_FT_Pupils >= 100,]
nrow(sld)
# Get rid of schools for which we have no coordinates
sld = sld[!is.na(sld$Northing) & !is.na(sld$Easting),]
nrow(sld) # 5 schools removed
# locations = ggmap::geocode(sld$LEA11_SchoolName[1:2000])
# good chance to test the validity of ggmap geocode results
sum(sld$Headcount_FT_Pupils)
# Get top 100 schools with most Secondary-level students
sld11_100 = top_n(x = sld11, n = 100, wt = sld11$Headcount_FT_Pupils)  %>%
  arrange(desc(Headcount_FT_Pupils))
sld11_100
# saveRDS(sld11_100, "private_data/sld11_100.Rds")

# make the data spatial
coords = cbind(as.numeric(sld$Easting), as.numeric(sld$Northing))
#summary(coords)
#sld_sp = SpatialPointsDataFrame(coords = coords, data = sld, proj4string = CRS("+init=epsg:32610 +datum=WGS84"))
#sld_sp = SpatialPointsDataFrame(coords = coords, data = sld, proj4string = CRS("+init=epsg:27700"))
sld_sp = SpatialPointsDataFrame(coords = coords, data = sld)
proj4string(sld_sp) = CRS("+init=epsg:27700")
#plot(sld_sp) # all the schools in England


