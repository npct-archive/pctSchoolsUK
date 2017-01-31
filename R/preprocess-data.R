# Aim: analyse and process the school-level data
# source("setup.R")
# if(!exists("sld11")){
#   source("R/load-all-data.R")
# }

# Aim: load all schools data
#private_dir = "private_data"
#old = setwd(private_dir) 
#list.files() # what's there
# Load the data on the schools, each identified by a "URN" code
# Verify that each school has only one entry:
# length(unique(sld11$LEA11_URN)) == nrow(sld11)

# WE DON'T USE THE 2015 OR 2010 DATA
# 2015 DOESN'T CONTAIN FLOWS, 2010 IS OLDER THAN 2011
#sld15 = readr::read_tsv(file = "SLD_CENSUS_2015.txt")
#names(sld15)
#head(sld15) # look at the data
#s10 = readr::read_tsv(file = "Spring_Census_2010.txt")
#names(s10)
#head(s10)




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





if(!file.exists("private_data/sld_england.Rds")){
  
  sld11 = readr::read_tsv(file = "private_data/SLD_CENSUS_2011.txt")
  names(sld11)
  head(sld11)
  
  
  #setwd(old) # go back to working directory
  
  # The "s" flows df and the "sld" schools data frames can be joined by each school's URN
  #library(dplyr)
  ## Schools in flow for which we have no data in sld are of no use => inner instead of left join
  #joined = s11 %>% inner_join(sld11, by = c("URN_SPR11" = "LEA11_URN"))
  #nrow(joined)
  #head(joined)
  # We then have the LSOA of origin and the flow number from s,
  #  together with the school's coordinates from sld. That's all we need.
  
  # Note that there are 3 more schools listed in sld than there are included in the flow data
  # sum(unique(s11$URN_SPR11) %in% unique(sld11$LEA11_URN)) == nrow(sld11)
  
  # Here we analyse the 2011 data.
  
  # sld11[, names(sld11)[!grepl(pattern = "LEA11_", names(sld11))]] # Non "LEA11_" columns
  # sld11[, names(sld11)[grepl(pattern = "Desc", names(sld11))]] # Columns providing description of other non-Desc column
  # column LEA11_Gender_Desc describes gender of students (mixed, girls, boys)
  
  # Subset the dataset
  #countvars_ft = grep(pattern = "FT", names(sld11))
  # Compute number full-time students
  #sld11$ft = rowSums(sld11[,countvars_ft])
  names(sld11) = gsub("^LEA11_","", names(sld11))
  sld11$Northing = as.integer(sld11$Northing)
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
  
  
  # vname_secondary_f = paste0("FT_Girls_", 12:18)
  # vname_secondary_m = paste0("FT_Boys_", 12:18)
  # vname_secondary = c(vname_secondary_f, vname_secondary_m)
  # sld11$Headcount_Secondary = rowSums(sld11[,vname_secondary])
  
  # Load Phase of Education data so we can select Secondary schools only
  #install.packages("gdata", dependencies = TRUE)
  # gdata::installXLSXsupport()
  # https://www.whatdotheyknow.com/request/list_of_all_schools_in_england_w
  #phase_edu = gdata::read.xls("phase_of_education.xls")
  phase_edu = readxl::read_excel("private_data/phase_of_education.xls")
  #names(phase_edu)
  #names(sld11)
  
  nrow(sld11)
  sld11 = dplyr::inner_join(sld11, phase_edu, by = c("URN" = "URN"))
  nrow(sld11)
  
  sldpri = sld11[(sld11$Phase == "Primary") | (sld11$Phase == "Middle Deemed Primary"), ]
  nrow(sldpri)
  
  sldsec = sld11[(sld11$Phase == "Secondary") | (sld11$Phase == "Middle Deemed Secondary"), ]
  nrow(sldsec)
  
  # unique(sld11$Phase)
  # nrow(sld11)
  # sld = sld11[(sld11$Phase == "Secondary") | (sld11$Phase == "Middle Deemed Secondary") | (sld11$Headcount_Secondary > 0), ]
  # nrow(sld)
  
  #school_levels = unique(sld11[, c("Form7_School_Type","Form7_School_Type_Desc")])
  #school_levels
  ##write.csv(school_levels, "school_levels.csv", row.names=FALSE)
  
  #sld = sld11[sld11$Form7_School_Type %in% c(7, 8, 21, 39, 41, 46),]
  #nrow(sld11)
  #nrow(sld)
  # Filter out schools with less than 100 Secondary-level students
  #nrow(sld)
  
  # CUT ON NUMBER OF STUDENTS?
  ##sld = sld[sld$Headcount_FT_Pupils >= 100,]
  #sldsec = sldsec[sldsec$Headcount_Pupils >= 100,]
  #nrow(sldsec)
  
  # Get rid of schools for which we have no coordinates
  sldsec = sldsec[!is.na(sldsec$Northing) & !is.na(sldsec$Easting),]
  nrow(sldsec) # 5 schools removed
  # locations = ggmap::geocode(sld$LEA11_SchoolName[1:2000])
  # good chance to test the validity of ggmap geocode results
  sum(sldsec$Headcount_FT_Pupils)
  
  # Get top 100 schools with most Secondary-level students
  #sld11_100 = top_n(x = sld11, n = 100, wt = sld11$Headcount_FT_Pupils)  %>%
  #  arrange(desc(Headcount_FT_Pupils))
  #sld11_100
  # saveRDS(sld11_100, "private_data/sld11_100.Rds")
  
  # make the data spatial
  coords = cbind(as.numeric(sldsec$Easting), as.numeric(sldsec$Northing))
  #summary(coords)
  #sld_sp = SpatialPointsDataFrame(coords = coords, data = sld, proj4string = CRS("+init=epsg:32610 +datum=WGS84"))
  #sld_sp = SpatialPointsDataFrame(coords = coords, data = sld, proj4string = CRS("+init=epsg:27700"))
  sld_sp = SpatialPointsDataFrame(coords = coords, data = sldsec)
  proj4string(sld_sp) = CRS("+init=epsg:27700")
  #plot(sld_sp) # all the schools in England
  # saveRDS(s11, "private_data/s11.Rds") # run once so commented
  # saveRDS(sld11, "private_data/sld11.Rds") # run once so commented
  
  # Need the full spatial dataframe for later when trying to plot schools and flows for Leeds/a sub-area
  #   sld_sp has already dropped all Primary schools, so when doing spatial subsetting most Leeds LSOAs do not appear
  # sldfull_sp = sld11[!is.na(sld11$Northing) & !is.na(sld11$Easting),]
  # coords_full = cbind(as.numeric(sldfull_sp$Easting), as.numeric(sldfull_sp$Northing))
  # sldfull_sp = SpatialPointsDataFrame(coords = coords_full, data = sldfull_sp)
  # proj4string(sldfull_sp) = CRS("+init=epsg:27700")
  
  schools = spTransform(sld_sp, CRSobj = proj4string(las))
  schools = schools[las,]
  nrow(schools[las,])
  
  saveRDS(schools, "private_data/sld_england.Rds")
} else{
  schools = readRDS("private_data/sld_england.Rds")
}


# Get the centroids for UK LSOAs
#cents_lsoa = readRDS("../pct-bigdata/cents_lsoa.Rds")
if(!file.exists("private_data/cents_lsoa_2011.Rds")){
  cents_lsoa = shapefile("private_data/lower_layer_super_output_areas_(e+w)_2011_population_weighted_centroids_v2/LSOA_2011_EW_PWC.shp")
  #cents_lsoa = spTransform(cents_lsoa, CRS("+init=epsg:4326"))
  cents_lsoa = spTransform(cents_lsoa, CRSobj = proj4string(las))
  #proj4string(cents_lsoa) = proj4string(las)
  
  # Some LSOA centroids are wrong and are in the ocean. Remove the ones not within LSOAs
  cents_lsoa = cents_lsoa[las,]
  nrow(cents_lsoa)
  
  names(cents_lsoa)[names(cents_lsoa)=="LSOA11CD"] <- "LSOA"
  
  saveRDS(cents_lsoa, "private_data/cents_lsoa_2011.Rds")
} else{
  cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")
}




###############################################################################



if(!file.exists("private_data/england_flows.Rds")){
  
  # Load the flows data, identifying LSOA of orgin and URN of school (destination)
  s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
  #names(s11)
  #head(s11)
  
  
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
  # names(cents_lsoa)
  # names(s)
  # names(schools)
  
  names(s)[names(s)=="LLSOA_SPR11"] <- "LSOA"
  names(s)[names(s)=="URN_SPR11"] <- "URN"
  
  summary(s$LSOA %in% cents_lsoa$LSOA)
  summary(s$URN %in% schools$URN)
  
  s = s[s$LSOA %in% cents_lsoa$LSOA,]
  s = s[s$URN %in% schools$URN,]
  
  # MINIMUM CUT ON FLOW NUMBER?
  summary(s$TOTAL)
  #nrow(s)
  #s_cut = s[s$TOTAL >= 10,]
  #nrow(s_cut)
  s_cut = s
  
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
  
  
  starttime <- proc.time()
  flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools,
                 zone_code = "LSOA", origin_code = "LSOA", dest_code = "URN",
                 zone_code_d = "URN")
  print(proc.time() - starttime)
  saveRDS(flow, "private_data/england_flows.Rds")
} else{
  flow = readRDS("private_data/england_flows.Rds")
}

