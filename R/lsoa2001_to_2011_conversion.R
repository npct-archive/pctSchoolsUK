# Aim: convert 2011 LSOA codes to 2001 codes for flows data
library(dplyr)

# Load the flows data, identifying LSOA of orgin and URN of school (destination)
s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
# Despite the '11' in the column name the school flows have 2001 LSOA codes. Relabel the column appropriately
#s11$LSOA01CD = s11$LLSOA_SPR11
names(s11)[names(s11)=="LLSOA_SPR11"] = "LSOA01CD"
lookup01to11 = readr::read_csv("private_data/lower_layer_super_output_areas_(2001)_to_lower_layer_super_output_areas_(2011)_to_local_authority_districts_(2011)_e+w_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")
lookup_unique = distinct(lookup01to11, LSOA01CD, .keep_all = T)
# lookup01to11[!duplicated(lookup01to11$LSOA01CD),] # the base R way

#isTRUE(nrow(lookup_unique) == nrow(lookup01to11))
#anti_join(lookup01to11,lookup_unique, by="LSOA01CD")

# Keep only rows with valid LSOA codes, and which are in the 2001->2011 lookup table (8875 rows dropped)
s11 = s11 %>%
  filter(!is.na(LSOA01CD)) %>% 
  filter(LSOA01CD %in% lookup01to11$LSOA01CD)

nrow(s11)
s11new = dplyr::inner_join(s11, select(lookup_unique, LSOA01CD, LSOA11CD, CHGIND), by="LSOA01CD")
nrow(s11new) # same number of rows
sum(s11new$TOTAL) / sum(s11$TOTAL) # same number of flows now NAs removed
numeric_vars = names(s11)[sapply(s11, is.numeric)]
numeric_vars = numeric_vars[!grepl(pattern = "11", numeric_vars)]

# CHGIND labels:
# U = Unchanged LSOA from 2001 --> 2011
# S = LSOA split into two or more LSOAs 2001 --> 2011
# M = LSOAs merged
# X = Complex change of LSOAs

# THE FLOWS IN COMPLEX LSOA CHANGES IS SO LOW WE CAN IGNORE THEM
s11nu = s11new %>% filter(CHGIND=="U")
s11ns = s11new %>% filter(CHGIND=="S")
s11nm = s11new %>% filter(CHGIND=="M")
s11nx = s11new %>% filter(CHGIND=="X")

sum(s11nu$TOTAL)/sum(s11new$TOTAL)
sum(s11ns$TOTAL)/sum(s11new$TOTAL)
sum(s11nm$TOTAL)/sum(s11new$TOTAL)
sum(s11nx$TOTAL)/sum(s11new$TOTAL)

# # # # #
# split #
# # # # #

s11ns_dups = inner_join(s11ns, select(lookup01to11, LSOA01CD), by="LSOA01CD")
# Rows of s11 which have a single 2001 LSOA code match to two different 2011 LSOA codes in the lookup,
#   so inner_join duplicates, with one entry for each 2011 LSOA code
nrow(s11ns_dups) / nrow(s11ns) # rows have correctly increased
sum(s11ns_dups$TOTAL) / sum(s11ns$TOTAL) # totals have incorrectly changed...

s11new_s = group_by(s11ns_dups, URN_SPR11, LSOA01CD) %>%
  mutate_if(is.numeric, funs(. / n())) # that's how you get n rows per group

sum(s11new_s$TOTAL) / sum(s11ns$TOTAL) # same sums
nrow(s11new_s) / nrow(s11ns) # different rownums

s11new_s = select(s11new_s, match(names(s11ns), names(s11new_s)))
nrow(s11ns) / nrow(s11new_s) # many lsoas added
sum(s11new_s$TOTAL) / sum(s11ns$TOTAL) # same total!

# # # # #
# merge #
# # # # #

nrow(distinct(s11nm, LSOA01CD)) / nrow(distinct(s11nm, LSOA11CD)) # half the number in the merged ones
# See http://stackoverflow.com/questions/34857099
s11new_m = group_by(s11nm, URN_SPR11, LSOA11CD) %>% 
  summarise_each(funs(if(is.numeric(.)) sum(.) else first(.)))
# s11new_m = group_by(s11nm, LAESTAB_SPR11, LSOA11CD) %>% 
#   summarise_each(funs(if(is.numeric(.)) sum(.) else first(.)))
nrow(s11nm) / nrow(s11new_m) # most duplicated, not all
sum(s11new_m$TOTAL) / sum(s11nm$TOTAL) # same total!
names(s11new_m) == names(s11nm) # incorrect col order
s11new_m = select(s11new_m, match(names(s11nm), names(s11new_m)))
colMeans(s11new_m[5:10]) / colMeans(s11nm[5:10])
colSums(s11new_m[5:10]) / colSums(s11nm[5:10])

# # # # # # # #
# save output #
# # # # # # # #

# Combine merged and split data
s11n = bind_rows(s11nu, s11new_s, s11new_m)
saveRDS(s11n, "private_data/s11n.Rds")

# check results
s11new_nc = bind_rows(s11nu, s11ns, s11nm)
colSums(s11new_nc[numeric_vars]) == colSums(s11n[numeric_vars])



# # # # # # # # # # #
# get desire lines  #
# # # # # # # # # # # 

# if you don't want to rerun code above and already have the Rds:
s11n = readRDS("private_data/s11n.Rds")

# Load the flows data, identifying LSOA of orgin and URN of school (destination)
#s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
#names(s11)
#head(s11)


nrow(s11n)
s = s11n[!is.na(s11n$LSOA11CD),] # drop flows with no LSOA data
nrow(s)

schools = readRDS("private_data/sld_england.Rds")

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

names(s)[names(s)=="LSOA11CD"] <- "LSOA"
names(s)[names(s)=="URN_SPR11"] <- "URN"

cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")

summary(s$LSOA %in% cents_lsoa$LSOA)
summary(s$URN %in% schools$URN)

s = s[s$LSOA %in% cents_lsoa$LSOA,]
s = s[s$URN %in% schools$URN,]

# MINIMUM CUT ON FLOW NUMBER?
summary(s$TOTAL)
sum(s$TOTAL)
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

library(stplanr)

if(!file.exists("private_data/england_flows_lsoa2011.Rds")){
starttime <- proc.time()
flow = od2line(flow = s_cut, zones = cents_lsoa, destinations = schools,
               zone_code = "LSOA", origin_code = "LSOA", dest_code = "URN",
               zone_code_d = "URN")
print(proc.time() - starttime)
saveRDS(flow, "private_data/england_flows_lsoa2011.Rds")
} else{
  flow = readRDS("private_data/england_flows_lsoa2011.Rds")
}


# Check the desire lines by plotting them for Leeds
# library(raster)
# las = shapefile("private_data/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp")
# las = spTransform(las, CRS(proj4string(cents_lsoa)))
# leeds_las = las[grepl("Leeds", las$lsoa11nm),]
# plot(leeds_las)
# plot(flow[leeds_las,], add=T, col="blue")
# plot(las)
# plot(flow[leeds_las,], add=T, col="red")


if(!file.exists("private_data/rf_england_schools_lsoa2011.Rds") | !file.exists("private_data/rq_england_schools_lsoa2011.Rds")){
  # , base_url="http://pct.cyclestreets.net/api/"
  #flow = readRDS("private_data/england_flows_lsoa2011.Rds")
  fast_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "fastest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  saveRDS(fast_routes_england, "private_data/rf_england_schools_lsoa2011.Rds")
  quiet_routes_england = line2route(l = flow, route_fun = route_cyclestreet, plan = "quietest", n_processes = 10, base_url="http://pct.cyclestreets.net/api/")
  saveRDS(quiet_routes_england, "private_data/rq_england_schools_lsoa2011.Rds")
} else{
  fast_routes_england = readRDS("private_data/rf_england_schools_lsoa2011.Rds")
  quiet_routes_england = readRDS("private_data/rq_england_schools_lsoa2011.Rds")
}
