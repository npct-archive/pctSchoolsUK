# Aim: convert 2011 LSOA codes to 2001 codes for flows data
library(dplyr)

# Load the flows data, identifying LSOA of orgin and URN of school (destination)
s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
# Keep only rows with valid LSOA names
s11 = s11 %>%
  filter(!is.na(LSOA01CD)) %>% 
  filter(LSOA01CD %in% lookup01to11$LSOA01CD)
# Despite the '11' in the column name the school flows have 2001 LSOA codes. Relabel the column appropriately
s11$LSOA01CD = s11$LLSOA_SPR11
lookup01to11 = readr::read_csv("private_data/lower_layer_super_output_areas_(2001)_to_lower_layer_super_output_areas_(2011)_to_local_authority_districts_(2011)_e+w_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")
lookup_unique = distinct(lookup01to11, LSOA01CD, .keep_all = T)
# lookup01to11[!duplicated(lookup01to11$LSOA01CD),] # the base R way

nrow(s11)
s11new = dplyr::inner_join(s11, select(lookup_unique, LSOA01CD, LSOA11CD, CHGIND))
nrow(s11new) # same number of rows
sum(s11new$TOTAL) / sum(s11$TOTAL) # same number of flows now NAs removed
numeric_vars = names(s11)[sapply(s11, is.numeric)]
numeric_vars = numeric_vars[!grepl(pattern = "11", numeric_vars)]

# U = Unchanged LSOA from 2001 --> 2011
# S = LSOA split into two or more LSOAs 2001 --> 2011
# M = LSOAs merged
# X = Complex change of LSOAs
# THE FLOWS IN COMPLEX LSOA CHANGES IS SO LOW WE CAN DROP THEM
s11nu = s11new[s11new$CHGIND=="U", ] #unchanged 2001 --> 2011 LSOAs
s11ns = s11new[s11new$CHGIND=="S", ] #split
s11nm = s11new[s11new$CHGIND=="M", ] #merged
s11nx = s11new[s11new$CHGIND=="X", ] #complex change
sum(s11nu$TOTAL)/sum(s11new$TOTAL)
sum(s11ns$TOTAL)/sum(s11new$TOTAL)
sum(s11nm$TOTAL)/sum(s11new$TOTAL)
sum(s11nx$TOTAL)/sum(s11new$TOTAL)

# # # # #
# split #
# # # # #

s11ns_dups = inner_join(s11ns, select(lookup01to11, LSOA01CD))
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
s11new_m = group_by(s11nm, LAESTAB_SPR11, LSOA11CD) %>% 
  summarise_each(funs(if(is.numeric(.)) sum(.) else first(.)))
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

