# Aim: load all schools data
private_dir = "private_data"
old = setwd(private_dir) 
list.files() # what's there
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

sld11 = readr::read_tsv(file = "SLD_CENSUS_2011.txt")
names(sld11)
head(sld11)

# Load the flows data, identifying LSOA of orgin and URN of school (destination)
s11 = readr::read_tsv(file = "Spring_Census_2011.txt")
names(s11)
head(s11)

setwd(old) # go back to working directory

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


