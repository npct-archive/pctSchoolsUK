# Aim: load all schools data
private_dir = "private_data/"
old = setwd(private_dir) 
list.files() # what's there
sld15 = readr::read_tsv(file = "SLD_CENSUS_2015.txt")
sld15 # look at the data
sld11 = readr::read_tsv(file = "SLD_CENSUS_2011.txt")
s11 = readr::read_tsv(file = "Spring_Census_2011.txt")
s10 = readr::read_tsv(file = "Spring_Census_2010.txt")
setwd(old) # go back to working directory
