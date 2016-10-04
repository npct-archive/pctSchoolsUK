sld = readRDS("private_data/sld_leeds.Rds")
sld
names(sld)
sel = s11$URN_SPR11 %in% sld$LEA11_URN
s = s11[sel,]

# https://data.gov.uk/dataset/lower_layer_super_output_area_lsoa_boundaries
#  1/1/2011 Lower layer super output areas (England and Wales) 2011 Population Weighted Centroids

lsoa_url = "https://files.datapress.com/london/dataset/lsoa-atlas/lsoa-data.xls"
download.file(lsoa_url, destfile = "lsoa_boundaries.xls")
