# Aim: load packages to analyse travel to school data
# source("https://gist.githubusercontent.com/jbryer/9112634/raw/29c0a82659ed303c9fa03f1ec07d86161294974d/package.R")
pkgs = c("devtools", "dplyr", "readr", "sp", "rgeos", "tmap", "stplanr","raster","readxl","geojsonio", "glmnet")
to_install = !pkgs %in% installed.packages()
if(sum(to_install) > 0){
  install.packages(pkgs[to_install])
}
lapply(X = pkgs, FUN = library, character.only = T)
# library(pkgs)
