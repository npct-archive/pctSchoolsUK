# Aim: load packages to analyse travel to school data
# source("https://gist.githubusercontent.com/jbryer/9112634/raw/29c0a82659ed303c9fa03f1ec07d86161294974d/package.R")
pkgs = c("devtools", "dplyr", "readr", "tmap", "sp", "rgeos")
# package(pkgs)
lapply(X = pkgs, FUN = library, character.only = T)
