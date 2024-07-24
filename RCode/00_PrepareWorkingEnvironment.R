#===============================================================================
# Description: Prepare working environment
# author: Loic Henry and Manon Eluard
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, readr, sp, raster, sf, here, tmap, rvest, ggplot2, 
               archive, snow, foreach, doParallel, furrr, purrr, tictoc, RCurl,
               rnaturalearth, rnaturalearthdata)

# Then: names of other folders in the R project folder
dir <- list()
dir$root <- getwd()
dir$raw_data <- paste0(dir$root, "/Raw_Data") 
dir$prep_data <- paste0(dir$root, "/Prepared_Data")
dir$rcode <- paste0(dir$root, "/RCode")
dir$figures <- paste0(dir$root, "/Figures")

# Create folders in working directory, only if not existing
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F)) 
