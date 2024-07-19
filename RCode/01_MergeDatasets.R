#===============================================================================
# Description: Prepare and merge all datasets
# author: Loic Henry and Manon Eluard
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

##### Only required package to initiate is pacman
# Install and load packages (other packages are install and loaded from "00_PrepareEnvironment.R")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)


#####  Precised relative access to useful Rcodes
dir <- list() # First create an empty list, that we then fill with subobjects
dir$root <- getwd() # First: the place of the R project
dir$rcode <- paste0(dir$root, "/RCode")

##### Prepare working environment
# Run R scrit "00" which contains all useful package, and creation of all directories
source(here(dir$rcode, "00_PrepareWorkingEnvironment.R"))

##### Load dataset of articles by country
load(file = here(dir$raw_data, "Corpus_Country_PY.Rdata"))
# Prepare dataset by adding observations: all possible combinations of country-years
countries <- unique(Country_PY_df$country)
years <- unique(Country_PY_df$PY)
df_final <- expand.grid(country = countries, PY = years)

# Create table of articles by countries for all years
df_final_join <- df_final %>% 
  left_join(Country_PY_df, by = c("country", "PY")) %>% 
  mutate_all(~ ifelse(is.na(.), 0, .)) # Replace missing values by 0s (no articles those years)