#===============================================================================
# Description: This RCode prepares Corpus data to add countries' location from each affiliations
# author: loic.henry@dauphine.psl.eu
#===============================================================================


##### Prepare working environment
# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, lubridate, ggmap, geosphere, stringr, 
               gender, genderdata, stringi, dplyr, tidyr, genderizeR, kableExtra, ggplot2,
               plm, fixest, stargazer, lmtest, modelsummary, tictoc, countries, stringr)

# List directories 
dir <- list()
dir$root <- here()
dir$figures <- here(dir$root, "Figures")
dir$tables <- here(dir$root, "Tables")
dir$raw.data <- here(dir$root, "Raw_data")
dir$prep.data <- here(dir$root, "Prepared_data")
# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

##### Load data
# First load data: Corpus_Short (adapt path to file to open them)
load(here(dir$prep.data, "GenderedAuthors_Data.Rdata"))

# Retain useful variables
Corpus.Countries <- Corpus.CleanedNames.2 %>% 
  select(TI, AU, AB, SO, PY, DE, ID, AF, TopFive, Top25, TopField, Top10, Top30, TrunkJournals, C1,
         RP, TC, Z9, U1, U2, J9, JI, PG, CCPub_strict, CCPub_large, CCPub_largebis, CCPub_narrow, CC,
         nb_authors, nb_authors_gendered, ratio_identified_gender, sum_gender_male,
         sum_gender_female, proportion_gender_male_all, proportion_gender_male_id,
         proportion_gender_female_all, proportion_gender_female_id, AtLeastOneMale,
         AtLeastOneFemale, category_gender) %>% 
  filter(!is.na(C1))

rm(authors_df, Corpus.CleanedNames.2)
gc()

##### Create list of countries object:
temp <- countries::country_reference_list %>% # Use data frame from country package
  filter(UN_en!="") %>%  # Retain countries reckognized by the UN
  as.data.frame() # Extract all country names 

# Select all columns which contains the different names/spellings
name_columns <- c(grep("^name", names(temp), value = TRUE), "Name0", "Name1", "Name2", "WTO_en", "simple") 

# Put all spellings in a single element
concatenate_non_blank <- function(row) {# Function to concatenate all orthographs in a single element, and discarding NA and blank space
  # Filter out blank spaces
  non_blank_values <- row[(row != "") & !is.na(row)]
  # Concatenate the non-blank values with "|"
  paste(non_blank_values, collapse = "|")
} 
concatenated_values <- apply(temp[,name_columns], 1, concatenate_non_blank) # Apply the function to each row of the UN countries
concatenated_values <- str_to_title(concatenated_values)
country_named_vector <- setNames(concatenated_values, temp$Name0) # Get a named vector, where each element is a country with all its orthographs separated by "|".
country_named_vector["United Kingdom"] <- paste(country_named_vector["United Kingdom"], "England|Scotland|Wales|Northern Ireland", sep = "|")

##### Test a function to identify countries in each affiliations
# Create subsample to test the function
sample_corpus <- Corpus.Countries %>% 
  filter(!is.na(C1)) %>% 
  slice_sample(n = 100)
C1_sample <- sample_corpus$C1

# Test function str_detect and str_extract
str_detect(string = sample_corpus$C1, pattern = country_named_vector[c("United States of America")]) # Returns T/F if country name appears
str_extract(string = sample_corpus$C1, pattern = country_named_vector[c("United States of America")]) # Return the exact string that match (the country name)

# Apply str_detect to the subsample of C1, and to all country names
tic()
Country_df <- as.list(country_named_vector) %>% # Take the vector of country names/spellings
  map_dfc(\(x) str_detect(string = str_to_title(sample_corpus$C1), pattern = x)) %>% # Apply the function str_detect to C1 on each element of the country named vector, and put the output in a dataframe with variable name being the name of the vector
  mutate(C1 = sample_corpus$C1) %>%  # Add to the data frame the affiliation to check
  select(C1, names(country_named_vector))
toc()

# Final object after function would be:
sample_corpus_fin <- sample_corpus %>% 
  bind_cols(dplyr::select(Country_df, -C1))
head(sample_corpus_fin)
# Ok: function ready to be applied to all articles

##### Apply function to all articles and all countries
# Apply function to all countries
# TAKES ABOUT 30 minutes
tic()
Country_df2 <- as.list(country_named_vector) %>% # Take the vector of country names/spellings
  map_dfc(\(x) str_detect(string = str_to_title(Corpus.Countries$C1), pattern = x)) # Apply the function str_detect to C1 on each element of the country named vector, and put the output in a dataframe with variable name being the name of the vector
toc()

# Add all other variables to data and only retain articles with at least one country of affiliation
Corpus_Country_wide <- Corpus.Countries %>% 
  bind_cols(Country_df2) %>% 
  filter(rowSums(across(all_of(names(country_named_vector)))) > 0)
# Note that we're losing 35 000 articles, where we cannot identify country (this is mostly because they do not contain any  country names, but only city, or US region/state)

# Cehck outcome of the function:
check <- Corpus_Country_wide %>% 
  slice_sample(n = 100) %>% 
  select(C1, names(country_named_vector))

##### Build panel df of article by countries and year
# First: construct stat at the world level for each year
World_df <- Corpus_Country_wide %>% 
  filter(!is.na(PY), PY > 1969) %>% 
  group_by(PY) %>% 
  summarize(country = "World",
            N_econ = n(),
            N_CC = length(TI[CC==1]),
            N_TopFive = length(TI[TopFive == 1]),
            N_TopTen = length(TI[Top10 == 1]),
            N_Top30 = length(TI[Top30 == 1]),
            N_TrunkJournals = length(TI[TrunkJournals == 1]),
            N_Top30_CC = length(TI[Top30 == 1 & CC == 1]),
            N_TrunkJournals_CC = length(TI[TrunkJournals == 1 & CC == 1]),
            mean_TC = mean(as.numeric(TC), na.rm = T),
            mean_nb_authors = mean(nb_authors, na.rm = T),
            mean_nb_authors_gendered = mean(nb_authors_gendered, na.rm = T),
            mean_share_male_authors = mean(proportion_gender_male_id, na.rm = T),
            mean_share_female_authors = mean(proportion_gender_female_id, na.rm = T),
            N_F = length(TI[category_gender == "F"]),
            N_M = length(TI[category_gender == "M"]),
            N_FF = length(TI[category_gender == "FF"]),
            N_MM = length(TI[category_gender == "MM"]),
            N_FM = length(TI[category_gender == "MF"])
            )

# Create a vector of real contries names
real_country_names <- countries::country_reference_list %>%
  filter(WTO_en != "") %>% 
  select(Name0) %>% 
  pull()

# Second, pivot data: have a country variable that states from which country comes the authors  
Corpus_Country_long <- Corpus_Country_wide %>% 
  filter(!is.na(PY), PY > 1969) %>% 
  pivot_longer(cols = names(country_named_vector),
               names_to = "country",
               values_to = "is_affiliated_in_country") %>% 
  filter(is_affiliated_in_country == TRUE)

# Third, only retain real country names
Corpus_Country_long2 <- Corpus_Country_long %>% 
  filter(country %in% real_country_names)

# Compute descriptive statistics by country and by year
Country_PY_df <- Corpus_Country_long %>% 
  filter(PY < 2023) %>% 
  group_by(country, PY) %>% 
  summarize(
    N_econ = n(), # Nb d'articles d'économie publiés dans l'année dont un (au moins) des auteurs sont affiliés dans le pays
    N_CC = length(TI[CC==1]), # Nb d'articles d'économie sur le CC publiés dans l'année dont un (au moins) auteurs sont affiliés dans le pays
    N_TopFive = length(TI[TopFive == 1]), # Nb d'articles publiés dans des journaux du top 5 dont un (au moins) des auteurs est affilié dans le pays
    N_TopTen = length(TI[Top10 == 1]), # même chose avec journaux du top ten"
    N_Top30 = length(TI[Top30 == 1]), # Idem, top 30
    N_TrunkJournals = length(TI[TrunkJournals == 1]), # Idem, Trunk journals
    N_Top30_CC = length(TI[Top30 == 1 & CC == 1]), # Idem, pour les articles dans un journal du top 30 sur le CC
    N_TrunkJournals_CC = length(TI[TrunkJournals == 1 & CC == 1]), # Idem pour les articles dans un trunk journals
    mean_TC = mean(as.numeric(TC), na.rm = T), # Moyenne des citations reçus sur les articles avec au moins auteur affilié dans le pays publié cette année
    mean_nb_authors = mean(nb_authors, na.rm = T), # Moyenne du nb d'auteurs sur ces articles
    mean_nb_authors_gendered = mean(nb_authors_gendered, na.rm = T), # Moyenne du nb d'auteurs dont le genre est assigné
    mean_share_male_authors = mean(proportion_gender_male_id, na.rm = T), # Moyenne de la part des auteurs qui sont des hommes pour tous les articles d'économie publiés cette année avec au moins un auteur affilié dans le pays
    mean_share_female_authors = mean(proportion_gender_female_id, na.rm = T), # Moyenne de la part des auteurs qui sont des femmes pour tous les articles d'économie publiés cette année avec au moins un auteur affilié dans le pays
    N_F = length(TI[category_gender == "F"]), # Nombre d'article avec une auteure (femme seule)
    N_M = length(TI[category_gender == "M"]), # Nombre d'article avec un auteur (homme seul)
    N_FF = length(TI[category_gender == "FF"]), # Nombre d'articles écrits par plusieurs femmes
    N_MM = length(TI[category_gender == "MM"]), # Nombre d'articles écrits pas plusieurs hommes
    N_FM = length(TI[category_gender == "MF"]) # Nombre d'articles écrits pas une équipe mixte
  ) %>% 
  bind_rows(World_df) %>% 
  group_by(PY) %>% 
  mutate(# Mêmes variables mais en pourcentage dans le total mondial de cette année
    Share_econ = N_econ / N_econ[country == "World"] * 100, 
    Share_CC = ifelse(test= N_CC[country == "World"] == 0, 
                      yes = NA, 
                      no = N_CC / N_CC[country == "World"] * 100),
    Share_TopFive = N_TopFive / N_TopFive[country == "World"] * 100,
    Share_TopTen = N_TopTen / N_TopTen[country == "World"] * 100, 
    Share_Top30 = N_Top30 / N_Top30[country == "World"] * 100,
    Share_TrunkJournals = N_TrunkJournals / N_TrunkJournals[country == "World"] * 100,
    Share_Top30_CC = ifelse(test= N_Top30_CC[country == "World"] == 0, 
                            yes = NA, 
                            no = N_Top30_CC / N_Top30_CC[country == "World"] * 100),
    Share_TrunkJournals_CC = ifelse(test= N_TrunkJournals_CC[country == "World"] == 0, 
                                    yes = NA, 
                                    no = N_TrunkJournals_CC / N_TrunkJournals_CC[country == "World"] * 100)
    ) %>% 
  ungroup() %>% 
  arrange(PY, country)

##### Save data
save(Country_PY_df, file = here(dir$prep.data, "Corpus_Country_PY.Rdata"))



