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

### Création base de données

library(dplyr)
library(stringr)

#Données banque mondiale: PIB
df_PIB <- read.csv(file = here(dir$raw_data,"PIB_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_PIB <- paste("X", as.character(c(1960:2023)), sep = "")

df_long_PIB <- df_PIB %>% 
  tidyr::pivot_longer(cols = all_of(year_col_PIB), names_to = "year", values_to = "PIBValues") 

df_long_PIB <- df_long_PIB %>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_PIB, file = here(dir$prep_data,"PIB2.csv"), row.names = FALSE)

#Données banque mondiale: R&D
df_RD <- read.csv(file = here(dir$raw_data,"RD_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_RD <- paste("X", as.character(c(1996:2023)), sep = "")

df_long_RD <- df_RD %>% 
  tidyr::pivot_longer(cols = all_of(year_col_RD), names_to = "year", values_to = "R&DValues") 

df_long_RD <- df_long_RD %>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_RD, file = here(dir$prep_data,"RD2.csv"), row.names = FALSE)

#Données banque mondiale: EducF
df_EducF <- read.csv(file = here(dir$raw_data,"EducF_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_EducF <- paste("X", as.character(c(1990:2023)), sep = "")

df_long_EducF <- df_EducF %>% 
  tidyr::pivot_longer(cols = all_of(year_col_EducF), names_to = "year", values_to = "EducFValues") 

df_long_EducF <- df_long_EducF %>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_EducF, file = here(dir$prep_data,"EducF2.csv"), row.names = FALSE)

#Données banque mondiale: EducH
df_EducH <- read.csv(file = here(dir$raw_data,"EducH_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_EducH <- paste("X", as.character(c(1990:2023)), sep = "")

df_long_EducH <- df_EducH %>% 
  tidyr::pivot_longer(cols = all_of(year_col_EducH), names_to = "year", values_to = "EducHValues") 

df_long_EducH <- df_long_EducH %>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_EducH, file = here(dir$prep_data,"EducH2.csv"), row.names = FALSE)

#Données banque mondiale: CO2_emissions
df_CO2_emissions <- read.csv(file = here(dir$raw_data,"CO2_emissions_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_CO2_emissions <- paste("X", as.character(c(1990:2023)), sep = "")

df_long_CO2_emissions <- df_CO2_emissions %>% 
  tidyr::pivot_longer(cols = all_of(year_col_CO2_emissions), names_to = "year", values_to = "CO2emissionsalues") 

df_long_CO2_emissions <- df_long_CO2_emissions%>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_CO2_emissions, file = here(dir$prep_data,"CO2_emissions2.csv"), row.names = FALSE)

#Données IMF: Expenditures
df_Expenditures <- read.csv(file = here(dir$raw_data,"Expenditures_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_Expenditures <- paste("X", as.character(c(1995:2022)), sep = "")

df_long_Expenditures <- df_Expenditures%>% 
  tidyr::pivot_longer(cols = all_of(year_col_Expenditures), names_to = "year", values_to = "ExpendituresValues") 

df_long_Expenditures <- df_long_Expenditures%>%
  mutate(year = str_replace(year, "^X", ""))

df_long_Expenditures <- df_long_Expenditures%>%
  select(!CTS.Name)

df_long_Expenditures <- df_long_Expenditures%>%
  filter(Indicator %in% "Expenditure on environment protection")

df_long_Expenditures <- df_long_Expenditures%>%
  filter(Unit %in% "Percent of GDP")

write.csv(df_long_Expenditures, file = here(dir$prep_data,"Expenditures2.csv"), row.names = FALSE)

#Données IMF: trade

df_trade <-  read.csv(file = here(dir$raw_data,"trade.csv"), header = T, sep = ";", dec = ",",na.strings = "")

df_trade_filtered <- df_trade %>% 
  select(ObjectId,Country, ISO3, Indicator, Unit, Scale, F1994, F1995, F1996, F1997, F1998, F1999, F2000, F2001, F2002, F2003, F2004, F2005, F2006, F2007, F2008, F2009, F2010, F2011, F2012, F2013, F2014, F2015, F2016, F2017, F2018, F2019, F2020, F2021, F2022) %>% 
  filter(Indicator %in% c("Comparative advantage in low carbon technology products","Total trade in low carbon technology products as percent of GDP","Trade balance in low carbon technology products as percent of GDP"))

year_col_trade <- paste("F", as.character(c(1994:2022)), sep = "")

df_long_trade <- df_trade_filtered %>% 
  tidyr::pivot_longer(cols = all_of(year_col_trade), names_to = "year", values_to = "tradeValues")

df_long_trade <- df_long_trade  %>%
  tidyr::pivot_wider(names_from = Indicator, values_from = "tradeValues",id_cols = c("ISO3","year"))

df_long_trade <- df_long_trade %>%
  mutate(year = str_replace(year, "^F", ""))

names(df_long_trade)

df_long_trade <- df_long_trade %>%
  rename("Comparative_advantage"="Comparative advantage in low carbon technology products")%>%
  rename("Total_trade"="Total trade in low carbon technology products as percent of GDP")%>%
  rename("Trade_balance"="Trade balance in low carbon technology products as percent of GDP")

write.csv(df_long_trade, file = here(dir$prep_data,"trade2.csv"), row.names = FALSE)

#Données IMF: disasters
df_disasters <- read.csv(file = here(dir$raw_data,"disasters_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_disasters <- paste("X", as.character(c(1980:2023)), sep = "")

df_long_disasters <- df_disasters%>% 
  tidyr::pivot_longer(cols = all_of(year_col_disasters), names_to = "year", values_to = "DisastersValues") 

df_long_disasters <- df_long_disasters%>%
  mutate(year = str_replace(year, "^X", ""))

df_long_disasters <- df_long_disasters%>%
  select(!Source)

df_long_disasters <- df_long_disasters%>%
  filter(Indicator %in% "Climate related disasters frequency, Number of Disasters: TOTAL")

write.csv(df_long_disasters, file = here(dir$prep_data,"disasters2.csv"), row.names = FALSE)

#Données IMF: temperature

df_temperature <- read.csv(file = here(dir$raw_data,"temperature_prepared.csv"), header = TRUE, sep = ";", dec = ",")
year_col_temperature <- paste("X", as.character(c(1961:2023)), sep = "")

df_long_temperature <- df_temperature%>% 
  tidyr::pivot_longer(cols = all_of(year_col_temperature), names_to = "year", values_to = "TemperatureValues")

df_long_temperature<- df_long_temperature%>%
  mutate(year = str_replace(year, "^X", ""))

write.csv(df_long_temperature, file = here(dir$prep_data,"temperature2.csv"), row.names = FALSE)

#Données IMF: targets
df_long_targets <- read.csv(file = here(dir$raw_data,"target_prepared.csv"), header = TRUE, sep = ";", dec = ",")
df_long_targets <- df_long_targets %>% rename("2030" = F2030)

df_long_targets <- df_long_targets %>% 
  select(!ObjectId) %>% 
  select(!X) 

df_long_targets <- df_long_targets %>%
  tidyr::pivot_wider(names_from = Indicator, values_from = "2030")

names(df_long_targets)[5:7]<-c("NDC_Implied_average","NDC_Implied_conditional","NDC_Implied_unconditional")

write.csv(df_long_targets, file = here(dir$prep_data,"targets2.csv"), row.names = FALSE)

##Données IMF: carbon_tax

df_carbon_tax <- read.csv(file = here(dir$raw_data,"carbon_tax.csv"), header = TRUE, sep = ";", dec = ",")

df_carbon_tax <- df_carbon_tax %>% 
  filter(Type == "National Carbon tax") %>% 
  slice(1:30) %>% 
  mutate(ImplementationYear = as.numeric(substring(Status, 16, 19)))

df_long_carbon_tax <- df_carbon_tax %>% 
  slice(rep(1:n(), each = 34)) %>% 
  group_by(Country) %>% 
  mutate(year = seq(from=1990, to=2023)) %>% 
  ungroup() %>% 
  mutate(is_carbon_tax = ifelse(year < ImplementationYear, yes = 0, no = 1)) %>% 
  select(Country, year, ImplementationYear, is_carbon_tax, Share.of.jurisdiction.emissions.covered, Price.on.1.April, X2024) %>% 
  arrange(Country, year)

write.csv(df_long_carbon_tax, file = here(dir$prep_data,"carbon_tax2.csv"), row.names = FALSE)

#add iso3 à la df Country_PY_df

temp <- countries::country_reference_list
association_table <- temp %>%
  select(Name0, ISO3, simple) %>% 
  mutate(Country.Code = ISO3) %>% 
  select(ISO3,Name0) %>% 
  rename(country=Name0)

Country_PY_df_ISO3 <- Country_PY_df %>% 
  left_join(association_table,by="country")

#add ISO3 à la df_long_carbon_tax

df_long_carbon_tax <- df_long_carbon_tax %>% 
  rename(country=Country)

df_long_carbon_tax <- df_long_carbon_tax %>% 
  left_join(association_table,by="country")

## préparation Country_PY_df

# Avant tout: créer des observations pays-années avec des valeurs de variable 0 pour les années sans publication
# Et les ajouter au tableau country py df

countries <- unique(Country_PY_df$country)
years <- unique(Country_PY_df$PY)
df_final <- expand.grid(country = countries, PY = years)

df_final_join <- df_final %>% 
  left_join(Country_PY_df_ISO3, by = c("country", "PY")) %>% 
  mutate_all(~ ifelse(is.na(.), 0, .))%>%
  mutate(year=as.character(PY))

### Appariement

library(dplyr)

# Renommer la colonne dans chaque dataframe
df_long_CO2_emissions <- df_long_CO2_emissions %>% rename(ISO3 = Country.Code)
df_long_EducF <- df_long_EducF %>% rename(ISO3 = Country.Code)
df_long_EducH <- df_long_EducH %>% rename(ISO3 = Country.Code)
df_long_PIB <- df_long_PIB %>% rename(ISO3 = Country.Code)
df_long_RD <- df_long_RD %>% rename(ISO3 = Country.Code)

df_long_RD <- df_long_RD %>% rename(RDValues = "R&DValues")
df_long_carbon_tax <- df_long_carbon_tax %>%
  mutate(year=as.character(year))

df_final_merged <- df_final_join %>% 
  left_join(select(df_long_PIB, ISO3,year,PIBValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_RD, ISO3,year,RDValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_EducF, ISO3,year,EducFValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_EducH, ISO3,year,EducHValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_CO2_emissions, ISO3,year,CO2emissionsalues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_targets,ISO3,NDC_Implied_average,NDC_Implied_conditional,NDC_Implied_unconditional),by = "ISO3") %>% 
  left_join(select(df_long_carbon_tax,ISO3,year,is_carbon_tax),by = c("ISO3","year")) %>% 
  left_join(select(df_long_Expenditures,ISO3,year,ExpendituresValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_disasters,ISO3,year,DisastersValues),by = c("ISO3","year")) %>% 
  left_join(select(df_long_temperature,ISO3,year,TemperatureValues),by = c("ISO3","year"))%>% 
left_join(select(df_long_trade,ISO3,year,Comparative_advantage,Total_trade,Trade_balance),by = c("ISO3","year"))

write.csv(df_final_merged, here(dir$prep_data,"df_final_merged.csv"), row.names = FALSE)
