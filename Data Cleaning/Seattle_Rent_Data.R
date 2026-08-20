###############################################################
# Seattle Rent Typology Data Frames
#
# Purpose:
# Establish both cross-sectional and panel data frames for 
# Seattle rent typology data provided by the City of Seattle. 
# LINK:  https://catalog.data.gov/dataset/apartment-market-rent-prices-by-census-tract
#
# Originator: Connor Wolf
# Date: 6/1/26
#
# Input: "RentTypology_-4394779434333664244.csv"
#
# Output: 
# df_Seattle_Rent_panel, df_Seattle_Rent_2025
###############################################################

# Import the provided comma separated data
df_Seattle_Rent <- read.csv("RentTypology_-4394779434333664244.csv") #Seattle Rent Typology

# panel data frame
df_Seattle_Rent_panel <- df_Seattle_Rent %>% # restricting panel data (major restriction)
  filter(Year %in% c(2020, 2021, 2022, 2023, 2024, 2025))
view(df_Seattle_Rent_panel)

# cross sectional data frame
df_Seattle_Rent_2025 <- df_Seattle_Rent %>% # major restriction
  filter(Year == 2025)

# Convert tract geoids to character to facilitate merge for both dfs
df_Seattle_Rent_panel$GEOID_clean <- as.character(df_Seattle_Rent_panel$GEOID)
df_Seattle_Rent_2025$GEOID_clean <- as.character(df_Seattle_Rent_2025$GEOID)

# Create Logged Rent Variables for both dfs
# For realism take the log of the rent variables 

df_Seattle_Rent_2025 <- df_Seattle_Rent_2025 %>%
  filter(Tract.Median.Apartment.Contract.Rent.per.Square.Foot > 0) %>%
  mutate(log_rentsqrft = log(Tract.Median.Apartment.Contract.Rent.per.Square.Foot))

df_Seattle_Rent_panel <- df_Seattle_Rent_panel %>%
  filter(Tract.Median.Apartment.Contract.Rent.per.Square.Foot > 0) %>%
  mutate(log_rentsqrft = log(Tract.Median.Apartment.Contract.Rent.per.Square.Foot))

