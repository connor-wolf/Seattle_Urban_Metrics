###############################################################
# Seattle Apartment Vcacy Rates by Tracts
#
# Purpose:
# 
#
# LINK: https://data.seattle.gov/dataset/Apartment-Market-Vacancy-by-Census-Tract/93bk-qn72/about_data
#
# Originator: Connor Wolf
# Date: 
#
# Input: Vacancy_8877360878413823137.csv
#
# Output: SE_Vacancy_Rates_2025
###############################################################

# Load Data
SE_Vacancy_Rates <- read.csv("Vacancy_8877360878413823137.csv")
view(SE_Vacancy_Rates_2025)

# Pull 2025 cross section 
SE_Vacancy_Rates_2025 <- SE_Vacancy_Rates %>% # major restriction
  filter(Year == 2025)

# Convert GEOID to a charter variable to facilitate future treatment 
SE_Vacancy_Rates_2025$GEOID <- as.character(SE_Vacancy_Rates_2025$GEOID)

                                            
