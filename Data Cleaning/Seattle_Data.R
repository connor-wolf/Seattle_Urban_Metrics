###############################################################
# Seattle Urban Characteristic Data Frame
#
# Purpose:
# Merge the prepared Smart Location Database with the Seattle rent typology, 
# Seattle tree canopy, and vacancy rate data frames.
#
# Originator: Connor Wolf
# Date: 7/1/26
#
# Input:
# df_Seattle_Rent_panel [1], df_Seattle_Rent_2025 [2], SE_Vacancy_Rates_2025 [3], df_seattle_trees [4]
# Output:
# final_df, SE_Urban_Metrics
###############################################################


##### Merge non-spatial data frames ---- 
final_df <- sld_agg %>% # houses only input data frames 1 and 2
  inner_join(
    df_Seattle_Rent_2025,
    by = c("tr2020ge" = "GEOID_clean")
  )

SE_Urban_Metrics <- final_df %>% # Contains all three input data frames
  inner_join(
    SE_Vacancy_Rates_2025,
    by = c("tr2020ge" = "GEOID")
  )
SE_Urban_Metrics <- left_join(SE_Urban_Metrics, df_seattle_trees, by = "tr2020ge")

names(SE_Urban_Metrics)
nrow(SE_Urban_Metrics) # model now conditional with tracts with observed rent 

##### Spatial Characteristics----
#  Add spatial characteristics to support future models 


options(tigris_use_cache = TRUE)

# Load geographic tract boundaries using the tigris library
wa_tracts <- tracts(
  state = "WA",
  year = 2020,
  class = "sf"
)

# Filter only tracts relevant to the data frame storing Seattle urban characteristics (SE_Urban_Metrics)
king_tracts <- wa_tracts %>% #Only tracts from king county (Seattle)
  filter(COUNTYFP == "033")

# Join spatial characteristics to the cross section holding geographic information
R_tracts <- king_tracts %>%
  inner_join(SE_Urban_Metrics, by = c("GEOID" = "tr2020ge"))

# R_tracts holds cross sectional data with spatial characteristics sorted by tract

