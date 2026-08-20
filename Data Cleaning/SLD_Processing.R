###############################################################
# Smart Location Database Processing
#
# Purpose:
# Prepare smart location data for use in spatial econometric
# models. Preprocess, geographically harmonize, and spatially aggregate
# smart location data.
#
# LINK: https://www.epa.gov/smartgrowth/smart-location-mapping  
#
# Originator: Connor Wolf
# Date: 6/5/26
#
# Input: ~/Downloads/SmartLocationDatabaseV3/SmartLocationDatabase.gdb
#
# Output: sld_agg
###############################################################
# Libraries 
library(tidyverse)
library(sf)

# Import Data ----
gdb_path <- "~/Downloads/SmartLocationDatabaseV3/SmartLocationDatabase.gdb"
st_layers(gdb_path)

# 2. Read the layer (The layer name for this specific EPA database is "SmartLocationDatabase")
spatial_data <- st_read(dsn = gdb_path, 
                        layer = "EPA_SLD_Database_V3" )

# 3. Strip the spatial properties to make it a regular data frame
sld_df <- st_drop_geometry(spatial_data)
view(sld_df)
df_SE <- df%>%
  filter(CBSA_Name == "Seattle-Tacoma-Bellevue, WA")
view(df_SE)

#Construct GEOIDS 
df_SE <- df_SE %>% 
  mutate(
    tract_geoid = paste0(
      sprintf("%02s", STATEFP),
      sprintf("%03s", COUNTYFP),
      sprintf("%06s", TRACTCE)
    ),
    blockgroup_geoid = paste0(
      sprintf("%02s", STATEFP),
      sprintf("%03s", COUNTYFP),
      sprintf("%06s", TRACTCE),
      sprintf("%01s", BLKGRPCE)
    )
  )


# Geographic Harmonization ----
# NHGIS Geographic Crosswalk 
# LINK:https://www.nhgis.org/geographic-crosswalks
# crosswalk converting 2010 vintage to 2020 vintage
# Joined at block group level (now where do tracts come in)

# Load NHGIS georaphic crosswalk
nhgis.cw <- nhgis.cw %>%
  mutate(bg2010ge = as.character(bg2010ge))

# Join the geogrpahic crosswalk to df_SE.
sld_xwalk <- df_SE %>%
  inner_join(
    nhgis.cw,            
    by = c("blockgroup_geoid" = "bg2010ge")   
  )


# Data Type Conversion ----
# Converting pertinent variables to numeric to enable data aggregation by tract
vars_to_numeric <- c(
  # Household variables
  "HH", "CountHU", "AutoOwn0", "AutoOwn1", "AutoOwn2p",
  
  # Population variables
  "TotPop", "Workers", "P_WrkAge", "TotEmp",
  
  # Employment variables
  "TotEmp", "E5_Ret", "E5_Off", "E5_Ind", "E5_Svc", "E5_Ent",
  "E8_Ret", "E8_off", "E8_Ind", "E8_Svc", "E8_Ent",
  "E8_Ed", "E8_Hlth", "E8_Pub",
  "E_LowWageWk", "E_MedWageWk", "E_HiWageWk",
  "R_LowWageWk", "R_MedWageWk", "R_HiWageWk",
  
  # Design (D3)
  "D3A", "D3AAO", "D3AMM", "D3APO",
  "D3B", "D3BAO", "D3BMM3", "D3BMM4",
  "D3BPO3", "D3BPO4",
  
  # Transit (D4)
  "D4A", "D4B025", "D4B050",
  "D4C", "D4D", "D4E"
)

sld_xwalk <- sld_xwalk %>%
  mutate(across(all_of(vars_to_numeric), as.numeric))


# Spatial Aggregation by tract ----
# The following adjusts simple variables using an appropreate 
# corresponding weight provided by the nhgis crosswalk. 

sld_agg <- sld_xwalk %>%
  group_by(tr2020ge) %>%
  summarise(
    TotPop = sum(TotPop * wt_pop, na.rm = TRUE),   
    HH     = sum(HH * wt_hh, na.rm = TRUE),
    CountHU = sum(CountHU * wt_hu, na.rm = TRUE),
    R_HiWageWk = sum(R_HiWageWk * wt_pop, na.rm = TRUE), 
    AutoOwn0 = sum(AutoOwn0 * wt_hh, na.rm = TRUE),
    AutoOwn1 = sum(AutoOwn1 * wt_hh, na.rm = TRUE),
    AutoOwn2p = sum(AutoOwn2p * wt_hh, na.rm = TRUE),
    TotEmp = sum(TotEmp * wt_pop, na.rm =  TRUE),
    # Use population weight for employment factors, assume employment distributes the same as population
    E5_Ret = sum(E5_Ret * wt_pop, na.rm = TRUE),
    E5_Off = sum(E5_Off * wt_pop, na.rm = TRUE),
    E5_Ind = sum(E5_Ind * wt_pop, na.rm = TRUE),
    E5_Svc = sum(E5_Svc * wt_pop, na.rm = TRUE),
    E5_Ent = sum(E5_Ent * wt_pop, na.rm = TRUE),
    E8_Ret = sum(E8_Ret * wt_pop, na.rm = TRUE),
    E8_off = sum(E8_off * wt_pop, na.rm = TRUE),
    E8_Ind = sum(E8_Ind * wt_pop, na.rm = TRUE),
    E8_Svc = sum(E8_Svc * wt_pop, na.rm = TRUE),
    E8_Ent = sum(E8_Ent * wt_pop, na.rm = TRUE),
    E8_Ed  = sum(E8_Ed  * wt_pop, na.rm = TRUE),
    E8_Hlth = sum(E8_Hlth * wt_pop, na.rm = TRUE),
    E8_Pub = sum(E8_Pub * wt_pop, na.rm = TRUE),
    # use parea as a weight to estimate spacial densities 
    D3A    = sum(D3A    * parea, na.rm = TRUE),
    D3AAO  = sum(D3AAO  * parea, na.rm = TRUE),
    D3AMM  = sum(D3AMM  * parea, na.rm = TRUE),
    D3APO  = sum(D3APO  * parea, na.rm = TRUE),
    D3B    = sum(D3B    * parea, na.rm = TRUE),
    D3BAO  = sum(D3BAO  * parea, na.rm = TRUE),
    D3BMM3 = sum(D3BMM3 * parea, na.rm = TRUE),
    D3BMM4 = sum(D3BMM4 * parea, na.rm = TRUE),
    D3BPO3 = sum(D3BPO3 * parea, na.rm = TRUE),
    D3BPO4 = sum(D3BPO4 * parea, na.rm = TRUE),
    # wt_pop used to scale D4A 
    # things like housing and infrastructure are more associated with people than land area  
    D4A    = sum(D4A    * wt_pop,  na.rm = TRUE),
    D4B025 = sum(D4B025 * parea,  na.rm = TRUE),
    D4B050 = sum(D4B050 * parea,  na.rm = TRUE),
    D4C    = sum(D4C    * parea,  na.rm = TRUE),
    D4D    = sum(D4D    * parea,  na.rm = TRUE),
    D4E    = sum(D4E    * wt_pop, na.rm = TRUE),
    .groups= "drop"
  )


# Complex SLD Variables ----
# The EPA's SLD carries a variety of more advanced descriptors/statitcis/varibles dervied 
# from other observable SLD variables. This subsequent section of script recalulates these 
# variables using the aggregated, adjusted (harmonized?) 2020 vinatge data. 

## D2b_E8MixA ----
# This entropy variable uses the eight-tier
# employment categories to calculate employment
# mix. The entropy denominator i s set to all eight
# employment types within each CBG.
# Calculated with formulas provided per SLD technical documentation.


# This function accounts for possible zero values incombatable with natural log.
entropy_term <- function(x, total) {
  p <- x / total
  ifelse(p > 0, p * log(p), 0)
}

# Calculate E, an indeterminate result needed to derive D2b_E8MixA.
sld_agg$E <-
  entropy_term(sld_agg$E8_Ret,  sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_off,  sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Ind,  sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Svc,  sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Ent,  sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Ed,   sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Hlth, sld_agg$TotEmp) +
  entropy_term(sld_agg$E8_Pub,  sld_agg$TotEmp)

# Establish D2b_E8MixA
sld_agg$D2b_E8MixA <- -sld_agg$E / log(8)


## D2a_EpHHm ----
# Employment and household entropy
# calculations, where employment and occupied
# housing are both included in the entropy
# calculations. This measure uses the five-tier
# employment categories.

#Compute intermediate results
# Total activity (households + employment)
sld_agg$TotAct <- sld_agg$TotEmp + sld_agg$HH

# Number of non-zero activity categories
sld_agg$N <-
  (sld_agg$HH     > 0) +
  (sld_agg$E5_Ret > 0) +
  (sld_agg$E5_Off > 0) +
  (sld_agg$E5_Ind > 0) +
  (sld_agg$E5_Svc > 0) +
  (sld_agg$E5_Ent > 0)

# Calculate D2a_EpHHm
sld_agg$D2a_EpHHm <- -(
  entropy_term(sld_agg$HH,     sld_agg$TotAct) +
    entropy_term(sld_agg$E5_Ret, sld_agg$TotAct) +
    entropy_term(sld_agg$E5_Off, sld_agg$TotAct) +
    entropy_term(sld_agg$E5_Ind, sld_agg$TotAct) +
    entropy_term(sld_agg$E5_Svc, sld_agg$TotAct) +
    entropy_term(sld_agg$E5_Ent, sld_agg$TotAct)
) / log(sld_agg$N)


## Ranked Variables ----
# create the ranked variables needed to calculate
# the regional walkbality score 
# Note these varibles rank wlakability ralitive to tracts belonging to the 
# Seattle Metroplotain what not 

# Rank D2b_E8MixA (Employment Mix)
sld_agg <- sld_agg %>%
  mutate(
    D2b_E8MixA_Ranked = ntile(D2b_E8MixA, 20)
  )

# Rank D2a_EpHHm (Employment and Household Mix)
sld_agg <- sld_agg %>%
  mutate(
    D2a_EpHHm_Ranked = ntile(D2a_EpHHm, 20)
  )

#  Rank D3b
sld_agg <- sld_agg %>%
  mutate(
    D3b_Ranked = ntile(D3B, 20)
  )

# Rank D4a 
sld_agg <- sld_agg %>%
  mutate(
    D4A_Ranked = ntile(D4A, 20)
  )

##  Seattle Walkabilty Index ----
# The relative walkability index incorporates the same formula provided by the EPA
# used for their national walkbaility index. However, given the truncated domain of this 
# data set the formula yields a walkability index unique to the Seattle Metropolitan Statistical 
# Area. 
# Calculated using the same formula as the national walkability index 
# but with this truncated data set gives a regional index unique to Seattle

# Linear combination of elements-- weighted sum 
sld_agg$SE_Walkability_Index <-
  (sld_agg$D4A_Ranked / 3) +
  (sld_agg$D3b_Ranked / 3) +
  (sld_agg$D2a_EpHHm_Ranked / 6) +
  (sld_agg$D2b_E8MixA_Ranked / 6)


# To ensure this XXX SLD dataframe can be joined to other seattle tract level data farmes 
# store tr2020ge as a character. 
sld_agg <- sld_agg %>% 
  mutate(tr2020ge = as.character(tr2020ge))


