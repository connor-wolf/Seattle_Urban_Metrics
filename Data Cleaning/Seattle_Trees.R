###############################################################
# Seattle Tree Canopy Data Frame
#
# Purpose:
# To establish a data frame holding Seattle tree coverage data. 
# Contains observations from both 2016 and 2021 by tract. 
#
# LINK:   https://data-seattlecitygis.opendata.arcgis.com/datasets/SeattleCityGIS::seattle-tree-canopy-2021-tree-crowns/about
#
# Originator: Connor Wolf
# Date: 6/1/26
#
# Input: Seattle_Tree_Canopy.csv
#
# Output:
###############################################################

library(tidyverse)

# Pull data from linked CSV file 
seattle_trees <- read_csv("Seattle_Tree_Canopy.csv") %>%
  mutate(ID = as.character(ID))
view(seattle_trees)

##### Data Harmonization----

# nhgis Crosswalk 
# LINK:https://www.nhgis.org/geographic-crosswalks
nhgis.cw <- nhgis.cw %>%
  mutate(bg2010ge = as.character(bg2010ge))
view(nhgis.cw)
names(nhgis.cw)

# Join Crosswalk
# crosswalk converting 2010 vintage to 2020 vintage
# Joined at block group level
trees_xwalk <- seattle_trees %>%
  inner_join(
    nhgis.cw,            
    by = c("ID" = "bg2010ge")   
  )

# now aggregate using the parea scalar
df_seattle_trees <- trees_xwalk %>%
  group_by(tr2020ge) %>%
  summarise(
    # Area variables
    Total_A = sum(Total_A * parea, na.rm = TRUE),
    Can_A = sum(Can_A * parea, na.rm = TRUE),
    Grass_A = sum(Grass_A * parea, na.rm = TRUE),
    Soil_A = sum(Soil_A * parea, na.rm = TRUE),
    Water_A = sum(Water_A * parea, na.rm = TRUE),
    Build_A = sum(Build_A * parea, na.rm = TRUE),
    Road_A = sum(Road_A * parea, na.rm = TRUE),
    Paved_A = sum(Paved_A * parea, na.rm = TRUE),
    Perv_A = sum(Perv_A * parea, na.rm = TRUE),
    Imperv_A = sum(Imperv_A * parea, na.rm = TRUE),
    
    TreeCanopy_2016_Area = sum(TreeCanopy_2016_Area * parea, na.rm = TRUE),
    TreeCanopy_2021_Area = sum(TreeCanopy_2021_Area * parea, na.rm = TRUE),
    
    Gain = sum(Gain * parea, na.rm = TRUE),
    Loss = sum(Loss * parea, na.rm = TRUE),
    No_Change = sum(No_Change * parea, na.rm = TRUE),
    
    .groups = "drop"
  ) 
 
# make tr2020ge a character to facilitate future uses 
df_seattle_trees <- df_seattle_trees %>% 
  mutate(tr2020ge = as.character(tr2020ge))

