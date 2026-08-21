# Data Preparation  
Each dataset corresponds to a different R script. Here, data is cleaned, harmonized, and grouped by geoid.  
Some datasets record observations at the block level while others do so at the tract level. Moreover, some datasets 
work within the 2010 census vintage while others work within the 2020 census vintage. In both cases, an NHGIS crosswalk 
is used to map block geoids and 2010 census geoids to the desired 2020 vintage with tract-level observations. 

The file Seattle_Data.R combines each of these data frames into one research-ready data frame grouped by tract.  
