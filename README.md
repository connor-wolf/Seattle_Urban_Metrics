# Seattle_Urban_Metrics
This project aims to employ econometric methods to better understand how urban characteristics affect apartment rents in Seattle. The primary variable of interest in this study is the EPA's walkability index. This index is a linear combination of underlying variables also included in the EPA's Smart Location Database. These underlying variables are also variables of interest and, in some cases, have a far stronger correlation with apartment rent than the walkability index itself. 

# Data and Model Development 
A research-ready dataset comprised of public tree canopy, vacancy rate, rent typology, and urban metrics data was built. Schema differences between the subsidiary datasets necessitated the use of NHGIS crosswalks.

After the establishment of a core data frame, OLS models were constructed using variance inflation factor tests and preexisting literature to select viable controls to pair with walkability and its components. Then spatial autoregressive models were built based on the established OLS models using Moran's I and LM tests to aid in a top-down model development process. 

# Findings 
Walkability shares only a weak positive correlation with log apartment rent per sqft; however, the variables D4a and D2a_EpHHm, both components of walkability, exhibit notably stronger correlations with log apartment rent per sqft— r = -0.4532742 and r = 0.4539736, respectively. D4a is the distance from a tract's population-weighted centroid to the nearest transit stop in meters, while D2a_EpHHm measures employment and household entropy.

# Notes 
The primary issue with the models here comes from a lack of hedonic apartment data and the potential variability of walkability within tracts. Furthermore, tract-level data not only reduces the accuracy of results but also results in high sampling error. 

Also note that all documents and scripts included are only those not actively being developed or revised. 
