###############################################################
# OLS Regressions
#
# Purpose:
# Investigate the variables of interest and build a standard OLS model 
# that will serve as the logical base for a subsequent spatial econometric model
#
# Originator: Connor Wolf
# Date: 7/4/26
#
###############################################################

# Pertinent Libraries
library(car)
library(corrplot)
library(dplyr)

#Load Data into work space 
source("Seattle_Data.R")
view(R_tracts) 

###### CORRELATION TESTS----
# Before continuing to OLS model devlopment it is impotant to understand 
# the correlation between potential explitory vraibles and log apartmnet rent. 

# 1. Community Reporting Area Vacancy Rate
cor.test(
  R_tracts$Community.Reporting.Area.Vacancy.Rate,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 2. Unit Count
cor.test(
  R_tracts$UNIT_COUNT,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 3. Overall Walkability Index
cor.test(
  R_tracts$SE_Walkability_Index,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)

# 4. income 
cor.test(
  R_tracts$R_HiWageWk,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)

# WALKABILITY COMPONENTS
# 5. D2a - EpHHm
cor.test(
  R_tracts$D2a_EpHHm,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 6. D3B
cor.test(
  R_tracts$D3B,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 7. D4A
cor.test(
  R_tracts$D4A,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 8. D2b_E8MixA
cor.test(
  R_tracts$D2b_E8MixA,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# RANKED WALKABILITY COMPONENTS 

# 9. D2a_EpHHm_Ranked
cor.test(
  R_tracts$D2a_EpHHm_Ranked,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 10. D3B_Ranked
cor.test(
  R_tracts$D3b_Ranked,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 11. D4A_Ranked
cor.test(
  R_tracts$D4A_Ranked,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 12. D2b_E8MixA_Ranked
cor.test(
  R_tracts$D2b_E8MixA_Ranked,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)


# 13. Tree Canopy Area (2021)
cor.test(
  R_tracts$TreeCanopy_2021_Area,
  R_tracts$log_rentsqrft,
  use = "complete.obs",
  method = "pearson"
)

# Correlation between all model variables heatmap 

cor_all <- cor(
  R_tracts %>%
    st_drop_geometry() %>%
    select(
      log_rentsqrft,
      D2a_EpHHm,
      D2b_E8MixA,
      D3B,
      D4A,
      R_HiWageWk,
      Community.Reporting.Area.Vacancy.Rate,
      TreeCanopy_2021_Area,
      UNIT_COUNT
    ),
  use = "complete.obs"
)

corrplot(
  cor_all,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7
)



##### OLS MODELS ----

# model 1 
# Basic model to see if there is a casual relationship between a
# general walkability index and log apartment rents by tract in Seattle
model1 <- lm(log_rentsqrft ~ 
               SE_Walkability_Index,
  data = R_tracts)
summary(model1)

# model 2 
# Walkability with controls 

model2 <- lm(log_rentsqrft ~ 
               SE_Walkability_Index 
             + Community.Reporting.Area.Vacancy.Rate
             + TreeCanopy_2021_Area
             +UNIT_COUNT, 
             data = R_tracts)
summary(model2)

# model 3
# Breaking apart the walkability index to see if any of
# its components HAVE A CAUSL RELATIONSHIP with log apartment rent 

model3 <- lm(log_rentsqrft ~
               D2a_EpHHm
             + D2b_E8MixA
             + D3B
             +D4A,
             data = R_tracts)

summary(model3)

# vif test to investigate potential multicolinearity 
vif(model3)

# model 4
# This model takes the components of a regional Seattle walkability index 
# with added controls 

model4 <- lm(log_rentsqrft ~
               D2a_EpHHm
             + D2b_E8MixA
             + D3B
             +D4A
             + Community.Reporting.Area.Vacancy.Rate
             + TreeCanopy_2021_Area
             +UNIT_COUNT,
             data = R_tracts)

summary(model4)

# vif test to investigate potential multicolinaity 
vif(model4)





