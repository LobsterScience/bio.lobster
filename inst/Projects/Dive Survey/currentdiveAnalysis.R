###Current Dive survey -- GLMM
require(dplyr)
require(devtools)
require(sf)
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)
require(spaMM)

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")

diveT = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOF_DiveFormatted.csv")


### Depth should be corrected for tides and rerun?

## Should this be corrected for density of lobster first since some years there are more transects ? 
##Effort is different or is that captured by the number of transects being different??



## Presence of lobster with temp, depth, substrate type, year , transect and diver effects
presence_model<- fitme(lobster ~  bottom_temp + avg_depth + substrate_1 + substrate_2 + 
                 (1 | year) + (1 | transect_id) + (1 | diver), 
               data = diveT, family = binomial(link="logit"))

summary(presence_model)


# GLMM for lobster size
size_model <- fitme(
  carapace ~ bottom_temp + avg_depth + substrate_1 + substrate_2 + (1 | year) + (1 | transect_id) + (1 | diver),
  data = diveT,
  family = gaussian(link = "identity")
)

summary(size_model)


### Comparing results
# AIC values
aic_presence <- AIC(presence_model)
aic_size <- AIC(size_model)

