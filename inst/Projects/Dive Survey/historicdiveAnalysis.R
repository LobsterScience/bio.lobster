require(sf)
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)
require(spaMM)
require(MASS)

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")

diveT = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/OD_DiveFormatted.csv")

diveT <- diveT %>% filter(LFA != 35)

sum_diveT<- diveT %>%
  group_by(transect) %>%
  summarize(
    total_lobster = sum(lobster, na.rm = TRUE),
    year = first(year),
    month = first(month),
    LFA = first(LFA),
    transect_length = first(transectlength),
    transect_width = first(transectwidth)
  )

sum_diveT$areasamp = (sum_diveT$transect_length*sum_diveT$transect_width)

sum_diveT<-as.data.frame(sum_diveT)






# GLMM for lobster presence (total count data) - Hierarchical cause of the LFA
presence_model <- HLfit(lobster ~ secPrimarySub + secSecondarySub + year + month + (1|LFA/transect),
                        family = poisson(), data = diveT)
summary(presence_model)

# Check for over dispersion? Looks ok for poisson
dispersion_test <- sum(residuals(presence_model, type = "pearson")^2) / df.residual(presence_model)

# GLMM for lobster size
size_model <- HLfit(carapacemm ~ secPrimarySub + secSecondarySub + year + month + (1|LFA/transect),
                    family = gaussian(), data = diveT)
summary(size_model)


### Comparing results
# AIC values
aic_presence <- AIC(presence_model)
aic_size <- AIC(size_model)




############# no mixed model 
nomix_model<-glm.nb(lobster ~  secPrimarySub + secSecondarySub + year + month +LFA:transect,
                 data = diveT)


diveTnew<-diveT %>%
  filter(!is.na(secPrimarySub))%>%
  mutate(res=nomix_model$residuals,
         fit=nomix_model$fitted.values)


ggplot(aes(x=res, y=fit, colour=lobster),data=diveTnew)+geom_point()

