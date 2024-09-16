### COMBINED DIVE ANALYSIS ###
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(nlme)
require(glmmTMB)
require(DHARMa)


setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")

diveH = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/OD_DiveFormatted.csv")

diveH <- diveH %>% filter(LFA != 35)

sum_diveH<- diveH %>%
  group_by(transect) %>%
  summarize(
    total_lobster = sum(lobster, na.rm = TRUE),
    year = first(year),
    month = first(month),
    LFA = first(LFA),
    region=first(region),
    transect_length = first(transectlength),
    transect_width = first(transectwidth)
  )

sum_diveH$areasamp = (sum_diveH$transect_length*sum_diveH$transect_width)

sum_diveH <- sum_diveH %>% rename(tid = transect)
sum_diveH$tid<-as.character(sum_diveH$tid)
sum_diveH<-as.data.frame(sum_diveH)



diveC = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOF_DiveFormatted.csv")
diveC$LFA = 38
diveC$region = "Grand Manan"

sum_diveC<- diveC %>%
  group_by(tid) %>%
  summarize(
    total_lobster = sum(lobster, na.rm = TRUE),
    year = first(year),
    month = first(month),
    LFA = first(LFA),
    region=first(region),
    transect_length = first(transect_length),
    transect_width = first(transect_width)
  )

sum_diveC$areasamp = (sum_diveC$transect_length*sum_diveC$transect_width)

sum_diveC<-as.data.frame(sum_diveC)

combined_dive <- bind_rows(sum_diveH, sum_diveC)

#combined_dive$lobster_density <- (combined_dive$total_lobster / combined_dive$areasamp)*100  ## get density of lobster


#############################################

## Data is aggregated at the Transect level ##
####### negative binomial with lobster count and an offset for area... GLMM ####
nb_1 <- glmmTMB(total_lobster ~ year + 
                      month + 
                      LFA + 
                    offset(log(areasamp)), 
                    data = combined_dive, 
                    family = nbinom2())
summary(nb_1)



nb_2 <- glmmTMB(total_lobster ~ year + 
                  month + 
                  offset(log(areasamp)), 
                data = combined_dive, 
                family = nbinom2())
summary(nb_2)


nb_3<- glmmTMB(total_lobster ~ year * month + 
                                  LFA + 
                                  offset(log(areasamp)), 
                                data = combined_dive, 
                                family = nbinom2())
summary(nb_3)



## Get residuals in glmm models
res1model<-residuals(nb_1, type = "response")
combined_dive$residuals_1<-res1model
plot(y=combined_dive$residuals_1, x=combined_dive$year)


res2model<-residuals(nb_2, type = "response")
combined_dive$residuals_2<-res2model
plot(y=combined_dive$residuals_2, x=combined_dive$year)


res3model<-residuals(nb_3, type = "response")
combined_dive$residuals_3<-res2model
plot(y=combined_dive$residuals_3, x=combined_dive$year)

#### High variation especially around 200-

ggplot(aes(x = factor(year), y = residuals_1),data=combined_dive) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

ggplot(aes(x = factor(year), y = residuals_2), data = combined_dive) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

ggplot(aes(x = factor(year), y = residuals_3), data = combined_dive) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

## AIC AND BIC
# Extract AIC and BIC values
aic_nb1 <- AIC(nb_1)
bic_nb1 <- BIC(nb_1)

aic_nb2 <- AIC(nb_2)
bic_nb2 <- BIC(nb_2)

aic_nb3 <- AIC(nb_3)
bic_nb3 <- BIC(nb_3)

# Print AIC and BIC values
cat("Model 1 - AIC:", aic_nb1, "BIC:", bic_nb1, "\n")
cat("Model 2 - AIC:", aic_nb2, "BIC:", bic_nb2, "\n")
cat("Model 3 - AIC:", aic_nb3, "BIC:", bic_nb3, "\n")

# Residual plots
par(mfrow = c(3, 1)) 
plot(residuals(nb_1), main = "Residuals Plot - Model 1", ylab = "Residuals", xlab = "Index")
plot(residuals(nb_2), main = "Residuals Plot - Model 2", ylab = "Residuals", xlab = "Index")
plot(residuals(nb_3), main = "Residuals Plot - Model 3", ylab = "Residuals", xlab = "Index")

anova(nb_1, nb_2, nb_3)

### summary Model 2 the simple one is the best fit



## Data is aggregated at the month of sampling level ##


dive_monthly <- combined_dive %>%
  group_by(year, month, LFA) %>%
  summarise(
    tid = n_distinct(tid),
    total_lobster = sum(total_lobster),
    areasamp_month = sum(areasamp)
  ) %>%
  ungroup()

#year*month+LFA+(1|monthly sept area)   total swept area per month would be offset 
nb_A <- glmmTMB(total_lobster ~ year +
                  month+
                  LFA +
                  offset(log(areasamp_month)), 
                data = dive_monthly, 
                family = nbinom2())
summary(nb_A)

nb_B<- glmmTMB(total_lobster  ~ year + 
                 LFA+
                 offset(log(areasamp_month)), 
               data = dive_monthly, 
               family = nbinom2())
summary(nb_B)

nb_C<- glmmTMB(total_lobster  ~ year + 
                 offset(log(areasamp_month)), 
               data = dive_monthly, 
               family = nbinom2())
summary(nb_C)



nb_D<- glmmTMB(total_lobster ~ year * month + 
                 LFA + 
                 offset(log(areasamp_month)), 
               data = dive_monthly, 
               family = nbinom2())
summary(nb_D)


## Get residuals in glmm models
resAmodel<-residuals(nb_A, type = "response")
dive_monthly$residuals_A<-resAmodel
plot(y=dive_monthly$residuals_A, x=dive_monthly$year)


resBmodel<-residuals(nb_B, type = "response")
dive_monthly$residuals_B<-resBmodel
plot(y=dive_monthly$residuals_B, x=dive_monthly$year)


resCmodel<-residuals(nb_C, type = "response")
dive_monthly$residuals_C<-resCmodel
plot(y=dive_monthly$residuals_C, x=dive_monthly$year)

resDmodel<-residuals(nb_D, type = "response")
dive_monthly$residuals_D<-resDmodel
plot(y=dive_monthly$residuals_D, x=dive_monthly$year)



ggplot(aes(x = factor(year), y = residuals_A),data=dive_monthly) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

ggplot(aes(x = factor(year), y = residuals_B), data = dive_monthly) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

ggplot(aes(x = factor(year), y = residuals_C), data = dive_monthly) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)


ggplot(aes(x = factor(year), y = residuals_D), data = dive_monthly) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)

## AIC AND BIC
# Extract AIC and BIC values
aic_nbA <- AIC(nb_A)
bic_nbA <- BIC(nb_A)

aic_nbB <- AIC(nb_B)
bic_nbB <- BIC(nb_B)

aic_nbC <- AIC(nb_C)
bic_nbC <- BIC(nb_C)

aic_nbD <- AIC(nb_D)
bic_nbD <- BIC(nb_D)
# Print AIC and BIC values
cat("Model A - AIC:", aic_nbA, "BIC:", bic_nbA, "\n")
cat("Model B - AIC:", aic_nbB, "BIC:", bic_nbB, "\n")
cat("Model C - AIC:", aic_nbC, "BIC:", bic_nbC, "\n")
cat("Model D - AIC:", aic_nbD, "BIC:", bic_nbD, "\n")
# Residual plots
par(mfrow = c(2, 2)) 
plot(residuals(nb_A), main = "Residuals Plot - Model A", ylab = "Residuals", xlab = "Index")
plot(residuals(nb_B), main = "Residuals Plot - Model B", ylab = "Residuals", xlab = "Index")
plot(residuals(nb_C), main = "Residuals Plot - Model C", ylab = "Residuals", xlab = "Index")
plot(residuals(nb_D), main = "Residuals Plot - Model D", ylab = "Residuals", xlab = "Index")

anova(nb_A, nb_B, nb_C, nb_D)

##Summary: Model C fits best... is the simplest 

### Data is subset to only September because that's when most consistent sampling was done ##


sept_dive <- dive_monthly %>%
  filter(month == 9)
sept_dive <- sept_dive %>%
  select(-residuals_A, -residuals_B, -residuals_C, -residuals_D)


nb_11 <- glmmTMB(total_lobster ~ year +
                   LFA+
                  offset(log(areasamp_month)), 
                data = sept_dive, 
                family = nbinom2())
summary(nb_11)

nb_22 <- glmmTMB(total_lobster ~ year +
                   offset(log(areasamp_month)), 
                 data = sept_dive, 
                 family = nbinom2())
summary(nb_22)

## Get residuals in glmm models
res11model<-residuals(nb_11, type = "response")
sept_dive$residuals_11<-res11model
plot(y=sept_dive$residuals_11, x=sept_dive$year)

res22model<-residuals(nb_22, type = "response")
sept_dive$residuals_22<-res22model
plot(y=sept_dive$residuals_22, x=sept_dive$year)


ggplot(aes(x = factor(year), y = residuals_11),data=sept_dive) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)


ggplot(aes(x = factor(year), y = residuals_22),data=sept_dive) +
  geom_boxplot() +
  labs(x = "Year", y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ LFA)


## AIC AND BIC
# Extract AIC and BIC values
aic_nb11 <- AIC(nb_11)
bic_nb11 <- BIC(nb_11)

aic_nb22 <- AIC(nb_22)
bic_nb22 <- BIC(nb_22)


# Print AIC and BIC values
cat("Model 11 - AIC:", aic_nb11, "BIC:", bic_nb11, "\n")
cat("Model 22 - AIC:", aic_nb22, "BIC:", bic_nb22, "\n")

# Residual plots
plot(residuals(nb_11), main = "Residuals Plot - Model 11", ylab = "Residuals", xlab = "Index")

plot(residuals(nb_22), main = "Residuals Plot - Model 22", ylab = "Residuals", xlab = "Index")


anova(nb_11, nb_22)
### Summary: LFA doesn't really make a difference






# read JARA use ?
### Try Ar1() option for autoregression Order
#When to Use AR(1):
 # Temporal Data: When you have repeated measures over time and expect that observations closer in time are more correlated.
#Spatial Data: When you have spatial data and expect that observations closer in space are more correlated.
#Benefits:
#  Improves Model Fit: By accounting for correlations in the data, the model can provide more accurate estimates.
#Reduces Bias: Ignoring correlations can lead to biased parameter estimates and incorrect inferences.