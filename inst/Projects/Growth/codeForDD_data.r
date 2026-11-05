require(readxl)
library(dplyr)

dat = read_excel('C:/Users/cooka/Documents/Drolet_et_al_raw_data.xlsx',sheet='Raw')
names(dat)[4] = 'CL'


growth <- dat %>%
  mutate(
    Date = as.Date(Date),
    Population = sub("[0-9]+$", "", Individual),
    Individual = paste(Individual, Temperature, sep="_")
    
  ) %>%
  arrange(Temperature, Individual, Date) %>%
  group_by(Temperature, Individual) %>%
  mutate(
    EndDate = lead(Date),
    EndCL = lead(CL),
    Days = as.numeric(EndDate - Date),
    Growth = EndCL - CL
  ) %>%
  filter(!is.na(EndCL)) %>%
  transmute(
    Temperature,
    Population,
    Individual,
    StartDate = Date,
    EndDate,
    Days,
    StartCL = CL,
    EndCL,
    Growth
  ) %>%
  ungroup()

grow1 = growth %>%
  mutate(GrowthRate = Growth / Days) %>%
  group_by(Temperature, Population) %>%
  summarise(
    n = n(),
    MeanGrowthRate = mean(GrowthRate, na.rm = TRUE),
    SD = sd(GrowthRate, na.rm = TRUE)
  )


growth <- growth %>%
  filter(Days>0) %>%
  mutate(
    GrowthRate = Growth / Days
  )



ggplot(grow1,aes(x=Temperature, y=MeanGrowthRate))+geom_point()+geom_smooth()+facet_wrap(~Population)



library(minpack.lm)
####does not work
weibull_tpc <- function(temp, rmax, topt, b, c){
  
  x <- (temp - topt)/b + ((c - 1)/c)^(1/c)
  
  pred <- rep(NA_real_, length(x))
  
  ok <- x > 0
  
  pred[ok] <-
    rmax *
    ((c - 1)/c)^((1 - c)/c) *
    x[ok]^(c - 1) *
    exp(
      -x[ok]^c +
        (c - 1)/c
    )
  
  pred
}

gBF = subset(growth,Population == 'BF')
fit <- nlsLM(
  GrowthRate ~ weibull_tpc(
    Temperature,
    rmax,
    topt,
    b,
    c
  ),
  data = gBF,
  start = list(
    rmax = 0.15,
    topt = 21,
    b = 10,
    c = 3
  ),
  lower = c(
    rmax = 0,
    topt = 5,
    b = 0.1,
    c = 1.01
  )
)
######################################

growth$Population = as.factor(growth$Population)
library(gamm4)

m <- gamm4(
  GrowthRate ~
    Population +
    s(Temperature, by = Population) +
    (StartCL),
  random = ~(1|Individual),
  data = growth
)


m1 <- gamm4(
  GrowthRate ~
    Population +
    s(Temperature) +
    (StartCL),
  random = ~(1|Individual),
  data = growth
)
AIC(m1$mer, m$mer)


newdat <- expand.grid(
  Temperature = seq(5, 28, length = 200),
  Population = levels(growth$Population),
  StartCL = mean(growth$StartCL)
)

pred <- predict(
  m$gam,
  newdat,
  se.fit = TRUE
)

newdat$fit <- pred$fit
newdat$lwr <- pred$fit - 2 * pred$se.fit
newdat$upr <- pred$fit + 2 * pred$se.fit

library(ggplot2)

ggplot(newdat,
       aes(Temperature, fit,
           colour = Population,
           fill = Population)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    data = growth,
    aes(Temperature, GrowthRate),
    colour = "grey60",
    alpha = 0.3,
    position = position_jitter(width = 0.02)
  )+

  ylab("Growth rate (mm/day)") +
  theme_bw()


#######################################################
##survival

library(dplyr)
library(lubridate)
library(dplyr)
library(stringr)
library(dplyr)
library(stringr)

surv_end <- dat %>%
  mutate(
    Population = str_extract(Individual, "^[A-Za-z]+")
  ) %>%
  group_by(Temperature, Population) %>%
  summarize(
    Survivors = n_distinct(Individual[Date == max(Date)]),
    Survival = Survivors / 10,
    .groups = "drop"
  )

oSur = aggregate(Survivors~Temperature, data=surv_end,FUN=sum)


############################################################
growth$Ind = as.factor(growth$Individual)
m <- gam(
  EndCL ~
    te(StartCL, Temperature, Days) +
    s(Ind, bs = "re"),
  data = growth,
  method = "REML"
)



##########################################
##spline model

library(TMB)
setwd('C:/Users/cooka/Documents/tmp')

compile("growth.cpp")
dyn.load(dynlib("growth"))

growth$IndividualF <- as.factor(growth$Individual)
library(mgcv)

sm <- smoothCon(
  te(Temperature, StartCL,
     bs = c("tp","tp"),
     k = c(5,5)),
  data = growth
)[[1]]

Data <- list(
  StartCL = growth$StartCL,
  EndCL = growth$EndCL,
  Days = growth$Days,
  X = sm$X,
  S = sm$S[[1]],
  Individual = as.integer(factor(growth$Individual)) - 1
)

Parameters <- list(
  beta0 = 0,
  beta = rep(0, ncol(sm$X)),
  log_sigma_beta = log(0.1),
  log_sigma_obs = log(0.1),
  log_sigma_ind = log(0.1),
  u = rep(0, nlevels(growth$IndividualF))
)

obj <- MakeADFun(
  Data,
  Parameters,
  DLL = "growth",
  random = c("beta","u")
)
fit <- nlminb(
  obj$par,
  obj$fn,
  obj$gr
)

rep <- sdreport(obj)

summary(rep)

pred <- obj$report()$pred

plot(growth$EndCL, pred)
abline(0,1,col=2)


newdat <- expand.grid(
  Temperature = seq(
    min(growth$Temperature),
    max(growth$Temperature),
    length.out = 100
  ),
  StartCL = c(4, 6, 8, 10)
)

Xp <- PredictMat(sm, newdat)

eta <- beta0_hat + as.vector(Xp %*% beta_hat)

newdat$growth_rate <- exp(eta)

library(ggplot2)

ggplot(newdat,
       aes(Temperature,
           growth_rate,
           colour = factor(StartCL))) +
  geom_line(linewidth = 1.2) +
  theme_bw()

################################################
###

library(TMB)
setwd('C:/Users/cooka/Documents/tmp')

compile("ss_lvbGrowth.cpp")
dyn.load(dynlib("ss_lvbGrowth"))
growth$IndividualF <- factor(growth$Individual)

Data <- list(
  StartCL = growth$StartCL,
  EndCL = growth$EndCL,
  Temperature = growth$Temperature,
  Days = growth$Days,
  Individual = as.integer(growth$IndividualF) - 1L
)

Parameters <- list(
  log_kref = log(0.02),
  log_E  = log(0.65),
  log_Eh = log(2),
  log_Th = log(298.15),
  log_Linf = log(15),
  log_sigma_obs = log(0.1),
  log_sigma_ind = log(0.1),
    u = rep(0,nlevels(growth$IndividualF))
    )
obj <- MakeADFun(
  Data,
  Parameters,
  random = "u",
  DLL = "ss_lvbGrowth"
)

opt <- nlminb(
  obj$par,
  obj$fn,
  obj$gr,
  control = list(
    iter.max = 1000,
    eval.max = 1000
  )
)

rep <- sdreport(obj, getJointPrecision = TRUE)
Q <- rep$jointPrecision
V <- solve(as.matrix(Q))
nn = row.names(V)
ii <- grep("log_kref|log_E|log_Eh|log_Th", nn)
theta_hat <- obj$env$last.par.best[ii]
Vtheta <- V[ii, ii]

nsim <- 2000

theta_sim <- mvrnorm(
  nsim,
  mu = theta_hat,
  Sigma = Vtheta
)
T <- seq(5, 28, length.out = 200)
TK <- T + 273.15
Tref <- 293.15
kb <- 8.617e-5

K <- matrix(
  NA,
  nrow = nsim,
  ncol = length(T)
)

for(i in seq_len(nsim)) {
  
  log_kref <- theta_sim[i,1]
  log_E    <- theta_sim[i,2]
  log_Eh   <- theta_sim[i,3]
  log_Th   <- theta_sim[i,4]
  
  kref <- exp(log_kref)
  E    <- exp(log_E)
  Eh   <- exp(log_Eh)
  Th   <- exp(log_Th)
  
  K[i,] <-
    kref *
    exp(
      (E/kb) *
        (1/Tref - 1/TK)
    ) /
    (
      1 +
        exp(
          (Eh/kb) *
            (1/Th - 1/TK)
        )
    )
}

log_kref <- theta_hat[1]
log_E    <- theta_hat[2]
log_Eh   <- theta_hat[3]
log_Th   <- theta_hat[4]

kref <- exp(log_kref)
E    <- exp(log_E)
Eh   <- exp(log_Eh)
Th   <- exp(log_Th)

k_hat <-
  kref *
  exp(
    (E/kb) *
      (1/Tref - 1/TK)
  ) /
  (
    1 +
      exp(
        (Eh/kb) *
          (1/Th - 1/TK)
      )
  )


lower <- apply(K, 2, quantile, 0.025)
upper <- apply(K, 2, quantile, 0.975)

library(ggplot2)

dd <- data.frame(
  Temperature = T,
  Estimate = k_hat,
  Lower = lower,
  Upper = upper
)

ggplot(dd,
       aes(Temperature, Estimate)) +
  geom_ribbon(
    aes(ymin = Lower,
        ymax = Upper),
    alpha = 0.25
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  theme_bw() +
  ylab("Growth coefficient k(T)")
`


