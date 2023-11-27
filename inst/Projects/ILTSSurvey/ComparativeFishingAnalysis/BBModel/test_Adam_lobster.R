# #############################################
# Test the models on the lobster data from Adam
# #############################################

rm(list = ls())
setwd("C:/Users/cooka/Documents/git/bio.lobster/inst/Projects/ILTSSurvey/ComparativeFishingAnalysis/BBModel/")

library(dplyr)
library(tidyr)
library(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)
la()

v = ILTS_ITQ_All_Data(species=2550)
cs = read.csv(file.path(project.datadirectory('bio.lobster'),'data','survey','comparativeStations.csv'))
cs$ID = paste(cs$YEAR,cs$STATION)

v$ID = paste(v$YEAR,v$STATION)
v$rFL = round(v$FISH_LENGTH)
v = subset(v,ID %in% cs$ID)


#total lobster
va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=sum)
names(va)[5]='N'

vaw = pivot_wider(va,id_cols=c(STATION, YEAR,VESSEL_NAME),names_from=GEAR,values_from=N)

#by length
vaL = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+rFL,data=v,FUN=sum)
names(vaL)[6]='N'


vaLw = pivot_wider(vaL,id_cols=c(STATION, YEAR,VESSEL_NAME, rFL),names_from=GEAR,values_from=N)
names(vaLw)[5] = 'BALLOON'
data = subset(vaLw,select=c(STATION, rFL,BALLOON, NEST))

data.ready = data <- rename.df(data,c('STATION','rFL','BALLOON'),c('stn','len','BALL'))

# read data


# --------------------------------------------
# data description

# paired catch by station 
ggplot(data = data) +
  geom_line(aes(x = len, y = NEST), color = "blue") +
  geom_line(aes(x = len, y = BALL), color = "red") +
  facet_wrap(~stn) +
  theme_bw()
ggsave("lobster/paired_catch_length.jpg", width = 10, height = 10)


# paired catch by station: length spectrum
data %>%
  gather("vessel", "catch", - stn, -len) %>%
  ggplot() +
  geom_tile(aes(stn, len, fill = catch)) +
  scale_fill_continuous(low = "white", high = "red", trans = "log10", na.value = "white") +
  facet_wrap(~vessel, nrow = 2) +
  theme_bw()
ggsave("lobster/paired_catch_length_spectrum.jpg", width = 10, height = 10)

# paired catch averaged over stations
data %>% 
  gather("vessel", "catch", -len,-stn) %>%
  ggplot() +
  geom_line(aes(len, catch, group = stn), color = "gray") +
  stat_summary(aes(len, catch), fun.y=mean, geom="line", colour="black") +
  facet_wrap(~vessel, nrow = 1) +
  theme_bw()
ggsave("lobster/average_paired_catch_length.jpg", width = 10, height = 6)

# paired catch total
data %>% 
  # mutate(len = as.integer(floor(len/5))) %>%
  group_by(len) %>% 
  summarise(A = (sum(NEST)), B = (sum(BALL))) %>%
  gather("vessel", "total.catch", -len) %>%
  ggplot() +
  geom_line(aes(len, total.catch, color = vessel)) +
  theme_bw()
ggsave("lobster/total_paired_catch_length.jpg", width = 8, height = 6)



# ------------------------------------------------
# beta-binomial model
rm(list = ls())
library(gamlss)

# d <- data %>% 
#   # mutate(len = as.integer(floor(len/5))) %>%
#   group_by(len) %>% 
#   summarise(A = (sum(NEST)), B = (sum(BALL))) %>%
#   mutate(N = A+B,
#          r.B = A/B, 
#          p.B = B/N) %>%
#   filter(r.B!=Inf)
#         
# fit <- gamlss(
#   formula = r.B ~ cs(len),
#   sigma.formula = ~ len,
#   family = BE(mu.link = "log", sigma.link = "log"),
#   data = d)
# 
# summary(fit)
# 
# 
# plot(d$len, d$r.B)
# lines(d$len, fit$mu.fv)


d <- data %>% 
    filter(len > 0 & !is.na(BALL)) %>%
    mutate(len = 0.5 + (floor(len/5))*5) %>% # length grouping
    group_by(len) %>% 
    summarise(A = (sum(NEST)), B = (sum(BALL))) %>%
    mutate(N = A+B) %>%
    filter(!(A==0&B==0))


d.orig <- data %>%
  transmute(station = stn,len, A = NEST, B = BALL) %>% 
  gather(gear, catch, -station, -len) %>%
  filter(catch > 0) %>%
  group_by(station, gear, len) %>%
  summarise(catch = sum(catch)) %>%
  ungroup() %>%
  spread(gear, catch, fill = 0) %>%
  mutate(N = A+B)


# overdispersion (of a beta distribution) only exists when trials > 1
# for lobster, best to use sigma.formula: ~cs(len)
fit <- gamlss(
  formula = cbind(A, B) ~ cs(len),
  sigma.formula = ~ cs(len),
  family = BB(mu.link = "logit", sigma.link = "log"),
  data = d)

summary(fit)

# plot of estimates
plot(fit)
fittedPlot(fit, x = d$len)
dev.off(); wp(fit, ylim.all = 2)

# Proportion of A: computation of CI is based on estimated BB dist
jpeg(paste0("lobster/mu-gamlss-blen5-cs-cs.jpg"),
     res = 300, width = 8, height = 6, units = "in")
plot(d$len, d$A/d$N, ylim = c(0, 1))
# points(d.orig$len, d.orig$A/d.orig$N, pch = 20, cex = 0.1)
lines(d$len, fit$mu.fv)
lines(fit$mu.x[,2], qBB(0.05, mu = fit$mu.fv, sigma = fit$sigma.fv, bd = fit$bd, fast = T)/fit$bd, col = "blue", lty = "dashed")
lines(fit$mu.x[,2], qBB(0.95, mu = fit$mu.fv, sigma = fit$sigma.fv, bd = fit$bd, fast = T)/fit$bd, col = "blue", lty = "dashed")
# lines(fit$mu.x[,2], qBB(0.5, mu = fit$mu.fv, sigma = fit$sigma.fv, bd = fit$bd, fast = T)/fit$bd, col = "blue")
# lines(fit$mu.x[,2], qBB(0.05, mu = fit$mu.fv, sigma = fit$sigma.fv, bd = 1000)/1000, col = "blue", lty = "dashed")
# lines(fit$mu.x[,2], qBB(0.95, mu = fit$mu.fv, sigma = fit$sigma.fv, bd = 1000)/1000, col = "blue", lty = "dashed")
dev.off()


# conversion factor: rho(NEST/BALL)
jpeg(paste0("lobster/rho-gamlss-blen5-cs-cs.jpg"),
     res = 300, width = 8, height = 6, units = "in")
plot(d$len, d$A/d$B, ylab = "rho: Gear 9 / Gear 15")
lines(d$len, fit$mu.fv/(1-fit$mu.fv))
abline(a = 1, b = 0)
# CI approximated by lognormal given variance of logit(P)
lines(d$len, fit$mu.fv/(1-fit$mu.fv)-1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv)), col = "blue", lty = "dashed")
lines(d$len, fit$mu.fv/(1-fit$mu.fv)+1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv)), col = "blue", lty = "dashed")
dev.off()





# ------------------------------------------------
# beta-binomial model with random effects
rm(list = ls())

i.species <- "LOBSTER"
b.len <- 5

# read data
d <-data.ready%>%
#  filter(len>45 & len<160) %>%
  transmute(station = stn, len = len, NEST = NEST, BALL = BALL) %>% 
  gather(gear, catch, -station, -len) %>%
  filter(catch > 0) %>%
  transmute(
    station = factor(station),
    gear = factor(gear), 
    len = (floor(len/b.len)*b.len+b.len/2), # length grouping
    catch = catch/10) %>%
  group_by(station, gear, len) %>%
  summarise(catch = sum(catch)) %>%
  ungroup() %>%
  complete(len,
           station, 
           gear, 
           fill = list(catch = 0)) # complete zero observations
  

# length at center of each bin, vector for plotting
lenseq <- unique(d$len)

# data for offset: same within a tow
# use zeros as this data is tow-standardized
d.offset <- d %>% 
  distinct(station) %>%
  mutate(offset = 0)

# basis and penalty matrices for cubic spline
# over length bins
# location of knots might influence smooth functions (and convergence)
# default to 10 knots resulting in 2 fixed and 8 random effects
library(mgcv)
cs <- smooth.construct(
  object = s(len, bs = "cr"),
  data = d %>% group_by(len) %>% summarise(catch = sum()),
  knots = NULL
)

# cs <- smooth.construct(
#   object = s(len, bs = "cr", k = 30),
#   data = d %>% group_by(len) %>% summarise(catch = sum()),
#   knots =  list(len = seq(min(lenseq),max(lenseq),length.out = 30)))

n_f <- 2
n_r <- cs$df - n_f
eigende <- eigen(cs$S[[1]])

# input for TMB
nlen = length(lenseq)
nstation = nlevels(d$station)

data = list(
  A = d %>% filter(gear == "NEST") %>% spread(len, catch) %>% dplyr::select(-station, -gear) %>% as.matrix(),
  B = d %>% filter(gear == "BALL") %>% spread(len, catch) %>% dplyr::select(-station, -gear) %>% as.matrix(),
  offset = outer(d.offset$offset,rep(1,length(lenseq))),
  Xf = cs$X %*% eigende$vectors[,1:n_f+n_r],
  Xr = cs$X %*% eigende$vectors[,1:n_r],
  d = eigende$value[1:n_r]
)
parameters = list(
  beta = rep(0, n_f),
  b = rep(0, n_r),
  gamma = rep(0, n_f),
  g = rep(0, n_r),
  delta = matrix(0, nstation, n_f),
  epsilon = matrix(0, nstation, n_r),
  log_s_b = log(10),
  log_s_g = log(10),
  log_s_epsilon = log(10),
  chol_delta = c(1,0,1) # use chol decomp in vector form
)
map <- list(
  # log_s_b = factor(NA),
  # log_s_g = factor(NA),
  # log_s_epsilon = factor(NA)
  # chol_delta = rep(factor(NA), length(parameters$chol_delta))
)

# run TMB model
library(TMB)
version <- "betabinom_re"
compile(paste0(version,".cpp")) #should return 0
dyn.load(dynlib(version))
obj = MakeADFun(data=data,
                parameters=parameters,
                map = map,
                DLL=version,
                random = c("b", "g", "delta", "epsilon"),
                silent = F)
opt <- nlminb(obj$par,obj$fn,obj$gr)
rep <- sdreport(obj)

obj$gr()
opt

# library(TMBhelper)
# fit <- fit_tmb(obj)
# opt <- fit$opt
# rep <- sdreport(obj)


# plot results

# estimate and std of mu and phi and rho
est <- summary(rep, "report")[,"Estimate"]
std <-  summary(rep, "report")[,"Std. Error"]
est.mean_mu <- est[names(est) == "mean_mu"]
std.mean_mu <- std[names(std) == "mean_mu"]
est.mean_phi <- est[names(est) == "mean_phi"]
std.mean_phi <- std[names(std) == "mean_phi"]
est.mean_rho <- est[names(est) == "mean_rho"]
std.mean_rho <- std[names(std) == "mean_rho"]

est.mean_log_rho <- est[names(est) == "mean_log_rho"]
std.mean_rho <- std[names(std) == "mean_rho"]


est.mu <- matrix(est[names(est) == "mu"], nrow = nstation, ncol = nlen)
std.mu <- matrix(std[names(std) == "mu"], nrow = nstation, ncol = nlen)
est.rho <- matrix(est[names(est) == "rho"], nrow = nstation, ncol = nlen)
std.rho <- matrix(std[names(std) == "rho"], nrow = nstation, ncol = nlen)

est.mean_rho = apply(est.rho,2,function(x) quantile(x,.5))

dd = pivot_wider(d,id_cols=c(len,station),names_from = gear,values_from = catch)
plot(dd$len, dd$NEST/dd$BALL, ylab = "")
lines(lenseq,apply(est.rho,2,function(x) quantile(x,.5)),col='red')

# estimated mu, phi and rho for each station
jpeg(paste0("lobster/estimates-CI95_zscore-species_", i.species, ".jpg"),
     res = 300, width = 6, height = 10, units = "in")
par(mfrow=c(3,1))
plot(lenseq, est.mean_mu, ylim = c(0,1), type = "l")
for(i in 1:nstation){lines(lenseq, obj$report()$mu[i,], col = "gray")}
lines(lenseq, est.mean_mu + 1.96*std.mean_mu, col = "blue", lty = "dashed")
lines(lenseq, est.mean_mu - 1.96*std.mean_mu, col = "blue", lty = "dashed")
plot(lenseq, est.mean_phi, type = "l")
plot(lenseq, est.mean_rho, ylim = c(0,10), type = "l")
abline(a = 1, b = 0, col = "red")
for(i in 1:nstation){lines(lenseq, obj$report()$rho[i,], col = "gray")}
lines(lenseq, est.mean_rho + std.mean_rho, col = "blue", lty = "dashed")
lines(lenseq, est.mean_rho - std.mean_rho, col = "blue", lty = "dashed")
dev.off()


# test rho: (1) mean_rho = f(mean(mu)) (2) mean_rho = mean(f(mu)) 
est.mean_rho_2 <- est[names(est) == "mean_rho_2"]
std.mean_rho_2 <- std[names(std) == "mean_rho_2"]
plot(lenseq, est.mean_rho, ylim = c(0,5), type = "l")
lines(lenseq, est.mean_rho_2, col = "blue")



# estimated mu with observations for each station
# SD is from TMB sd
jpeg(paste0("lobster/mu_by_station-TMB_SD-species_", i.species, ".jpg"),
     res = 300, width = 16, height = 12, units = "in")
par(mfrow=c(5,6))
for(i in 1:nstation){
  plot(lenseq, data$A[i,]/(data$A[i,]+data$B[i,]), ylim = c(0, 1), ylab = "Prop. of Gear 9", main = paste0("Station ", levels(d$station)[i]))
  lines(lenseq, est.mu[i,])
  lines(lenseq, est.mu[i,] - std.mu[i,], lty = "dashed")
  lines(lenseq, est.mu[i,] + std.mu[i,], lty = "dashed")
}
dev.off()


# estimated rho with observations for each station: 
# calculation of CI: what to do
jpeg(paste0("lobster/rho_by_station-TMB_SD-species_", i.species, ".jpg"),
     res = 300, width = 16, height = 12, units = "in")
par(mfrow=c(5,6))
for(i in 1:nstation){
  plot(lenseq, data$A[i,]/data$B[i,], ylim = range(0, 10), ylab = "Conversion: 9/15", main = paste0("Station ", levels(d$station)[i]))
  lines(lenseq, est.rho[i,])
  abline(a = 1, b = 0, col = "red")
  lines(lenseq, est.rho[i,] - std.rho[i,], lty = "dashed")
  lines(lenseq, est.rho[i,] + std.rho[i,], lty = "dashed")
}
dev.off()



