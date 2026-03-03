#combining egg bearing and pleopod staging

#pleo
da = read.csv(file.path(project.datadirectory('bio.lobster'), 'data','maturity','maturity_all.csv'))
da = subset(da,select=c(LFA,Carapace_mm,CG_stage,year))
names(da)=c('LFA','CL',"Y",'year')

#atSea
require(bio.lobster)
require(dplyr)

require(ggplot2)
require(devtools)
require(bio.utilities)
setwd('C:/Users/cooka/OneDrive - DFO-MPO/Projects/HCR_CSAS/LobsterSamplingData')

lobster.db('atSea')
a = subset(atSea,SPECIESCODE==2550 & SEX%in%c(2,3) & lubridate::year(STARTDATE)>2016,select=c(LFA,CARLENGTH,SEX))
a$SEX=ifelse(a$SEX==3,1,0)
names(a) = c('LFA','CL','B')


pleo = subset(da,LFA=='L34' & year>2020 &!is.na(CL) & !is.na(Y),select=c(CL,Y))
berr = subset(a,LFA==34&!is.na(CL) & !is.na(B),select=c(CL,B))


# Packages
library(bbmle)
ilogit <- function(eta) 1/(1+exp(-eta))


#### if we have some idea of Se and Sp---- #Se or sensitivity is the essentially-- if a lobster is truely mature how often does it pleopod staging say its immature (high sensitity= few false negatives)
										   #Sp of specificity is essentiall-- if a lobster is truely immature how often does it get classified as immature (high specificy= few false positives)												

# Se_fix <- 0.90
# Sp_fix <- 0.95

# nll_fix_SeSp <- function(a0, a1, g0) {
#   Se  <- Se_fix
#   Sp  <- Sp_fix
#   psi <- ilogit(g0)

#   p_pleo <- ilogit(a0 + a1 * pleo$CL)
#   p_berr <- ilogit(a0 + a1 * berr$CL)

#   q  <- Se * p_pleo + (1 - Sp) * (1 - p_pleo)

#   ll_pleo <- dbinom(pleo$Y, 1, q, log=TRUE)

#   mu <- p_berr * psi
#   ll_berr <- dbinom(berr$B, 1, mu, log=TRUE)

#   -(sum(ll_pleo) + sum(ll_berr))
# }

# # Good starting values: seed from a simple pleopod-only logistic
# glm_seed <- glm(Y ~ CL, data = pleo, family = binomial())
# st <- list(a0 = coef(glm_seed)[1], a1 = coef(glm_seed)[2], g0 = qlogis(mean(berr$B)))

# fit_fix <- mle2(nll_fix_SeSp, start = st, method = "Nelder-Mead")
# summary(fit_fix)


#if we know either Sp or Se set one and estimate the other (cant estimate both due to identifiability issues)
Sp_fix <- 0.98

nll_est_Se <- function(a0, a1, se_logit, g0) {
  Se  <- ilogit(se_logit)
  Sp  <- Sp_fix
  psi <- ilogit(g0)

  p_pleo <- ilogit(a0 + a1 * pleo$CL)
  p_berr <- ilogit(a0 + a1 * berr$CL)

  q  <- Se * p_pleo + (1 - Sp) * (1 - p_pleo)
  ll_pleo <- dbinom(pleo$Y, 1, q, log=TRUE)

  mu <- p_berr * psi
  ll_berr <- dbinom(berr$B, 1, mu, log=TRUE)

  -(sum(ll_pleo) + sum(ll_berr))
}

st <- list(a0 = coef(glm_seed)[1], a1 = coef(glm_seed)[2], se_logit = qlogis(0.9), g0 = qlogis(0.5))
fit_estSe <- mle2(nll_est_Se, start = st, method = "Nelder-Mead")
summary(fit_estSe)


########################################
#outputs

# Choose your fitted object, e.g., fit_fix or fit_estSe or fit_both
fit <- fit_fix  # example

ilogit <- function(eta) 1/(1+exp(-eta))

fit <- fit_fix  # your fitted mle2 object

co    <- coef(fit)
alpha <- co[["a0"]]
beta  <- co[["a1"]]
psi   <- ilogit(co[["g0"]])

# Correct L50/L95 for logit(p) = alpha + beta * CL
L50 <- -alpha / beta
L95 <- (log(19) - alpha) / beta

# Delta-method CIs
vc <- vcov(fit)

# L50 = -alpha/beta
dL50_da <- -1 / beta
dL50_db <-  alpha / (beta^2)

var_L50 <- dL50_da^2 * vc["a0","a0"] +
           dL50_db^2 * vc["a1","a1"] +
           2 * dL50_da * dL50_db * vc["a0","a1"]

se_L50  <- sqrt(var_L50)
CI_L50  <- L50 + c(-1.96, 1.96) * se_L50

# L95 = (log(19) - alpha)/beta
dL95_da <- -1 / beta
dL95_db <- -(log(19) - alpha) / (beta^2)   # NOTE the minus sign here

var_L95 <- dL95_da^2 * vc["a0","a0"] +
           dL95_db^2 * vc["a1","a1"] +
           2 * dL95_da * dL95_db * vc["a0","a1"]

se_L95  <- sqrt(var_L95)
CI_L95  <- L95 + c(-1.96, 1.96) * se_L95

list(
  L50 = c(est = L50, LCL = CI_L50[1], UCL = CI_L50[2]),
  L95 = c(est = L95, LCL = CI_L95[1], UCL = CI_L95[2]),
  psi = psi
)
library(dplyr); library(ggplot2)

newCL <- data.frame(CL = seq(min(c(pleo$CL, berr$CL), na.rm=TRUE),
                             max(c(pleo$CL, berr$CL), na.rm=TRUE), length.out=200))
p_mat <- ilogit(alpha + beta*newCL$CL)
p_ber <- p_mat * psi

plot_df <- newCL |>
  mutate(`Mature (ogive)` = p_mat, `Berried (expected)` = p_ber) |>
  tidyr::pivot_longer(-CL, names_to="curve", values_to="p")

ggplot(plot_df, aes(CL, p, colour=curve)) +
  geom_line(linewidth=1.2) +
  scale_colour_manual(values=c("black","steelblue")) +
  labs(x="Carapace length", y="Probability", colour=NULL) +
  theme_minimal(base_size=13)
