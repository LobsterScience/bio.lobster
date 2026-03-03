
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

pleo$B = NA
berr$Y=NA
lob=rbind(berr,pleo)

library(dplyr)
library(tidyr)

# lob has columns:
#   CL  (numeric carapace length)
#   Y   (pleopod staging: 1=mature, 0=immature, NA if not staged)
#   B   (berried: 1=yes, 0=no, NA if not checked)

lob_long <- lob %>%
  pivot_longer(c(Y, B), names_to = "type", values_to = "obs") %>%
  filter(!is.na(obs)) %>%
  mutate(
    # 1 = pleopod row, 2 = berried row
    type_int = if_else(type == "Y", 1L, 2L),
    YB       = as.integer(obs)
  ) %>%
  select(CL, YB, type_int)



# --- Custom family: mu must be first and have a link; dpars use logit link
library(brms)

joint_fam <- custom_family(
  "joint_maturity",
  dpars = c("mu", "Se", "Sp", "psi"),
  links = c("logit", "logit", "logit", "logit"),
  type  = "int",
  vars  = "vint1"   # <- important: tells brms there is one integer addition
)

stan_funs <- "
real joint_maturity_lpmf(int y,
                         real mu, real Se, real Sp, real psi,
                         int vint1) {
  real mu_c = fmin(fmax(mu, 1e-12), 1 - 1e-12);
  real ll = 0;
  if (vint1 == 1) {
    real q = Se * mu_c + (1 - Sp) * (1 - mu_c);
    q = fmin(fmax(q, 1e-12), 1 - 1e-12);
    ll += bernoulli_lpmf(y | q);
  } else if (vint1 == 2) {
    real r = mu_c * psi;
    r = fmin(fmax(r, 1e-12), 1 - 1e-12);
    ll += bernoulli_lpmf(y | r);
  }
  return ll;
}

// vectorized overload: y[], mu, Se, Sp, psi as vectors; vint1[]
real joint_maturity_lpmf(int[] y,
                         vector mu, vector Se, vector Sp, vector psi,
                         int[] vint1) {
  int N = num_elements(y);
  real lp = 0;
  for (n in 1:N) {
    lp += joint_maturity_lpmf(y[n] | mu[n], Se[n], Sp[n], psi[n], vint1[n]);
  }
  return lp;
}

// HYBRID 1: scalar y, but vint1 passed as int[] (take first element)
real joint_maturity_lpmf(int y,
                         real mu, real Se, real Sp, real psi,
                         int[] vint1) {
  return joint_maturity_lpmf(y | mu, Se, Sp, psi, vint1[1]);
}

// HYBRID 2: vector y[], but vint1 passed as scalar (reuse same flag)
real joint_maturity_lpmf(int[] y,
                         vector mu, vector Se, vector Sp, vector psi,
                         int vint1) {
  int N = num_elements(y);
  real lp = 0;
  for (n in 1:N) {
    lp += joint_maturity_lpmf(y[n] | mu[n], Se[n], Sp[n], psi[n], vint1);
  }
  return lp;
}
"
stanvars <- stanvar(scode = stan_funs, block = "functions")


# 1) Model formula:
#    - mu (maturity) uses a logistic ogive: mu ~ 1 + CL
#    - Se, Sp, psi are intercept-only on the logit scale
bf_joint <- bf(
  YB | vint(type_int) ~ 1 + CL,  # <-- mu model
  Se  ~ 1,
  Sp  ~ 1,
  psi ~ 1
)

# 2) Priors:

# Compute target means on the logit scale in R
logit_Se  <- qlogis(0.90)  # ~ 2.1972246
logit_Sp  <- qlogis(0.98)  # ~ 3.8918203
logit_psi <- qlogis(0.50)  # 0

# Build character priors with numeric constants embedded
priors <- c(
  set_prior("normal(0, 5)",               class = "Intercept"),         # alpha (mu)
  set_prior("normal(0, 5)",               class = "b", coef = "CL"),    # beta  (mu)

  set_prior(sprintf("normal(%f, 1)",   logit_Se),  class = "Intercept", dpar = "Se"),
  set_prior(sprintf("normal(%f, 1)",   logit_Sp),  class = "Intercept", dpar = "Sp"),
  set_prior(sprintf("normal(%f, 1.5)", logit_psi), class = "Intercept", dpar = "psi")
)



fit_brm <- brm(
  bf_joint,
  data     = lob_long,
  family   = joint_fam,
  stanvars = stanvars,
  prior    = priors,
  chains   = 4, cores = 4, iter = 2000,  # start modestly; scale up later
  control  = list(adapt_delta = 0.95)
  # , backend = "cmdstanr"  # <- recommend for speed with your 2.7M rows
)

summary(fit_brm)

post <- as_draws_df(fit_brm)

alpha <- post$b_Intercept
beta  <- post$b_CL

L50 <- -alpha / beta
L95 <- (log(19) - alpha) / beta

quantile(L50, c(.025,.5,.975))
quantile(L95, c(.025,.5,.975))

# Posterior means for a simple plot
alpha_hat <- mean(alpha)
beta_hat  <- mean(beta)
psi_hat   <- plogis(mean(post$b_psi_Intercept))  # logit link -> inverse-logit

newCL <- seq(min(lob_long$CL), max(lob_long$CL), length.out = 200)
p_mat <- plogis(alpha_hat + beta_hat * newCL)
p_ber <- p_mat * psi_hat

library(ggplot2)
ggplot(data.frame(CL = newCL, Mature = p_mat, Berried = p_ber)) +
  geom_line(aes(CL, Mature)) +
  geom_line(aes(CL, Berried), linetype = 2, colour = "steelblue") +
  labs(x = "Carapace length", y = "Probability",
       title = "Joint maturity (mu) + pleopod (Se/Sp) + berried (psi)") +
  theme_minimal()