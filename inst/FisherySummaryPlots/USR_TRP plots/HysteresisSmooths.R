library(tidyverse)
library(patchwork)

set.seed(123)

#=========================================================
# PARAMETERS
#=========================================================

nyears <- 100
yrs <- 1:nyears

USR_rebuild <- 0.90
USR <- 0.80
LRP <- 0.20
depletion_frac <- 0.15
Bmin <- LRP + depletion_frac * (USR - LRP)
Frr = Ftarget <- 0.30
mgmt_lag <- 5

#=========================================================
# BIOMASS STORY
#=========================================================
knots <- tibble(
  Year = c(1, 15, 30, 45, 60, 80, 100),
  Brel = c(
    1,
    0.98,
    USR,
    Bmin,
    (USR + Bmin)/2,
    USR_rebuild,
    1
  )
)

#=========================================================
# SMOOTH TRAJECTORY
#=========================================================

Btrend <- spline(
  x = knots$Year,
  y = knots$Brel,
  xout = yrs
)$y

#=========================================================
# AR1 DEVIATIONS
#=========================================================

rho <- 0.85
sigma <- 0.04

dev <- numeric(nyears)

for(i in 2:nyears){
  dev[i] <- rho * dev[i-1] +
    rnorm(1,0,sigma)
}

Brel <- Btrend + dev

Brel <- pmax(
  0.05,
  pmin(1.1,Brel)
)

#=========================================================
# HYSTERESIS MANAGEMENT
#=========================================================
F_desired <- numeric(nyears)
F <- numeric(nyears)

F[1] <- .25

for(t in 2:nyears){
  
  lag_t <- max(1, t - mgmt_lag)
  
  b <- Brel[lag_t]
  
  if(b >= USR){
    
    F_desired[t] <-
      F[1] +
       (Ftarget - F[1]) *
        (1 - exp(-t/2))
    F_desired[t] <-
    F_desired[t] +
    rnorm(1, 0, 0.015)

    F_desired[t] <-
    pmin(1.2 * Ftarget,
         pmax(0, F_desired[t]))
    alpha <- 0.30
    
  } else if(b <= LRP){
    
    F_desired[t] <- 0.01
    
  } else {
    
    status <-
      (b - LRP)/(USR - LRP)
    
    F_desired[t] <-
      Ftarget * status
  }
  if(F_desired[t] < F[t-1]){
    
    alpha <- 0.60
    
  } else {
    
    alpha <- 0.05
  }
  
  F[t] <-
    F[t-1] +
    alpha *
    (F_desired[t] - F[t-1])
}
  #=========================================================
# IMPLEMENTATION ERROR
#=========================================================

f_dev <- numeric(nyears)

for(i in 2:nyears){
  
  f_dev[i] <-
    0.7 * f_dev[i-1] +
    rnorm(1,0,0.02)
}

Fobs <-
  pmax(
    0,
    pmin(
      Ftarget*1.05,
      F * exp(f_dev)
    )
  )

#=========================================================
# RESULTS
#=========================================================

dat <- tibble(
  Year = yrs,
  Biomass = Brel,
  F = Fobs
)

#=========================================================
# HCR CURVE
#=========================================================

hcr <- tibble(
  Biomass = seq(0,1.05,length.out=500)
) %>%
  mutate(
    F =
      case_when(
        Biomass <= LRP ~ 0,
        Biomass >= USR ~ Ftarget,
        TRUE ~
          Ftarget *
          (Biomass-LRP)/(USR-LRP)
      )
  )

#=========================================================
# BIOMASS
#=========================================================

p1 <- ggplot(dat,
             aes(Year,Biomass)) +
  geom_line(
    colour="steelblue",
    linewidth=1
  ) +
  geom_hline(
    yintercept=USR,
    colour="orange",
    linetype=2
  ) +
  geom_hline(
    yintercept=LRP,
    colour="red",
    linetype=2
  ) +
  theme_bw() +
  labs(
    title="Biomass Trajectory",
    y="B / K"
  )

#=========================================================
# F
#=========================================================

p2 <- ggplot(dat,
             aes(Year,F)) +
  geom_line(
    colour="darkgreen",
    linewidth=1
  ) +
  theme_bw() +
  labs(
    title="Fishing Mortality",
    y="F"
  )

#=========================================================
# B-F HYSTERESIS PLOT
#=========================================================

p3 <- ggplot() +
  
  geom_line(
    data=hcr,
    aes(Biomass,F),
    colour="grey50",
    linewidth=1.5
  ) +
  
  geom_path(
    data=dat,
    aes(Biomass,F),
    colour="blue",
    linewidth=1
  ) +
  
  geom_point(
    data=dat,
    aes(Biomass,F,
        colour=Year),
    size=2
  ) +
  
  geom_vline(
    xintercept=USR,
    colour="orange",
    linetype=2
  ) +
  
  geom_vline(
    xintercept=LRP,
    colour="red",
    linetype=2
  ) +
  
  theme_bw() +
  theme(legend.position = 'none')+
  
  labs(
   # title="B-F Hysteresis Plot",
    x="Scaled Biomass",
    y="Fishing Mortality"
  )


#=========================================================
# DISPLAY
#=========================================================
(p1 / p2) | (p3 )


pp = ggplot() +
  annotate("rect", xmin = 0, xmax = LRP,
           ymin = 0, ymax = 1.05,
           fill = "#DD9271") +
  annotate("rect", xmin = LRP, xmax = USR,
           ymin = 0, ymax = 1.05,
           fill = "#DDD27F") +
  annotate("rect", xmin = USR, xmax = 1.14,
           ymin = 0, ymax = 1.05,
           fill = "#66EE00") +
  
  geom_vline(xintercept = c(LRP, USR), linetype = "dotted") +
  
  geom_segment(aes(x = LRP, y = 0,
                   xend = USR, yend = Frr),
               linetype = "dashed", linewidth = 0.8) +
  
  geom_segment(aes(x = USR, y = Frr,
                   xend = 1, yend = Frr),
               linewidth = 0.8) +
  
  annotate("text", x = LRP - 1.5, y = 0.63,
           label = "LRP", angle = 90) +
  annotate("text", x = USR - 1.5, y = 0.63,
           label = "USR", angle = 90) +
  annotate("text", x = 75, y = Frr + 0.05,
           label = "RR", size = 6) +
  
  coord_cartesian(xlim = c(0, 1.14), ylim = c(0, Frr+0.05)) +
  labs(x = "Biomass", y = "Fishing Mortality") +
  theme_classic(base_size=14) +
  theme(
    panel.background = element_rect(fill = "grey90", colour = NA),
    plot.background = element_rect(fill = "grey90", colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

pp
+geom_path(
  data=dat,
  aes(Biomass,F),
  colour="blue",
  linewidth=1
) +
  
  geom_point(
    data=dat,
    aes(Biomass,F,
        colour=Year),
    size=2
  )
  