library(tidyverse)

set.seed(123)

#=========================================================
# Simulation function
#=========================================================

run_sim <- function(
    USR,
    LRP = 0.4,
    USR_rebuild = 0.9,
    nyears = 100,
    mgmt_lag = 1,
    Ftarget = 0.30,
    nsim_dev_sd = 0.03
){
  
  yrs <- 1:nyears
  
  #-------------------------------------------------------
  # Biomass story scales with cautious-zone width
  #-------------------------------------------------------
  
  width <- USR - LRP
  
  Bmin <- LRP + 0.15 * width
  
  knots <- tibble(
    Year = c(
      1,
      15,
      30,
      45,
      60,
      80,
      100
    ),
    Brel = c(
      1.0,
      0.98,
      USR,
      Bmin,
      (USR + Bmin)/2,
      USR_rebuild,
      1.0
    )
  )
  
  Btrend <- spline(
    x = knots$Year,
    y = knots$Brel,
    xout = yrs
  )$y
  
  #-------------------------------------------------------
  # stochastic biomass deviations
  #-------------------------------------------------------
  
  dev <- numeric(nyears)
  
  for(t in 2:nyears){
    dev[t] <-
      0.85 * dev[t-1] +
      rnorm(1,0,nsim_dev_sd)
  }
  
  Brel <- Btrend + dev
  
  #-------------------------------------------------------
  # HCR with hysteresis
  #-------------------------------------------------------
  
  F <- numeric(nyears)
  
  mode <- "healthy"
  
  for(t in yrs){
    
    lag_t <- max(1,t-mgmt_lag)
    
    b <- Brel[lag_t]
    
    if(mode == "healthy" &&
       b < USR){
      
      mode <- "rebuilding"
      
    }
    
    if(mode == "rebuilding" &&
       b > USR_rebuild){
      
      mode <- "healthy"
    }
    
    if(mode == "healthy"){
      
      F[t] <- Ftarget
      
    } else {
      
      status <-
        pmax(
          0,
          pmin(
            1,
            (b-LRP)/(USR-LRP)
          )
        )
      
      F[t] <- 0.02 + 0.08 * status
    }
  }
  
  any(Brel < LRP)
}

#=========================================================
# USR scenarios
#=========================================================

LRP <- 0.4

USR_vals <- seq(
  0.50,
  0.90,
  by = 0.05
)

#=========================================================
# Monte Carlo experiment
#=========================================================

results <-
  map_dfr(
    USR_vals,
    function(USR){
      
      below_lrp <-
        replicate(
          1000,
          run_sim(
            USR = USR,
            LRP = LRP
          )
        )
      
      tibble(
        USR = USR,
        Width = USR - LRP,
        ProbBelowLRP =
          mean(below_lrp)
      )
    }
  )

#=========================================================
# Plot
#=========================================================

ggplot(
  results,
  aes(
    Width,
    ProbBelowLRP
  )
) +
  
  geom_line(
    linewidth = 1.2,
    colour = "steelblue"
  ) +
  
  geom_point(
    size = 3,
    colour = "steelblue"
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  labs(
    x = "USR − LRP",
    y = "Probability of Crossing LRP",
    title =
      "Risk of Falling Below the LRP vs Width of the Cautious Zone"
  ) +
  
  theme_bw()