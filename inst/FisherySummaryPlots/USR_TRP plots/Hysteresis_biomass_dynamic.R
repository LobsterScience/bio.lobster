  library(tidyverse)
  
  nyears <- 50
  
  K <- 1000
  r <- 0.25
  
  USR <- 0.4
  LRP <- 0.20
  zone_width <- USR - LRP
  
  USR_rebuild <- USR + 0.5 * zone_width
  
  Ftarget <- 0.3
  mgmt_lag <- 5
  
  B <- F <- F_desired <- numeric(nyears)
  
  B[1] <- K*0.9
  F[1] <- 0.18
  
  r_eff <- numeric(nyears)
  r_eff[1] <- r
  
  rebuilding <- FALSE
  
  for(t in 2:nyears){
    
    lag_t <- max(1, t - mgmt_lag)
    
    b_rel <- B[lag_t]/K
    
    #=================================================
    # Management state
    #=================================================
    
    if(!rebuilding && b_rel < USR)
      rebuilding <- TRUE
    
    if(rebuilding && b_rel > USR_rebuild)
      rebuilding <- FALSE
    
    #=================================================
    # Desired F
    #=================================================
    
    if(!rebuilding){
      
      # Fishery develops from 0.18 to ~0.30 within ~5 yrs
      
      #ramping F to target over the first 6 YRS
      F_desired[t] <-
        F[1] +
        (Ftarget - F[1]) *
        plogis((t - 2)/2)
      
      F_desired[t] <-
        F_desired[t] +
        rnorm(1, 0, 0.02)
      
      F_desired[t] <-
        pmin(
          1.20 * Ftarget,
          pmax(0, F_desired[t])
        )
      
    } else {
      
      # Much more conservative rebuilding branch
      
      if(b_rel <= LRP){
        
        F_desired[t] <- 0.05
        
      } else {
        
        status <- (b_rel - LRP)/(USR - LRP)
        
        F_desired[t] <- Ftarget * status^2
        
      }
    }
    
    #=================================================
    # Management inertia
    #=================================================
    if(t<10){
      alpha = 1
    } else if (F_desired[t] < F[t-1]){
      
      alpha <- 0.60
      
    } else {
      
      alpha <- 0.1
    }
    
    F[t] <-
      F[t-1] +
      alpha *
      (F_desired[t] - F[t-1])
    
    #=================================================
    # Productivity hysteresis
    #=================================================
    
    if(rebuilding){
      
      # productivity recovers slowly
      
      r_eff[t] <-
        r_eff[t-1] +
        (0.3 - r_eff[t-1]) / 12
      
    } else {
      
      # gradually recovers toward full productivity
      
      r_eff[t] <-
        r_eff[t-1] +
        (r - r_eff[t-1]) / 8
    }
    
    #=================================================
    # Biomass dynamics
    #=================================================
    
    growth <-
      r_eff[t] *
      B[t-1] *
      (1 - B[t-1]/K)
    
    catch <-
      (1-exp(-F[t])) *
      B[t-1]
    
    process_error <-
      rnorm(1,0,0.015*K)
    
    B[t] <-
      B[t-1] +
      growth -
      catch +
      process_error
    
    B[t] <- max(0.05*K,B[t])
  }
  
  dat <- tibble(
    Year = 1:nyears,
    B = B,
    Brel = B/K,
    F = F,
    r_eff = r_eff)
  
  pp+geom_path(data=dat,aes(x=Brel,y=F))
  
  
  
  
  
  
  
  
  
  
  #############################base plot
  
  pp = ggplot() +
    annotate("rect", xmin = 0, xmax = LRP,
             ymin = 0, ymax = 1.05,
             fill = "#DD9271") +
   # annotate("rect", xmin = LRP, xmax = USR,
  #           ymin = 0, ymax = 1.05,
  #           fill = "#DDD27F") +
    annotate("rect", xmin =LRP, xmax = 1.14,
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
  
  
  
  n <- 500
  cols <- colorRampPalette(c("#DDD27F", "#66EE00"))(n)
  grad <- matrix(cols, nrow = 1)
  
  pg = ggplot() +
    annotate("rect", xmin = 0, xmax = LRP,
             ymin = 0, ymax = 1.05,
             fill = "#DD9271") +
    annotation_raster(
       raster = as.raster(grad),
       xmin = LRP, xmax = 1.14,
       ymin = 0, ymax = 1.05
       )+
    geom_vline(xintercept = c(LRP), linetype = "dotted") +
    annotate("text", x = LRP - 0.05, y = 0.13,
             label = "LRP", angle = 90)  +
    coord_cartesian(xlim = c(0, 1.14), ylim = c(0, Frr+0.05)) +
    labs(x = "Biomass", y = "Fishing Mortality") +
    theme_classic(base_size=14) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "grey90", colour = NA),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  pw = ggplot() +
    annotate("rect", xmin = 0, xmax = LRP,
             ymin = 0, ymax = 1.05,
             fill = "#DD9271") +
    annotate("rect", xmin = LRP, xmax = 1.14,
             ymin = 0, ymax = 1.05,
             fill = "white") +
    geom_vline(xintercept = c(LRP), linetype = "dotted") +
    annotate("text", x = LRP - 0.05, y = 0.13,
             label = "LRP", angle = 90)  +
    coord_cartesian(xlim = c(0, 1.14), ylim = c(0, Frr+0.05)) +
    labs(x = "Biomass", y = "Fishing Mortality") +
    theme_classic(base_size=14) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  
  

  