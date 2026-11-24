#' @export
lobsterBodyArea <- function(CL, sex = c("male","female"),   mature = TRUE,  n = 10000,  seed = 123){
  sex <- match.arg(sex)
  set.seed(seed)
  out =  lapply(CL,function(cl) {
  # Carapace width
  CW_coef <- rnorm(n, mean = 0.72, sd = 0.04)
  
  # Abdomen length
  AL_coef <- rnorm(n, mean = 2.8, sd = 0.15)
  
  # Abdomen width
  AW_coef <- if(sex == "female") {
      if(mature)
      rnorm(n, mean = 0.78, sd = 0.05)
      else
      rnorm(n, mean = 0.62, sd = 0.05)
    } else {
      rnorm(n, mean = 0.62, sd = 0.04)
    }
  
  CW <- CW_coef * cl
  
  AC <- pi * (cl/2) * (CW/2)
  
  AL <- AL_coef * cl
  AW <- AW_coef * cl
  
  AA <- pi * (AL/2) * (AW/2)
  
  #claws
  
  claw_coef <- if(sex == "male"){
      if(mature)
      runif(n, .22, .40)
    else
      runif(n, .10, .20)
    } else {
    
    if(mature)
      runif(n, .12, .22)
    else
      runif(n, .08, .15)
    
  }
  
  body <- AC + AA
  claw = claw_coef * body
  
  total_area <- body + claw
  

  c( mean = mean(total_area),
     median = median(total_area),
     sd = sd(total_area)
  )
    
  })
  out = do.call(rbind, out) |>
          data.frame(CL = CL)
  
  return(out)
}

lobsterCover <- function( density_t_km2,mean_weight_kg=0.75, area_m2 = 0.018){
   100 * density_t_km2 * 1000 /    mean_weight_kg *    area_m2 / 1e6
}
