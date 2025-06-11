# Function to estimate max sustainable harvest dynamically
estimate_max_harvest <- function(population, growth_rate, carrying_capacity) {
  return(max(0,growth_rate * population * (1 - population / carrying_capacity)))  # Ensures harvest does not exceed growth and keeps it from going below 0
}


# Function to simulate population growth with depensation, dynamic harvesting, and cycles

simulate_population <- function(initial_pop, growth_rate, carrying_capacity, base_harvest_rate, USR,LRP, min_harvest_rate, depensation_threshold, depensation_factor, time_steps, variation_sd, density_effect, base_cycle_period, cycle_var_sd) {
  population <- numeric(time_steps)
  harvest_rate_vector <- numeric(time_steps)
  max_harvest_vector <- numeric(time_steps)
  population[1] <- initial_pop
  
  for (t in 2:time_steps) {
    max_harvest_vector[t] <- estimate_max_harvest(population[t-1], growth_rate, carrying_capacity)
  
    # Dynamic harvest rate adjustment
    if (population[t-1] > USR) {
      harvest_rate_vector[t] <- base_harvest_rate  # Normal harvest rate
    } else if  (population[t-1] <=USR & population[t-1]>LRP ){
      harvest_rate_vector[t] <- base_harvest_rate * (population[t-1] / USR)  # Reduced harvest
    } else {
      harvest_rate_vector[t] <- min_harvest_rate
    }
    # Adding random variation to growth
    random_growth <- rnorm(1, mean = growth_rate, sd = growth_variation_sd)
    
    # Density-dependent mortality term
    density_dependent_mortality <- density_effect * (population[t-1] / carrying_capacity)^2
    
    # Variable cycle period
    cycle_period <- base_cycle_period + rnorm(1, mean = 0, sd = cycle_var_sd)
    cycle_period <- max(1, cycle_period)  # Ensure cycle period doesn't become negative
    
    # Cyclical term affected by density
    cycle_effect <- sin(2 * pi * t / cycle_period) * population[t-1]/carrying_capacity * 0.05
    
    # **Depensation Effect**: Growth rate reduced at low population sizes
    adjusted_growth_rate <- ifelse(population[t-1] < depensation_threshold, 
                                   random_growth * (population[t-1] / depensation_threshold)^depensation_factor, 
                                   random_growth)
    
    # Logistic growth formula with depensation, harvest, randomness, density dependence, and cycles
    population[t] <- population[t-1] + adjusted_growth_rate * population[t-1] * (1 - population[t-1] / carrying_capacity) - (harvest_rate_vector[t] + density_dependent_mortality + cycle_effect)*population[t-1]
    
    # Ensure population does not fall below zero
    if (population[t] < 0) {
      population[t] <- 0
      browser()
    }
  }
  
  return(list(population = population, harvest_rate = harvest_rate_vector, max_harvest_vector = max_harvest_vector, catch_vector = harvest_rate_vector * population ))
  
}

# Parameters
initial_pop <- 10000
growth_rate <- 0.4
growth_variation_sd <- .01
carrying_capacity <- 10000
base_harvest_rate <- 0
min_harvest_rate <- 0.01
USR <- carrying_capacity/2 *.8 
LRP <- carrying_capacity/2 *.4 
depensation_threshold <- LRP *.8
depensation_factor <- 1.5
time_steps <- 100

density_effect <- .05
base_cycle_period <- 12
cycle_var_sd = .5

# Run simulation
result <- simulate_population(initial_pop, growth_rate, carrying_capacity, base_harvest_rate, USR,LRP,min_harvest_rate, depensation_threshold, depensation_factor, time_steps, variation_sd, density_effect, base_cycle_period, cycle_var_sd)
population_over_time <- result$population
harvest_rate_vector <- result$harvest_rate
max_harvest_vector <- result$max_harvest_vector
catch_vector <- result$catch_vector

# Plot results
par(mfrow=c(3,1))  # Split plotting area into three
plot(1:time_steps, population_over_time, type="l", col="blue", lwd=2, xlab="Time Steps", ylab="Population Size",
     main="Population Growth with Sustainable Harvesting")
plot(1:time_steps, harvest_rate_vector, type="l", col="red", lwd=2, xlab="Time Steps", ylab="Actual Harvest Rate",
     main="Harvest Rate Over Time")
plot(1:time_steps, max_harvest_vector, type="l", col="green", lwd=2, xlab="Time Steps", ylab="Maximum Sustainable Harvest",
     main="Estimated Maximum Harvest Rate Over Time")