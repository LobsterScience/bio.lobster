
# Install if needed:
# install.packages(c("dplyr","tidyr","purrr","ggplot2","tibble"))

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)
set.seed(42)



create_params <- function() {
  list(
    # Fleet / season
    n_vessels              = 60,          # number of active vessels
    season_days            = 90,          # fishing season length
    trips_per_vessel_mean  = 45,          # avg trips per vessel
    trips_per_vessel_sd    = 6,
    traps_per_trip_mean    = 250,         # hauled per trip
    traps_per_trip_sd      = 30,

    # Catch (CPUE) model — per trap (kg)
    cpue_base_mean         = 0.8,         # <-- replace with your data
    cpue_base_sd           = 0.3,
    # soak-time response (saturating curve parameters)
    soak_time_mean         = 2.0,         # days
    soak_time_sd           = 0.6,
    cpue_a                 = 1.2,         # max kg/trap potential
    cpue_b                 = 0.9,         # response rate (higher = faster saturation)

    # Fuel model
    fuel_burn_lph_mean     = 45,          # litres per hour (steaming+hauling)
    fuel_burn_lph_sd       = 8,
    hours_per_trip_mean    = 8,
    hours_per_trip_sd      = 1.5,
    fuel_efficiency_gain   = 0.12,        # 12% reduction scenario

    # Bait model
    bait_kg_per_trap_mean  = 0.30,        # kg bait/trap/soak (edit to your data)
    bait_kg_per_trap_sd    = 0.05,
    bait_reduction_factor  = 0.25,        # 25% less bait scenario

    # Processing (optional – basic)
    processing_kwh_per_kg  = 0.20,        # kWh/kg lobster (grading/holding/cooking)
    packaging_plastic_kg_per_kg = 0.05,   # kg plastic per kg lobster (if processed/packed)
    include_processing     = TRUE,

    # Materials (annualized per FU)
    traps_per_vessel       = 300,
    trap_mass_steel_kg     = 20,          # kg steel per trap
    trap_plastic_kg        = 2,           # plastic coatings/cones etc
    rope_mass_per_vessel_kg = 400,        # kg rope per vessel
    buoy_hdpe_per_vessel_kg = 60,
    gear_service_life_seasons = 6,
    gear_loss_rate         = 0.01,        # 1% lost per season (ghost gear module)

    # Emission factors (placeholder defaults – replace with LCI/primary data)
    ef_fuel_combustion_kgco2e_per_l = 2.68,  # tailpipe CO2
    ef_fuel_upstream_kgco2e_per_l   = 0.50,  # extraction/refining/transport
    ef_bait_purpose_kgco2e_per_kg   = 5.0,   # purpose-caught bait
    ef_bait_byproduct_kgco2e_per_kg = 1.5,   # by-product bait
    ef_grid_kgco2e_per_kwh          = 0.60,  # regional electricity
    ef_steel_kgco2e_per_kg          = 1.9,
    ef_plastic_kgco2e_per_kg        = 2.5,
    ef_rope_pp_kgco2e_per_kg        = 1.8,
    ef_hdpe_kgco2e_per_kg           = 1.7,

    # Scenario controls
    scenario_bait_type              = "purpose", # "purpose" or "byproduct"
    scenario_fuel_efficiency        = FALSE,
    scenario_bait_optimization      = FALSE,
    scenario_fewer_hauls            = FALSE,     # reduce trips, longer soak
    fewer_hauls_trip_reduction      = 0.20,      # 20% fewer trips
    fewer_hauls_soak_increase_days  = 1.0,       # +1 day soak
    scenario_include_distribution   = FALSE,     # optional extension
    distribution_tkm_per_kg         = 50,        # tonne-km per kg lobster
    ef_truck_kgco2e_per_tkm         = 0.07
  )

}

# CPUE as a function of soak time (saturating curve)
cpue_from_soak <- function(params, soak_days) {
  # Base potential adjusted by soak time: a * (1 - exp(-b * soak))
  raw <- params$cpue_a * (1 - exp(-params$cpue_b * soak_days))
  # Add random noise around base mean & sd
  base <- rnorm(1, mean = params$cpue_base_mean, sd = params$cpue_base_sd)
  # Blend: 50% soak-driven, 50% baseline (tune as needed)
  max(0, 0.5 * raw + 0.5 * base)
}

# One trip simulation
simulate_trip <- function(params, vessel_eff_mult = 1) {
  # Hauls and soak time
  traps_hauled <- max(10, round(rnorm(1, params$traps_per_trip_mean, params$traps_per_trip_sd)))
  soak_days <- max(0.5, rnorm(1, params$soak_time_mean, params$soak_time_sd))
  if (params$scenario_fewer_hauls) {
    soak_days <- soak_days + params$fewer_hauls_soak_increase_days
  }
  # CPUE per trap
  cpue <- cpue_from_soak(params, soak_days)
  catch_kg <- traps_hauled * cpue
  # Bait used
  bait_per_trap <- rnorm(1, params$bait_kg_per_trap_mean, params$bait_kg_per_trap_sd)
  if (params$scenario_bait_optimization) {
    bait_per_trap <- bait_per_trap * (1 - params$bait_reduction_factor)
  }
  bait_kg <- traps_hauled * bait_per_trap
  # Fuel use
  hours <- max(1, rnorm(1, params$hours_per_trip_mean, params$hours_per_trip_sd))
  fuel_lph <- rnorm(1, params$fuel_burn_lph_mean, params$fuel_burn_lph_sd) * vessel_eff_mult
  if (params$scenario_fuel_efficiency) {
    fuel_lph <- fuel_lph * (1 - params$fuel_efficiency_gain)
  }
  fuel_l <- hours * fuel_lph
  tibble(traps_hauled, soak_days, catch_kg, bait_kg, fuel_l, hours)
}

# One vessel's season
simulate_vessel_season <- function(params) {
  # Vessel-specific efficiency multiplier (±10%)
  vessel_eff_mult <- rnorm(1, mean = 1, sd = 0.10)
  trips_n <- max(1, round(rnorm(1, params$trips_per_vessel_mean, params$trips_per_vessel_sd)))
  if (params$scenario_fewer_hauls) {
    trips_n <- round(trips_n * (1 - params$fewer_hauls_trip_reduction))
  }
  trips <- map_dfr(seq_len(trips_n), ~ simulate_trip(params, vessel_eff_mult))
  trips %>%
    summarise(
      trips_n       = n(),
      catch_kg      = sum(catch_kg),
      bait_kg       = sum(bait_kg),
      fuel_l        = sum(fuel_l),
      hours_total   = sum(hours),
      traps_hauled  = sum(traps_hauled),
      soak_days_avg = mean(soak_days)
    )
}

# Fleet season
simulate_fleet_season <- function(params) {
  fleet <- map_dfr(seq_len(params$n_vessels), ~ simulate_vessel_season(params))
  fleet_summary <- fleet %>%
    summarise(
      vessels_n   = n(),
      trips_n     = sum(trips_n),
      catch_kg    = sum(catch_kg),
      bait_kg     = sum(bait_kg),
      fuel_l      = sum(fuel_l),
      traps_hauled= sum(traps_hauled),
      hours_total = sum(hours_total)
    )
  list(vessel_detail = fleet, fleet_summary = fleet_summary)
}



# Compute emissions contributions and intensity (kgCO2e/kg lobster)
compute_lcia <- function(params, fleet_summary) {
  # Fuel
  ef_fuel_total <- params$ef_fuel_combustion_kgco2e_per_l + params$ef_fuel_upstream_kgco2e_per_l
  gwp_fuel <- fleet_summary$fuel_l * ef_fuel_total

  # Bait EF by type
  ef_bait <- if (params$scenario_bait_type == "byproduct") {
    params$ef_bait_byproduct_kgco2e_per_kg
  } else params$ef_bait_purpose_kgco2e_per_kg
  gwp_bait <- fleet_summary$bait_kg * ef_bait

  # Processing & packaging (optional)
  gwp_processing <- 0
  gwp_packaging  <- 0
  if (params$include_processing) {
    gwp_processing <- fleet_summary$catch_kg * params$processing_kwh_per_kg * params$ef_grid_kgco2e_per_kwh
    gwp_packaging  <- fleet_summary$catch_kg * params$packaging_plastic_kg_per_kg * params$ef_plastic_kgco2e_per_kg
  }

  # Materials annualized to FU
  # Per vessel material per season / total catch => kg material/kg lobster
  steel_total <- params$n_vessels * params$traps_per_vessel * params$trap_mass_steel_kg / params$gear_service_life_seasons
  plastic_total <- params$n_vessels * params$traps_per_vessel * params$trap_plastic_kg / params$gear_service_life_seasons
  rope_total <- params$n_vessels * params$rope_mass_per_vessel_kg / params$gear_service_life_seasons
  buoy_total <- params$n_vessels * params$buoy_hdpe_per_vessel_kg / params$gear_service_life_seasons

  gwp_materials <- steel_total * params$ef_steel_kgco2e_per_kg +
                   plastic_total * params$ef_plastic_kgco2e_per_kg +
                   rope_total * params$ef_rope_pp_kgco2e_per_kg +
                   buoy_total * params$ef_hdpe_kgco2e_per_kg

  # Optional distribution
  gwp_distribution <- 0
  if (params$scenario_include_distribution) {
    tkm <- fleet_summary$catch_kg * params$distribution_tkm_per_kg
    gwp_distribution <- tkm * params$ef_truck_kgco2e_per_tkm
  }

  total_gwp <- gwp_fuel + gwp_bait + gwp_processing + gwp_packaging + gwp_materials + gwp_distribution
  intensity <- total_gwp / fleet_summary$catch_kg

  contrib <- tibble(
    category = c("Fuel","Bait","Processing","Packaging","Materials","Distribution"),
    kgCO2e   = c(gwp_fuel,gwp_bait,gwp_processing,gwp_packaging,gwp_materials,gwp_distribution)
  )

  list(
    intensity_kgco2e_per_kg = as.numeric(intensity),
    total_gwp_kgco2e        = as.numeric(total_gwp),
    contrib                 = contrib
  )
}

# Helper to switch scenario flags quickly
apply_scenario <- function(params, scenario) {
  p <- params
  if (scenario == "baseline") {
    p$scenario_fuel_efficiency   <- FALSE
    p$scenario_bait_optimization <- FALSE
    p$scenario_bait_type         <- "purpose"
    p$scenario_fewer_hauls       <- FALSE
  }
  if (scenario == "fuel_efficiency") p$scenario_fuel_efficiency <- TRUE
  if (scenario == "bait_byproduct")  p$scenario_bait_type <- "byproduct"
  if (scenario == "bait_optimization") p$scenario_bait_optimization <- TRUE
  if (scenario == "fewer_hauls") p$scenario_fewer_hauls <- TRUE
  if (scenario == "combined") {
    p$scenario_fuel_efficiency   <- TRUE
    p$scenario_bait_type         <- "byproduct"
    p$scenario_bait_optimization <- TRUE
    p$scenario_fewer_hauls       <- TRUE
  }
  p
}

run_monte_carlo <- function(params, scenario = "baseline", n_iter = 500) {
  p <- apply_scenario(params, scenario)
  res <- map_dfr(seq_len(n_iter), function(i) {
    fleet <- simulate_fleet_season(p)
    lcia <- compute_lcia(p, fleet$fleet_summary)
    tibble(
      iteration = i,
      scenario  = scenario,
      catch_kg  = fleet$fleet_summary$catch_kg,
      fuel_l    = fleet$fleet_summary$fuel_l,
      bait_kg   = fleet$fleet_summary$bait_kg,
      intensity_kgco2e_per_kg = lcia$intensity_kgco2e_per_kg,
      total_gwp_kgco2e        = lcia$total_gwp_kgco2e
    )
  })
  res
}

run_scenarios <- function(params, scenarios = c("baseline","fuel_efficiency","bait_byproduct","fewer_hauls","combined"),
                          n_iter = 500) {
  bind_rows(lapply(scenarios, function(s) run_monte_carlo(params, s, n_iter)))
}



summary_table <- function(df) {
  df %>%
    group_by(scenario) %>%
    summarise(
      catch_kg_mean   = mean(catch_kg),
      fuel_l_mean     = mean(fuel_l),
      bait_kg_mean    = mean(bait_kg),
      gwp_total_mean  = mean(total_gwp_kgco2e),
      gwp_intensity_mean = mean(intensity_kgco2e_per_kg),
      gwp_intensity_p05  = quantile(intensity_kgco2e_per_kg, 0.05),
      gwp_intensity_p50  = quantile(intensity_kgco2e_per_kg, 0.50),
      gwp_intensity_p95  = quantile(intensity_kgco2e_per_kg, 0.95),
      .groups = "drop"
    )
}

plot_intensity <- function(df) {
  ggplot(df, aes(x = scenario, y = intensity_kgco2e_per_kg)) +
    geom_violin(fill = "#3478F6", alpha = 0.4) +
    geom_boxplot(width = 0.2, outlier.shape = NA) +
    labs(x = "Scenario", y = "kg CO₂e per kg lobster (dockside)",
         title = "LCA Intensity Distribution by Scenario") +
    theme_minimal(base_size = 12)
}

# Stacked contributions for a single (representative) iteration
contrib_for_scenario <- function(params, scenario = "baseline") {
  p <- apply_scenario(params, scenario)
  fleet <- simulate_fleet_season(p)
  compute_lcia(p, fleet$fleet_summary)$contrib %>%
    mutate(scenario = scenario)
}

plot_contrib_stacked <- function(params, scenario = "baseline") {
  contrib <- contrib_for_scenario(params, scenario)
  ggplot(contrib, aes(x = category, y = kgCO2e, fill = category)) +
    geom_col() +
    labs(title = paste0("Contributions to Total GWP – ", scenario),
         x = NULL, y = "kg CO₂e (fleet-season, representative draw)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
    }



params <- create_params()

# Monte Carlo across scenarios (500 iterations each)
results <- run_scenarios(params, n_iter = 500)

# Summary table with CIs
summary_table(results)

# Plot intensity distribution by scenario
plot_intensity(results)

# Stacked contribution plot for baseline and combined scenario; gwp is global warming potential
plot_contrib_stacked(params, "baseline")
plot_contrib_stacked(params, "combined")
