p = bio.lobster::load.environment()
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)
require(dplyr)
library(rstanarm)
library(loo)

la()

# define place for figures to go
save_dir = file.path(project.datadirectory("bio.lobster","assessments","33.34.41.framework","fsrs.model.testing"))
dir.create( save_dir, recursive = TRUE, showWarnings = FALSE )

FSRSvesday<-FSRSModelData()
FSRS = subset(FSRSvesday,LFA==33&SYEAR<p$current.assessment.year) #index year
FSRS$fYEAR=as.factor(FSRS$SYEAR)
FSRS$logTRAPS=log(FSRS$TOTAL_TRAPS) # create log traps
FSRS$LEGALS=as.numeric(FSRS$LEGALS)
FSRS$TEMP[FSRS$TEMP==-99]=NA # remove temp nas
FSRS=subset(FSRS,!is.na(TEMP)&TEMP>(-5))

# Example: Your dataset must already exist
# It must include: LEGALS, logTRAPS, fYEAR, DOS, TEMP
# FSRS <- read.csv("path/to/FSRS_data.csv")


compare_models_safe <- function(FSRS, response, iter = 2000, save_dir = "ModelResults") {
  
  #--------------------------------------------------------------
  # Make save_dir an absolute path
  #--------------------------------------------------------------
  
  save_dir <- normalizePath(
    save_dir,
    winslash = "/",
    mustWork = FALSE
  )
  
  # Create response-specific folder
  response_dir <- file.path(save_dir, response)
  
  if (!dir.exists(response_dir)) {
    dir.create(response_dir, recursive = TRUE)
  }
  
  cat("\n========================================\n")
  cat("Response:", response, "\n")
  cat("Saving models to:", response_dir, "\n")
  cat("========================================\n")
  
  
  # Define model formulas
  formulas <- list(
    M_0 = as.formula(paste(response, "~ 1")),
    M_1 = as.formula(paste(response, "~ fYEAR")),
    M_2 = as.formula(paste(response, "~ fYEAR + DOS")),
    M_3 = as.formula(paste(response, "~ fYEAR + DOS + TEMP")),
    M_4 = as.formula(paste(response, "~ fYEAR + DOS * TEMP")),
    M_5 = as.formula(paste(response, "~ fYEAR * DOS + TEMP")),
    M_6 = as.formula(paste(response, "~ fYEAR + DOS + TEMP + DOS*TEMP"))
  )
  
  # Path for cumulative results
  waic_file <- file.path(response_dir, "model_comparison.csv")
  
  # If previous results exist, load them
  if (file.exists(waic_file)) {
    waics <- read.csv(waic_file)
  } else {
    waics <- data.frame(
      model = character(),
      waic = numeric(),
      se_waic = numeric(),
      looic = numeric(),
      se_looic = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # Loop through models
  for (name in names(formulas)) {
    
    cat("\n--- Fitting", response, name, "---\n")
    
    model_path <- file.path(response_dir, paste0(name, ".rds"))
    
    # Skip if model already exists
    if (file.exists(model_path)) {
      cat("Model", name, "already exists — skipping.\n")
      next
    }
    
    # Fit model
    fit <- tryCatch({
      
      stan_glm.nb(
        formula = formulas[[name]],
        offset = FSRS$logTRAPS,
        data = FSRS,
        iter = iter
      )
      
    }, error = function(e) {
      
      cat("❌ Error fitting", name, ":", conditionMessage(e), "\n")
      return(NULL)
      
    })
    
    if (is.null(fit)) next
    
    # Save model immediately
    saveRDS(fit, model_path)
    
    # Assign to global environment
    assign(
      paste0(response, "_", name),
      fit,
      envir = .GlobalEnv
    )
    
    # Compute WAIC and LOOIC
    w <- waic(fit)
    l <- loo(fit)
    
    # Record metrics
    row <- data.frame(
      model = name,
      waic = w$estimates["waic", "Estimate"],
      se_waic = w$estimates["waic", "SE"],
      looic = l$estimates["looic", "Estimate"],
      se_looic = l$estimates["looic", "SE"],
      stringsAsFactors = FALSE
    )
    
    # Append and save cumulative results
    waics <- rbind(waics, row)
    write.csv(
      waics,
      waic_file,
      row.names = FALSE
    )
    
    cat(
      "✅ Saved", response, name,
      "with WAIC =", round(row$waic, 2),
      "and LOOIC =", round(row$looic, 2), "\n"
    )
  }
  
  # Sort by WAIC
  waics <- waics[order(waics$waic), ]
  
  print(waics)
  
  return(waics)
}
# Run model comparisons safely
#waic_summary <- compare_models_safe(FSRS, iter = 4000)

responses <- c("LEGALS", "RECRUITS")

results <- lapply(
  responses,
  function(x) {
    compare_models_safe(
      FSRS = FSRS,
      response = x,
      iter = 2000,
      save_dir = "ModelResults"
    )
  }
)

names(results) <- responses

#If your session crashes, just re-run the same command:
    
waic_summary <- compare_models_safe(FSRS, iter = 4000)


#--------------------------------------------------------------
# Create combined model comparison table
#--------------------------------------------------------------

# Read results
recruits <- read.csv(
  file.path("ModelResults", "RECRUITS", "model_comparison.csv")
)

legals <- read.csv(
  file.path("ModelResults", "LEGALS", "model_comparison.csv")
)

# Model statements
model_statements <- c(
  L_0 = "Intercept only",
  L_1 = "fYEAR",
  L_2 = "fYEAR + DOS",
  L_3 = "fYEAR + DOS + TEMP",
  L_4 = "fYEAR + DOS * TEMP",
  L_5 = "fYEAR * DOS + TEMP",
  L_6 = "fYEAR + DOS + TEMP + DOS * TEMP"
)

# Create combined table
comparison_table <- data.frame(
  
  `Model Statement` = unname(model_statements[recruits$model]),
  
  # Recruit-sized lobster
  `Recruit WAIC` = recruits$waic,
  `Recruit WAIC SE` = recruits$se_waic,
  `Recruit LOOIC` = recruits$looic,
  `Recruit LOOIC SE` = recruits$se_looic,
  
  # Legal-sized lobster
  `Legal WAIC` = legals$waic,
  `Legal WAIC SE` = legals$se_waic,
  `Legal LOOIC` = legals$looic,
  `Legal LOOIC SE` = legals$se_looic,
  
  stringsAsFactors = FALSE
)

# Round metrics
comparison_table[, -1] <- round(
  comparison_table[, -1],
  2
)

# Save combined results
write.csv(
  comparison_table,
  file.path(
    "ModelResults",
    "model_comparison_combined.csv"
  ),
  row.names = FALSE
)

# Display
comparison_table

