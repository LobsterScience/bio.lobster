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
FSRS = subset(FSRSvesday,LFA==33&SYEAR<=p$current.assessment.year) #index year
FSRS$fYEAR=as.factor(FSRS$SYEAR)
FSRS$logTRAPS=log(FSRS$TOTAL_TRAPS) # create log traps
FSRS$LEGALS=as.numeric(FSRS$LEGALS)
FSRS$TEMP[FSRS$TEMP==-99]=NA # remove temp nas
FSRS=subset(FSRS,!is.na(TEMP)&TEMP>(-5))



#Legals
#L_0 = stan_glm.nb(LEGALS ~ 1, offset=logTRAPS, data = FSRS,iter=iter) #intecept only or base model
#L_1 = stan_glm.nb(LEGALS ~ fYEAR, offset=logTRAPS, data = FSRS,iter=iter) #just a year term
#L_2 = stan_glm.nb(LEGALS ~ fYEAR + DOS, offset=logTRAPS, data = FSRS,iter=iter)
#L_3 = stan_glm.nb(LEGALS ~ fYEAR + DOS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
#L_4 = stan_glm.nb(LEGALS ~ fYEAR + DOS * TEMP, offset=logTRAPS, data = FSRS,iter=iter)
#L_5 = stan_glm.nb(LEGALS ~ fYEAR * DOS + TEMP, offset=logTRAPS, data = FSRS,iter=iter) 

#comparison
#waic0 = waic(L_0)
#waic1 = waic(L_1)
#waic2 = waic(L_2)
#waic3 = waic(L_3)
#waic4 = waic(L_4)
#waic5 = waic(L_5)


compare_models_safe <- function(FSRS, iter = 2000, save_dir = "ModelResults") {
    # FSRS: your dataset, already in memory (data.frame)
    # must include LEGALS, RECRUITS, logTRAPS, fYEAR, DOS, TEMP
    
    # Make sure the save directory exists
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    # Define your model formulas
   # formulas <- list(
   #     L_0 = LEGALS ~ 1,
   #     L_1 = LEGALS ~ fYEAR,
   #     L_2 = LEGALS ~ fYEAR + DOS,
   #     L_3 = LEGALS ~ fYEAR + DOS + TEMP,
   #     L_4 = LEGALS ~ fYEAR + DOS * TEMP,
   #     L_5 = LEGALS ~ fYEAR * DOS + TEMP,
   #     L_6 = LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP
   # )
    
    # Define your model formulas
     formulas <- list(
         L_0 = RECRUITS ~ 1,
         L_1 = RECRUITS ~ fYEAR,
         L_2 = RECRUITS ~ fYEAR + DOS,
         L_3 = RECRUITS ~ fYEAR + DOS + TEMP,
         L_4 = RECRUITS ~ fYEAR + DOS * TEMP,
         L_5 = RECRUITS ~ fYEAR * DOS + TEMP,
         L_6 = RECRUITS ~ fYEAR + DOS + TEMP + DOS*TEMP
     )
    # Path for cumulative results
    waic_file <- file.path(save_dir, "model_comparison.csv")
    
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
    
    # Loop through model list
    for (name in names(formulas)) {
        cat("\n--- Fitting", name, "---\n")
        
        # If model file already exists, skip it
        model_path <- file.path(save_dir, paste0(name, ".rds"))
        if (file.exists(model_path)) {
            cat("Model", name, "already exists — skipping.\n")
            next
        }
        
        #----------------------------------------------
        # Fit the model using stan_glm.nb
        #----------------------------------------------
        fit <- tryCatch({
            stan_glm.nb(
                formula = formulas[[name]],
                offset = FSRS$logTRAPS,   # explicitly uses FSRS dataset column
                data = FSRS,
                iter = iter
            )
        }, error = function(e) {
            cat("❌ Error fitting", name, ":", conditionMessage(e), "\n")
            return(NULL)
        })
        
        if (is.null(fit)) next
        
        # Save model immediately so it’s not lost if R crashes
        saveRDS(fit, model_path)
        assign(name, fit, envir = .GlobalEnv)
        
        # Compute WAIC and LOOIC
        w <- waic(fit)
        l <- loo(fit)
        
        # Record both metrics
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
        write.csv(waics, waic_file, row.names = FALSE)
        
        cat("✅ Saved", name, "with WAIC =", round(row$waic,2),
            "and LOOIC =", round(row$looic,2), "\n")
    }
    
    # Sort models by WAIC before returning
    waics <- waics[order(waics$waic), ]
    print(waics)
    return(waics)
}


# Example: Your dataset must already exist
# It must include: LEGALS, logTRAPS, fYEAR, DOS, TEMP
# FSRS <- read.csv("path/to/FSRS_data.csv")

# Run model comparisons safely
waic_summary <- compare_models_safe(FSRS, iter = 4000)

#If your session crashes, just re-run the same command:
    
waic_summary <- compare_models_safe(FSRS, iter = 4000)



