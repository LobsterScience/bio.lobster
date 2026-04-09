#### Curves for SOM ####
#maturity modelling
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)


#Import all data sources

mat_dfo = read.csv('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity/matClean.csv')
mat_old = read.csv('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity/LobsterMaturityDatabase.csv')
mat_new = read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM trips/som_2025_cleaned.csv")

## Subset and reformat

mat_dfo$CG_stage <-ifelse(mat_dfo$Cement_gland_stage<2,0,1)

mat_dfo<-mat_dfo %>%
  mutate(Org = "DFO")

mat_dfo_clean <- mat_dfo %>%
  dplyr::select(
    LFA,
    Lob_num,
    Sex,
    CG_stage,
    year,
    mon,
    Carapace_mm,
    Org
  )



## Subset and reformat

mat_old$Y = mat_old$Lat
mat_old$X = mat_old$Long*-1
mat_old$EID = 1:nrow(mat_old)
mat_old$Date = as.Date(mat_old$Date,"%d/%m/%Y")
mat_old$mon = month(mat_old$Date)
mat_old$year = year(mat_old$Date)


## Add LFAs
mat_old$LFA <- ""
mat_old$LFA <-ifelse(mat_old$Location == "Lobster Bay", 34, mat_old$LFA)
mat_old$LFA <-ifelse(mat_old$Location == "Harrigan Cove" | mat_old$Location == "Tangier"| mat_old$Location == "Mushaboom", 32, mat_old$LFA)
mat_old$LFA <-ifelse(mat_old$Location == "Port Mouton", 33,mat_old$LFA)
mat_old$LFA <-ifelse(mat_old$Location == "Canso", 31, mat_old$LFA)

mat_old<-mat_old%>%
  mutate(Org = "DFO")

#Pleopod Staging
mat_old$CG_stage <-ifelse(mat_old$Cemgland_stage<2,0,1)

mat_old<-mat_old %>%
  rename(Lob_num = Vialno)

mat_old_clean <- mat_old%>%
  dplyr::select(
    LFA,
    Org,
    Lob_num,
    Sex,
    CG_stage,
    year,
    mon,
    Carapace_mm
  )

########### New

mat_new$Sex = 2

mat_new<- mat_new%>%
  mutate( DATE_raw = substr(TRIP_ID, nchar(TRIP_ID) - 5, nchar(TRIP_ID)), DATE = dmy(DATE_raw) )

mat_new$DATE= as.Date(mat_new$DATE,"%d/%m/%Y")
mat_new$mon = month(mat_new$DATE)
mat_new$year = year(mat_new$DATE)

mat_new$CG_STAGE<-as.numeric(mat_new$CG_STAGE)
mat_new$CG_stage <-ifelse(mat_new$CG_STAGE<2,0,1)

mat_new_clean<- mat_new%>%
  dplyr::select(
    LFA,
    ORG,
    LOBSTER_ID,
    Sex,
    CG_stage,
    year,
    mon,
    lobster_length
  )

mat_new_clean <- mat_new_clean %>% 
  dplyr::rename( Org = ORG, Lob_num = LOBSTER_ID, Carapace_mm = lobster_length )


mat_dfo_clean <- mat_dfo_clean %>%
  mutate(
    LFA = as.character(LFA),
    Lob_num = as.character(Lob_num),
    Sex = as.numeric(Sex),
    CG_stage = as.numeric(CG_stage),
    Carapace_mm = as.numeric(Carapace_mm)
  )

mat_old_clean <- mat_old_clean %>%
  mutate(
    Lob_num = as.character(Lob_num),
    Carapace_mm = as.numeric(Carapace_mm)
  )

mat_new_clean <- mat_new_clean %>%
  mutate(
    Carapace_mm = as.numeric(Carapace_mm)
  )

mat_old_clean <- mat_old_clean %>%
  mutate(LFA = paste0("L", LFA))

mat_dfo_clean <- mat_dfo_clean %>%
  mutate(LFA = paste0("L", as.character(LFA)))



mat_all <- bind_rows(
  mat_dfo_clean,
  mat_new_clean,
  mat_old_clean
)

mat_all <- mat_all %>% filter(mon %in% c(5, 6))

mat_all <- mat_all %>%
  mutate(
    era = case_when(
      year %in% 2008:2011 ~ "2008–2011",
      year %in% 2022:2025 ~ "2022–2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(era))   
mat_all$era <- gsub("–", "-", mat_all$era)

## REMOVE JUMBOS 

mat_all<-mat_all[mat_all$Carapace_mm <120,]

#################  Get Curves  ################
lfa_list <- unique(mat_all$LFA) 
era_list <- c("2008-2011", "2022-2025")

run_glm_for_lfa_era <- function(df, lfa_name, era_name) {
  
  # Subset data for this LFA + era
  b <- df %>% 
    filter(LFA == lfa_name, era == era_name) %>%
    filter(!is.na(CG_stage))
  
  if(nrow(b) < 10) {
    message("Skipping ", lfa_name, " (", era_name, ") — not enough data")
    return(NULL)
  }
  
  # Fit GLM
  g <- glm(CG_stage ~ Carapace_mm, 
           data = b, 
           family = binomial(link = "logit"))
  
  # Prediction
  l <- seq(min(b$Carapace_mm), max(b$Carapace_mm), by = 0.1)
  ndata <- glmCIs(g, list(Carapace_mm = l))
  
  # L50
  L50_fit <- ndata$Carapace_mm[which.min(abs(ndata$fit_resp - 0.5))]
  L50_upr <- ndata$Carapace_mm[which.min(abs(ndata$upr - 0.5))]
  L50_lwr <- ndata$Carapace_mm[which.min(abs(ndata$lwr - 0.5))]

  n_samples <- nrow(b)
  years_used <- paste(sort(unique(b$year)), collapse = ", ")
  size_range <- paste0(min(b$Carapace_mm), "-", max(b$Carapace_mm))
  
  #### ---------------- PLOT SECTION ---------------- ####
  
  plot(b$Carapace_mm, b$CG_stage,
       pch = 16,
       xlab = "Carapace Length (mm)",
       ylab = "Proportion Mature",
       xlim = c(50, 120),
       ylim = c(0, 1),
       main = paste(lfa_name, era_name))
  par(lwd=2)
  
  lines(ndata$Carapace_mm, ndata$fit_resp, col = "black")
  lines(ndata$Carapace_mm, ndata$upr, lty = 2, col = "darkgrey")
  lines(ndata$Carapace_mm, ndata$lwr, lty = 2, col = "darkgrey")
  
  abline(v = 82.5, col = "red", lty = 3)

  text(55, 0.95, paste0("L50 = ", round(L50_fit, 1)), pos = 4)
  text(55, 0.88, paste0("n = ", n_samples), pos = 4)
  text(55, 0.81, paste0("Years: ", years_used), pos = 4)
  
  #### ------------------------------------------------ ####
  
  return(list(
    LFA = lfa_name,
    era = era_name,
    model = g,
    L50 = L50_fit,
    L50_lower = L50_lwr,
    L50_upper = L50_upr,
    n = n_samples,
    years = years_used,
    size_range = size_range
  ))
}

export_dir <- "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/PlotsTables"

for(lfa in lfa_list){
  for(era in era_list){
    
    png(
      filename = file.path(export_dir, paste0("plot_", lfa, "_", era, ".png")),
      width = 10,      # inches
      height = 8,      # inches
      units = "in",
      res = 300        # high resolution
    )
    
    run_glm_for_lfa_era(mat_all, lfa, era)
    
    dev.off()
    
  }
}

results <- list()

for(lfa in lfa_list){
  for(era in era_list){
    results[[paste(lfa, era, sep = "_")]] <- run_glm_for_lfa_era(mat_all, lfa, era)
  }
}



###Make a table 

glm_table <- do.call(rbind, lapply(results, function(x) {
  if (is.null(x)) return(NULL)
  
  model <- x$model
  coefs <- coef(summary(model))
  
  beta0 <- coefs[1, "Estimate"]
  beta1 <- coefs[2, "Estimate"]
  se0   <- coefs[1, "Std. Error"]
  se1   <- coefs[2, "Std. Error"]
  
  model_aic <- AIC(model)
  
  data.frame(
    LFA = c(x$LFA, ""),
    era = c(x$era, ""),
    n = c(x$n, ""),
    Size_Range_mm = c(x$size_range, ""),
    CL50_mm = c(round(x$L50, 1), ""),
    LCI_mm = c(round(x$L50_lower, 1), ""),
    UCI_mm = c(round(x$L50_upper, 1), ""),
    Parameter = c("Intercept", "CL"),
    Estimate = c(round(beta0, 3), round(beta1, 3)),
    SE = c(round(se0, 3), round(se1, 3)),
    AIC = c(round(model_aic, 2), ""),
    stringsAsFactors = FALSE
  )
}))

write.csv(glm_table,
          file = "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/PlotsTables/glm_table.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


### LOOK at maturity  by size


plot_hist_lfa_era <- function(df, lfa_name, era_name, glm_results) {
  
  # Subset data
  b <- df %>%
    filter(LFA == lfa_name, era == era_name)
  
  if(nrow(b) < 10) {
    message("Skipping ", lfa_name, " (", era_name, ") — not enough data")
    return(NULL)
  }
  
  # Get GLM L50
  key <- paste(lfa_name, era_name, sep = "_")
  L50_val <- round(glm_results[[key]]$L50, 1)
  
  # ---- HERE is where max_y goes ----
  max_y <- max(
    ggplot_build(
      ggplot(b, aes(x = Carapace_mm)) +
        geom_histogram(bins = 70)
    )$data[[1]]$count
  )
  # ----------------------------------
  
  pal <- c("#75D7E8", "#828289")
  
  p <- ggplot() +
    geom_histogram(
      data = b,
      aes(x = Carapace_mm, fill = as.character(CG_stage)),
      alpha = 0.6,
      position = "identity",
      bins = 70
    ) +
    geom_vline(
      data = data.frame(x = 1),
      aes(xintercept = 82.5, colour = "MLS"),
      lty = 2, lwd = 0.8
    ) +
    geom_vline(
      data = data.frame(x = 1),
      aes(xintercept = L50_val, colour = "L50"),
      lty = 3, lwd = 0.8
    ) +
    scale_y_continuous(
      limits = c(0, max_y * 1.1),   # <-- dynamic y-axis
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name ="") +
    scale_colour_manual(
      values = c("MLS" = "#F02C2C", "L50" = "blue"),
      labels = c("MLS (82.5 mm)", "L50"),
      name = "Reference lines"
    ) +
    facet_wrap(~year) +
    theme_bw() +
    theme(
      axis.line = element_line(colour = "black"),
      text = element_text(size = 15),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "right",
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    ) +
    annotate("text",
             label = paste("LFA", lfa_name),
             x = min(b$Carapace_mm) + 5,
             y = max_y * 1.05,
             size = 4,
             col = "black")
  
  return(p)
}






export_dir <- "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/PlotsTables"

for(lfa in lfa_list){
  for(era in era_list){
    
    p <- plot_hist_lfa_era(mat_all, lfa, era, results)
    
    if(!is.null(p)){
      ggsave(
        filename = paste0("HIST_", lfa, "_", era, ".png"),
        plot = p,
        path = export_dir,
        width = 12,
        height = 8,
        dpi = 300
      )
    }
  }
}




### Another Summary Table
# -----------------------------------------
# FUNCTION: Build one summary row per LFA × era
# -----------------------------------------
make_summary_row <- function(df, lfa_name, era_name) {
  
  # Subset data for this LFA × era
  b <- df %>%
    filter(LFA == lfa_name, era == era_name) %>%
    filter(!is.na(CG_stage))
  
  # Skip empty groups
  if(nrow(b) < 1) return(NULL)
  
  # Years present
  years_present <- paste(sort(unique(b$year)), collapse = ", ")
  
  # Months present
  months_present <- paste(sort(unique(b$mon)), collapse = ", ")
  
  # Sample size
  n_samples <- nrow(b)
  
  # Percent mature
  percent_mature <- round(sum(b$CG_stage == 1) / n_samples * 100, 1)
  
  # Size range
  size_range <- paste0(min(b$Carapace_mm), "-", max(b$Carapace_mm))
  
  # Return one row
  data.frame(
    LFA = lfa_name,
    era = era_name,
    years = years_present,
    months = months_present,
    n = n_samples,
    percent_mature = percent_mature,
    size_range_mm = size_range,
    stringsAsFactors = FALSE
  )
}



summary_list <- list()

for(lfa in lfa_list){
  for(era in era_list){
    summary_list[[paste(lfa, era, sep = "_")]] <- 
      make_summary_row(mat_all, lfa, era)
  }
}

final_summary_table <- do.call(rbind, summary_list)


write.csv(
  final_summary_table,
  file = "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/PlotsTables/final_summary_table.csv",
  row.names = FALSE
)
