### REVIEW DATA COLLECTED ###

library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
require(devtools)
require(bio.lobster)
require(bio.utilities)
library(stringr)
library(purrr)
library(readr)
library(lubridate)
library(statmod)
library(broom)
library(pwr)

#### 2025 Data ####
mat2025<-read_csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM Files R/combineddata2025.csv")

mat2025<- subset(mat2025,CG_STAGE <5)
mat2025$CG_MAT<-ifelse(mat2025$CG_STAGE <2,0,1)
mat2025<-mat2025[mat2025$Carapace_length>40 & mat2025$Carapace_length<120,]

mat2025$MONTH <- month(mat2025$USE_DATE)
mat2025$YEAR <- year(mat2025$USE_DATE)
mat2025 <- mat2025 %>% filter(rowSums(is.na(.)) < ncol(.))


mat2025<-mat2025 %>%
  dplyr::select(
   LFA,
   ORG,
   LAT,
   LONG,
   LOBSTER_NUMBER,
   CG_MAT,
   MONTH,
   YEAR,
   Carapace_length,
   HARDNESS
  )
mat2025<-as.data.frame(mat2025)


#### BRING IN OLDER DATA

mat_dfo = read.csv('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity/matClean.csv')
mat_dfo$CG_stage <-ifelse(mat_dfo$Cement_gland_stage<2,0,1)
mat_dfo<-mat_dfo[mat_dfo$Carapace_mm<120,]
mat_dfo<-mat_dfo[mat_dfo$Sex ==2, ]
mat_dfo<-mat_dfo %>%
  mutate(Org = "DFO")

mat_dfo_clean <- mat_dfo %>%
  dplyr::select(
    LFA,
    Lob_num,
    CG_stage,
    year,
    mon,
    Carapace_mm,
    Org,
    X,
    Y, 
    Shell_hardness)

### RENAME
mat_dfo_clean <- mat_dfo_clean %>%
  dplyr::rename(
    LFA = LFA,                       
    ORG = Org,                       
    LAT = Y,                         
    LONG = X,                        
    LOBSTER_NUMBER = Lob_num,        
    CG_MAT = CG_stage,       
    MONTH = mon,
    YEAR = year,
    Carapace_length = Carapace_mm,    
    HARDNESS = Shell_hardness         
  )

### Match Format

mat_dfo_clean$Carapace_length <- as.numeric(mat_dfo_clean$Carapace_length)
mat_dfo_clean$LFA <- paste0("L", mat_dfo_clean$LFA)
mat_dfo_clean <- mat_dfo_clean %>% 
  mutate(
    YEAR = as.numeric(as.character(YEAR)),      # Convert YEAR to numeric
    MONTH = as.numeric(as.character(MONTH)),    # Convert MONTH to numeric
    HARDNESS = as.numeric(as.character(HARDNESS)) # Convert HARDNESS to numeric
  )

matdat <- rbind(mat2025, mat_dfo_clean)

unique_values <- list(
  Hardness      = unique(matdat$HARDNESS),
  Carapace_length    = unique(matdat$Carapace_length),
  GG_MAT      = unique(matdat$GG_MAT),
  YEAR  = unique(matdat$YEAR),
  LFA = unique(matdat$LFA),
 MONTH = unique(matdat$MONTH)
)
unique_values



#### REVIEW THE DATA

unique_LFAs <- unique(matdat$LFA)

results <- data.frame(
  LFA = character(),
  size_50_fit = numeric(),
  size_50_upr = numeric(),
  size_50_lwr = numeric(),
  mcFadden_R2 = numeric(),
  stringsAsFactors = FALSE
)


all_residuals <- list()

for (lfa in unique_LFAs) {
  # Filter data for the current LFA
  b <- matdat %>% filter(LFA == lfa)
  
  # Fit logistic regression model
  g <- glm(CG_MAT ~ Carapace_length, data = b, family = binomial(link = 'logit'))
  
  # Calculate McFadden R-squared
  null_model <- glm(CG_MAT ~ 1, data = b, family = binomial(link = 'logit'))
  mcFadden_R2 <- 1 - (logLik(g) / logLik(null_model))  # McFadden's pseudo-R2
  
  # Extract size at 50% maturity (size_50_fit)
  size_50_fit <- -coef(g)[1] / coef(g)[2]  # Intercept / slope
  
  # Get standard error for confidence intervals
  coef_summary <- summary(g)$coefficients
  slope <- coef_summary["Carapace_length", "Estimate"]
  slope_se <- coef_summary["Carapace_length", "Std. Error"]
  intercept <- coef_summary["(Intercept)", "Estimate"]
  intercept_se <- coef_summary["(Intercept)", "Std. Error"]
  
  # Confidence intervals (95%)
  z_value <- qnorm(0.975)  # 1.96 for 95% confidence
  size_50_lwr <- -(intercept + z_value * intercept_se) / (slope - z_value * slope_se)
  size_50_upr <- -(intercept - z_value * intercept_se) / (slope + z_value * slope_se)
  
  results <- rbind(results, data.frame(
    LFA = lfa,
    size_50_fit = size_50_fit,
    size_50_upr = size_50_upr,
    size_50_lwr = size_50_lwr,
    mcFadden_R2 = as.numeric(mcFadden_R2)
  ))
  
  if (requireNamespace("statmod", quietly = TRUE)) {
    b$CL_residuals <- statmod::qresid(g)
  } else {
    b$CL_residuals <- residuals(g, type = "response")
  }
  all_residuals[[lfa]] <- b
  
  # --- Plot by YEAR ---
  if ("YEAR" %in% names(b)) {
    p_year <- ggplot(b, aes(x = Carapace_length, y = CL_residuals, color = as.factor(YEAR))) +
      geom_point(size = 3) +
      ggtitle(paste0("Residuals by YEAR, LFA: ", lfa))
    print(p_year)
    # ggsave(filename = paste0(lfa, "_residuals_year.png"), plot = p_year)
  }
  
  # --- Plot by MONTH ---
  if ("MONTH" %in% names(b)) {
    p_month <- ggplot(b, aes(x = Carapace_length, y = CL_residuals, color = as.factor(MONTH))) +
      geom_point(size = 3) +
      ggtitle(paste0("Residuals by MONTH, LFA: ", lfa))
    print(p_month)
    # ggsave(filename = paste0(lfa, "_residuals_month.png"), plot = p_month)
  }
  
  # --- Plot by HARDNESS ---
  if ("HARDNESS" %in% names(b)) {
    p_hardness <- ggplot(b, aes(x = Carapace_length, y = CL_residuals, color = as.factor(HARDNESS))) +
      geom_point(size = 3) +
      ggtitle(paste0("Residuals by HARDNESS, LFA: ", lfa))
    print(p_hardness)
    # ggsave(filename = paste0(lfa, "_residuals_hardness.png"), plot = p_hardness)
  }
}



##### LOOK AT THE IMMATURE/MATURE SIZE Breakdown 
for (lfa in unique(matdat$LFA)) {
  # Filter data for current LFA
  lfa_data <- matdat %>% filter(LFA == lfa)
  
  # Get size_50_upr from the results data frame for the current LFA
  size_50_upr <- results %>%
    filter(LFA == lfa) %>%
    pull(size_50_upr)
  
  # Create the plot
  p <- ggplot(lfa_data, aes(x = Carapace_length, fill = as.factor(CG_MAT))) +
    geom_histogram(
      binwidth = 1,
      color = "black",
      alpha = 0.8 ) +
    scale_fill_manual(
      values = c("0" = "lightblue", "1" = "darkblue"),
      labels = c("Immature", "Mature"),
      name = "Maturity Status") +
    # Facet by Maturity Status
    facet_wrap(~CG_MAT, labeller = as_labeller(c("0" = "Immature", "1" = "Mature"))) +
    geom_vline(
      xintercept = 82.5,
      color = "red",
      linetype = "dashed",
      size = 1 ) +
    geom_vline(
      xintercept = size_50_upr,
      color = "black",
      linetype = "dashed",
      size = 1 ) +

    labs(
      title = paste("Length Frequency by Maturity Status (LFA ", lfa, ")", sep = ""),
      x = "Carapace Length (mm)",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(fill = "gray90", color = "white"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"  # Legend not needed if facets are labeled
    )
  
  # Print each plot
  print(p)
  # ggsave(filename = paste0("LFA_", lfa, "_length_frequency_facet.png"), plot = p)
}

############look at just one LFA no maturity status

ggplot(matdat %>% dplyr::filter(LFA == "L34"), aes(x = Carapace_length)) +
  geom_histogram(
    binwidth = 1,
    color = "black",
    alpha = 0.8 ) +
  geom_vline( xintercept = 82.5,color = "red",  linetype = "dashed",  size = 1 ) +
  labs( x = "Carapace Length (mm)", y = "Frequency" ) +
  theme_minimal() +
  theme( text = element_text(size = 15),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"   )



#### MATURITY CURVES
for (lfa in unique(matdat$LFA)) {
  b <- matdat %>% filter(LFA == lfa)
  
  g <- glm(CG_MAT ~ Carapace_length, data = b, family = binomial(link = 'logit')) 
  
  plot(
    b$Carapace_length, b$CG_MAT,
    pch = 16,                                   # Solid points
    xlab = "Carapace Length (mm)",             # X-axis label
    ylab = "Proportion Mature",                # Y-axis label
    main = paste("Proportion Mature vs Carapace Length (LFA ", lfa, ")", sep = "")
  )
 
   size_50 <- results$size_50_fit[results$LFA == lfa]
  
   text(x = min(b$Carapace_length), y = max(b$CG_MAT) - 0.05, 
       labels = paste("SOM =", round(size_50, 1), "mm"),
       pos = 4, col = "black", cex = 0.9)
  
  l <- seq(min(b$Carapace_length), max(b$Carapace_length), by = 0.1)
  
  mm <- predict(g, list(Carapace_length = l), type = 'response')
  
  lines(l, mm, col = "red", lwd = 2)
  
  size_50 <- l[which.min(abs(mm - 0.5))]
  
  print(paste("LFA:", lfa, "- Size at 50% maturity:", round(size_50, 2)))
  
  ndata <- list(Carapace_length = l)          
  ndata <- glmCIs(g, ndata)                   
  
  lines(ndata$Carapace_length, ndata$fit_resp, col = "black", lwd = 2)  # Regression line
  lines(ndata$Carapace_length, ndata$upr, lty = 2, col = "darkgrey")       # Upper CI
  lines(ndata$Carapace_length, ndata$lwr, lty = 2, col = "darkgrey")       # Lower CI
  
  with(ndata, {
    size_50_fit = Carapace_length[which.min(abs(fit_resp - 0.5))]
    size_50_upr = Carapace_length[which.min(abs(upr - 0.5))]
    size_50_lwr = Carapace_length[which.min(abs(lwr - 0.5))]
    
    print(paste("LFA:", lfa))
    print(paste("50% maturity (fitted):", round(size_50_fit, 2)))
    print(paste("50% maturity (upper CI):", round(size_50_upr, 2)))
    print(paste("50% maturity (lower CI):", round(size_50_lwr, 2)))
  })
}



#### REVIEW SPATIAL DATA
matdat_sf <- matdat %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) 
#----------------------------------------------------------
# Interactive Map 
#----------------------------------------------------------

matdat_sf$popup <- paste0(
  "<b>Lobster #:</b> ", matdat_sf$LOBSTER_NUMBER, "<br>",
  "<b>MONTH:</b> ", matdat_sf$MONTH, "<br>",
  "<bYEAR:</b> ", matdat_sf$YEAR, "<br>",
  "<b>LFA:</b> ", matdat_sf$LFA, "<br>",
  "<b>Org:</b> ", matdat_sf$ORG
)

leaflet(matdat_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 4,
    color = "blue",
    fillOpacity = 0.7,
    popup = ~popup
  )


######### ILTS  Survey Stations  ######### 
#lobster.db('survey.redo')
lobster.db('survey')
###LFAs coastline
lobarea = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
lobarea <- st_make_valid(lobarea)
LS<-surveyCatch[(surveyCatch$HAULCCD_ID== 1),] ##Only Successful Tows
LS<-LS[(LS$LFA %in% c("L34","L35","L36","L37","L38")),] 

LS$HAUL_DATE <- as.Date(LS$HAUL_DATE, format = "%Y-%m-%d")
LS$MONTH <- format(LS$HAUL_DATE, "%m")
LS$YEAR <- format(LS$HAUL_DATE, "%y")

sumilts<-LS %>%
  filter(YEAR == c(25,24,23)) %>%
  distinct(SET_ID, SET_LAT, SET_LONG, LFA, STATION, MONTH,HAUL_DATE,YEAR)

ilts_sf <- sumilts%>%
  st_as_sf(coords = c("SET_LONG", "SET_LAT"), crs = 4326) 

matdat_sf34<-matdat_sf[matdat_sf$LFA =="L34",]
ilts_sf34<-ilts_sf[ilts_sf$LFA =="L34",]

##Week of survey
ilts_sf34 <- ilts_sf34 %>%
  mutate(
    week_num = ceiling(day(HAUL_DATE) / 7),
    WOSurvey = paste0("Week ", week_num) )

base_map <- ggLobsterMap(
  area = "34",
  addGrids = FALSE,
  fill.colours = "grey",
  bathy = TRUE,
  color = NA,
  colourLFA = FALSE,
  addLFAlines = TRUE)

base_map +
  geom_sf(
    data = ilts_sf34,
    aes(color = WOSurvey),
    size = 2
  ) +
  facet_wrap(~ YEAR) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

ilts_sf2025<-ilts_sf34  %>%
  filter(YEAR == c(25))



ggLobsterMap(area="34", addGrids=FALSE,
             fill.colours='grey', bathy=TRUE, color=NA,
             colourLFA=FALSE, addLFAlines=TRUE) +
  
  geom_sf(data = matdat_sf34, fill = NA, color = "red", linewidth = 0.75) +
  
  geom_sf(data = ilts_sf2025, fill = NA, color = "green", linewidth = 0.75, size =1.5) +
  
  geom_sf_text(
    data = ilts_sf2025,
    aes(label = STATION),
    size = 3.2,
    color = "black",
    nudge_y = 0.01   
  )



################### REVIEW DISTANCE FROM SHORE in LFA 34   ###################
shoreline_sf = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
matdat_sf      <- st_transform(matdat_sf, 26922)
shoreline_sf   <- st_transform(shoreline_sf, 26922)
shoreline_sf <- st_make_valid(shoreline_sf)
shoreline_sf <- st_buffer(shoreline_sf, 0) 
sf::sf_use_s2(FALSE)

dist_matrix <- st_distance(matdat_sf, shoreline_sf)
matdat_sf$distanceshore <- apply(dist_matrix, 1, min)
matdat_sf$distanceshore_km <- matdat_sf$distanceshore / 1000


matdat_sf$dist_bin <- cut(
  matdat_sf$distanceshore,
  breaks = c(0, 500, 1000, 2000, 5000, Inf),
  labels = c("0–500 m", "500–1000 m", "1–2 km", "2–5 km", ">5 km")
)



# Make a distance bin 
if (!"distanceshore" %in% names(matdat_sf)) {
  stop("distanceshore column not found. Compute it before running this section.")
}

matdat_sf <- matdat_sf %>%
  mutate(
    dist_bin = ifelse(
      distanceshore < 5000,
      "<5 km",
      "≥5 km"
    ),
    dist_bin = factor(dist_bin, levels = c("<5 km", "≥5 km"))
  )

# ---------------------------------------------------------
# 1. RESIDUALS VS DISTANCE (per LFA)
# ---------------------------------------------------------

unique_LFAs <- unique(matdat_sf$LFA)

for (lfa in unique_LFAs) {
  
  b <- matdat_sf %>% filter(LFA == lfa)
  
  # Fit the same model as before (no distance)
  g <- glm(CG_MAT ~ Carapace_length, data = b, family = binomial)
  
  # Compute residuals
  if (requireNamespace("statmod", quietly = TRUE)) {
    b$CL_residuals <- statmod::qresid(g)
  } else {
    b$CL_residuals <- residuals(g, type = "response")
  }
  
  # Plot residuals vs distance
  p_dist <- ggplot(b, aes(x = distanceshore, y = CL_residuals)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggtitle(paste("Residuals vs Distance to Shore — LFA", lfa)) +
    xlab("Distance to shore (m)") +
    ylab("Residuals")
  
  print(p_dist)
}

# ---------------------------------------------------------
# 2. MATURITY CURVES BY DISTANCE BIN (per LFA)
# ---------------------------------------------------------

for (lfa in unique_LFAs) {
  
  b <- matdat_sf %>% filter(LFA == lfa)
  
  p_curve <- ggplot(b, aes(x = Carapace_length, y = CG_MAT, color = dist_bin)) +
    geom_point(alpha = 0.3) +
    geom_smooth(
      method = "glm",
      method.args = list(family = binomial),
      se = FALSE,
      linewidth = 1.2
    ) +
    ggtitle(paste("Maturity Curves by Distance Bin — LFA", lfa)) +
    xlab("Carapace Length (mm)") +
    ylab("Proportion Mature") +
    scale_color_brewer(palette = "Spectral", na.value = "grey50")
  
  print(p_curve)
}

# ---------------------------------------------------------
# 3. MODEL COMPARISON
# ---------------------------------------------------------

model_results <- data.frame()

for (lfa in unique_LFAs) {
  
  b <- matdat_sf %>% filter(LFA == lfa)
  
  g1 <- glm(CG_MAT ~ Carapace_length, data = b, family = binomial)
  g2 <- glm(CG_MAT ~ Carapace_length + distanceshore, data = b, family = binomial)
  
  # Likelihood ratio test
  lr <- anova(g1, g2, test = "Chisq")
  
  model_results <- rbind(
    model_results,
    data.frame(
      LFA = lfa,
      AIC_no_dist = AIC(g1),
      AIC_with_dist = AIC(g2),
      LR_p_value = lr$`Pr(>Chi)`[2]
    )
  )
}

print(model_results)






samples_per_year <- matdat_sf %>%
  st_drop_geometry() %>% 
  group_by(LFA, YEAR) %>%
  summarise(
    n_samples  = n(),
    n_mature   = sum(CG_MAT == 1, na.rm = TRUE),
    n_immature = sum(CG_MAT == 0, na.rm = TRUE),
    pct_mature = n_mature / n_samples * 100
  ) %>% arrange(LFA, YEAR)

print(samples_per_year)
