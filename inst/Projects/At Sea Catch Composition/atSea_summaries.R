###
require(sf)
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(ggplot2)
require(dplyr)
require(PBSmapping)


la()
p=list()
p$libs = NULL
la()
p$yrs = 1947:2026
load_all('~/GitHub/bio.survey/')
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests")


#lobster.db('atSea.redo')
lobster.db('atSea')
#lobster.db('season.dates.redo')
sd= lobster.db('season.dates')
#lobster.db('atSea.clean.redo') PBS mapping can't handle the amount of data here anymore
aS <- atSea
aS = subset(aS,LFA %in% c(33,34,35,36,38) & !is.na(SPECIESCODE) & STARTDATE>as.Date('2000-01-01'),
           select=c(TRIPNO,DESCRIPTION,STARTDATE,LFA,LICENCE_ID,TRAPNO,TRAPTYPE,STRINGNO,DEPTH, SOAKDAYS, SPECIESCODE, SPECIES, SEX, SHELL, CARLENGTH, CONDITION,CALWT)) 
aS$YR=year(aS$STARTDATE)
aS=subset(aS, DESCRIPTION %ni% c('EA-MINAS','Data not collected by trap'))

aS$STARTDATE <- as.Date(aS$STARTDATE)
aS2 <- addSYEAR(aS)



aS2= subset(aS2, SPECIESCODE == '2550')
aS2<- aS2[rowSums(!is.na(aS2)) > 0, ]

keep_types <- c(1, 2, 3, 7,22,-99,NA)
aS2<- aS2[aS2$TRAPTYPE %in% keep_types, ]
dim(aS2)


### CHECk Lengths
sum(aS2$CARLENGTH < 82, na.rm = TRUE)
sum(aS2$CARLENGTH >= 82 & aS2$CARLENGTH <= 83, na.rm = TRUE)
sum(aS2$CARLENGTH > 83, na.rm = TRUE)

aS2 <- aS2 %>%
  mutate(UID = paste0(TRIPNO, "_", TRAPNO))

aS2 <- aS2 %>%
  mutate(
    commercial = case_when(
      CARLENGTH >= 83 ~ "legal",
      CARLENGTH < 82  ~ "sublegal",
      TRUE ~ NA_character_   # placeholder for 82–83 group
    )
  ) %>%
  group_by(UID) %>%
  mutate(
    #  82–83 animals
    mid_group = (CARLENGTH >= 82 & CARLENGTH < 83),
    
    # within UID  split 50/50
    rank_mid = ifelse(mid_group, row_number(), NA),
    
    # Count how many mid-group animals per UID
    n_mid = sum(mid_group, na.rm = TRUE),
    
    # Assign half to legal, half to sublegal
    commercial = case_when(
      !mid_group ~ commercial,  # keep existing assignments
      rank_mid <= n_mid / 2 ~ "sublegal",
      TRUE ~ "legal"
    )
  ) %>%
  ungroup() %>%
  select(-mid_group, -rank_mid, -n_mid)

sum(aS2$commercial == 'legal', na.rm = TRUE)
sum(aS2$commercial == 'sublegal', na.rm = TRUE)

##### LFAs 33-35 from 2018-2025 #####
df_33_35 <- aS2 %>%
  filter(LFA %in% c("33", "34", "35"),
         SYEAR >= 2018,
         SYEAR <= 2026)
df_33_35<-as.data.frame(df_33_35)

legal <- subset(df_33_35, commercial == "legal")
total_legal <- aggregate(
  CARLENGTH ~ SYEAR + LFA,
  data = legal,
  FUN = length)
names(total_legal)[3] <- "total_legal"

legal_140 <- aggregate(
  CARLENGTH ~ SYEAR + LFA,
  data = subset(legal, CARLENGTH >= 140),
  FUN = length)
names(legal_140)[3] <- "legal_140plus"

catch_summary33_35 <- merge(total_legal, legal_140, by = c("SYEAR", "LFA"), all.x = TRUE)

catch_summary33_35$legal_140plus[is.na(catch_summary33_35$legal_140plus)] <- 0

catch_summary33_35$pct_140plus <-
  100 * catch_summary33_35$legal_140plus / catch_summary33_35$total_legal

write.csv(catch_summary33_35,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/catch_summary33_35.csv" )



ggplot(catch_summary33_35, aes(x = SYEAR, y = pct_140plus)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black") +
  facet_wrap(~ LFA, scales = "free_y") +
  labs(
    x = "Season Year",
    y = "Percent ≥ 140 mm"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )



### ALL YEARS TOGETHER
legal <- subset(df_33_35, commercial == "legal")

total_legal_all <- aggregate(
  CARLENGTH ~ LFA,
  data = legal,
  FUN = length)
names(total_legal_all)[2] <- "total_legal"


legal_140_all <- aggregate(
  CARLENGTH ~ LFA,
  data = subset(legal, CARLENGTH >= 140),
  FUN = length)
names(legal_140_all)[2] <- "legal_140plus"

# Merge
catch_summary33_35_all <- merge(
  total_legal_all,
  legal_140_all,
  by = "LFA",
  all.x = TRUE)

# Percent ≥140
catch_summary33_35_all$pct_140plus <-
  100 * catch_summary33_35_all$legal_140plus /catch_summary33_35_all$total_legal
write.csv(catch_summary33_35_all,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/catch_summary33_35_all.csv" )





############# LFAs 36-38 from 2000-2025 ##############
df_36_38 <- aS2 %>%
  filter(LFA %in% c("36", "38"),
         SYEAR >= 2000,
         SYEAR <= 2026)
df_36_38<-as.data.frame(df_36_38)


legal <- subset(df_36_38, commercial == "legal")
total_legal <- aggregate(
  CARLENGTH ~ SYEAR + LFA,
  data = legal,
  FUN = length)
names(total_legal)[3] <- "total_legal"

legal_140 <- aggregate(
  CARLENGTH ~ SYEAR + LFA,
  data = subset(legal, CARLENGTH >= 140),
  FUN = length)
names(legal_140)[3] <- "legal_140plus"

catch_summary36_38 <- merge(total_legal, legal_140, by = c("SYEAR", "LFA"), all.x = TRUE)

catch_summary36_38$legal_140plus[is.na(catch_summary36_38$legal_140plus)] <- 0

catch_summary36_38$pct_140plus <-
  100 * catch_summary36_38$legal_140plus / catch_summary36_38$total_legal

write.csv(catch_summary36_38,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/catch_summary36_38.csv" )



ggplot(catch_summary36_38, aes(x = SYEAR, y = pct_140plus)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black") +
  facet_wrap(~ LFA, scales = "free_y") +
  labs(
    x = "Season Year",
    y = "Percent ≥ 140 mm"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )




### ALL YEARS TOGETHER
legal <- subset(df_36_38, commercial == "legal")

total_legal_all <- aggregate(
  CARLENGTH ~ LFA,
  data = legal,
  FUN = length)
names(total_legal_all)[2] <- "total_legal"


legal_140_all <- aggregate(
  CARLENGTH ~ LFA,
  data = subset(legal, CARLENGTH >= 140),
  FUN = length)
names(legal_140_all)[2] <- "legal_140plus"

# Merge
catch_summary36_38_all <- merge(
  total_legal_all,
  legal_140_all,
  by = "LFA",
  all.x = TRUE)

# Percent ≥140
catch_summary36_38_all$pct_140plus <-
  100 * catch_summary36_38_all$legal_140plus /catch_summary36_38_all$total_legal

write.csv(catch_summary36_38_all,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/catch_summary36_38_all.csv" )










#####review length frequencies


plot_length_freq <- function(df, LFA_code) {
  
  df_sub <- df[df$LFA == LFA_code, ]
  
  ggplot(df_sub, aes(x = CARLENGTH)) +
    geom_histogram(bins = 40, fill = "steelblue", color = "black") +
    facet_wrap(~ YR, scales = "free_y") +
    labs(
      title = paste("LFA", LFA_code),
      x = "Carapace Length (mm)",
      y = "Frequency"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
    )
}

plot_length_freq(df_33_35, "33")
plot_length_freq(df_33_35, "34")
plot_length_freq(df_33_35, "35")
plot_length_freq(df_36_38, "36")
plot_length_freq(df_36_38, "38")



#### Caveats in Data Collection - trap types ####


recode_traptype <- function(x) {
  x2 <- x
  
  # Convert numeric to character (if needed)
  x2 <- as.character(x2)
  
  # Apply rules
  x2[x2 == "1"]  <- "Wood"
  x2[x2 == "2"]  <- "Wire"
  x2[x2 == "3"]  <- "Wood and Wire"
  x2[x2 == "7"]  <- "Offshore Trap (Wire)  "
  x2[x2 == "22"] <- "Wire"

  # Unknown cases
  x2[x2 %in% c("NA", NA,  "-99")] <- "Not Recorded"
  
  return(x2)
}

df_33_35$TRAPTYPE <- recode_traptype(df_33_35$TRAPTYPE)
df_36_38$TRAPTYPE <- recode_traptype(df_36_38$TRAPTYPE)

table_33_35 <- aggregate(
  cbind(DESCRIPTION, TRAPTYPE) ~ LFA + SYEAR,
  data = df_33_35,
  FUN = function(x) paste(sort(unique(x)), collapse = ", ")
)

write.csv(table_33_35 ,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/sources3335.csv" )


table_36_38 <- aggregate(
  cbind(DESCRIPTION, TRAPTYPE) ~ LFA + SYEAR,
  data = df_36_38,
  FUN = function(x) paste(sort(unique(x)), collapse = ", ")
)

write.csv(table_36_38 ,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/sources368.csv" )

