###
require(sf)
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(ggplot2)
require(dplyr)

la()
p=list()
p$libs = NULL
la()
p$yrs = 1947:2026
load_all('~/GitHub/bio.survey/')
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests")


a=lobster.db('atSea.redo')
b=lobster.db('season.dates.redo')
c=lobster.db('atSea.clean.redo')

a = lobster.db('atSea.clean') ## If you want to switch to Season year instead
a = subset(a,LFA %in% c(33,34,35,36,38) & !is.na(SPECIESCODE) & STARTDATE>as.Date('2000-01-01'),
           select=c(TRIPNO,DESCRIPTION,STARTDATE,LFA,LICENCE_ID,TRAPNO,TRAPTYPE,STRINGNO,DEPTH, SOAKDAYS, SPECIESCODE, SPECIES, SEX, SHELL, CARLENGTH, CONDITION,CALWT, SYEAR)) 
a$YR=year(a$STARTDATE)
a=subset(a, DESCRIPTION %ni% c('EA-MINAS','Data not collected by trap'))


a= subset(a, SPECIESCODE == '2550')
a<- a[rowSums(!is.na(a)) > 0, ]

keep_types <- c(1, 2, 3, 7,22,-99,NA)
a <- a[a$TRAPTYPE %in% keep_types, ]
dim(a)



a_sub <- a %>%
  group_by(TRIPNO, TRAPNO) %>% 
  mutate(
    # Identify categories
    keep_group = case_when(
      CARLENGTH > 82 ~ "keep_all",
      CARLENGTH >= 82 & CARLENGTH <= 83 ~ "sample_half",
      TRUE ~ "drop"
    )
  ) %>%
  # For the sample_half group, take 50% of rows per TRIPNO+TRAPNO
  group_modify(~ {
    df <- .x
    keep_all <- df %>% filter(keep_group == "keep_all")
    sample_half <- df %>% filter(keep_group == "sample_half")
    
    # sample half of them (floor ensures no over-sampling)
    n_half <- floor(nrow(sample_half) / 2)
    
    sampled <- sample_half %>% slice_sample(n = n_half)
    
    bind_rows(keep_all, sampled)
  }) %>%
  ungroup() %>%
  select(-keep_group)

dim(a_sub)


# Create a grouping factor for TRIPNO + TRAPNO
grp <- interaction(a$TRIPNO, a$TRAPNO, drop = TRUE)

a_list <- split(a, grp)

process_group <- function(df) {
  # Keep all >82
  keep_all <- df[df$CARLENGTH >= 83, ]
  
  # Identify 82–83 animals
  mid <- df[df$CARLENGTH >= 82 & df$CARLENGTH <83, ]
  
  # Sample half of them
  if (nrow(mid) > 0) {
    n_half <- floor(nrow(mid) / 2)
    mid_sample <- mid[sample(seq_len(nrow(mid)), n_half), ]
  } else {
    mid_sample <- df[FALSE, ]  # empty
  }
  
  rbind(keep_all, mid_sample)
}
a_sub <- do.call(rbind, lapply(a_list, process_group))
rownames(a_sub) <- NULL




### LFAs 33-35 from 2018-2025
df_33_35 <- a %>%
  filter(LFA %in% c("33", "34", "35"),
         YR >= 2018,
         YR <= 2026)

tot_33_35 <- aggregate(CARLENGTH ~ LFA + YR, data = df_33_35, FUN = length)
over_140_33_35 <- aggregate(CARLENGTH ~ LFA + YR, data = df_33_35,
                            FUN = function(x) sum(x >= 140, na.rm = TRUE))
pct_33_35 <- merge(tot_33_35, over_140_33_35,
                   by = c("LFA", "YR"),
                   suffixes = c("_total", "_over140"))
pct_33_35$pct_over140 <- 100 * pct_33_35$CARLENGTH_over140 / pct_33_35$CARLENGTH_total

pct_33_35$LFA <- as.factor(pct_33_35$LFA)
pct_33_35$YR  <- as.numeric(as.character(pct_33_35$YR))
pct_33_35$Percent_Rounded <- round(pct_33_35$pct_over140, 2)

write.csv(pct_33_35,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/Jumbos33_35.csv" )

ggplot(pct_33_35, aes(x = YR, y = pct_over140)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black") +
  facet_wrap(~ LFA, scales = "free_y") +
  labs(
   #title = "LFAs 33–35",
    x = "Year",
    y = "Percent ≥ 140 mm"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )


### LFAs 36-38 from 2000-2025
df_36_38 <- a %>%
  filter(LFA %in% c("36", "38"),
         YR >= 2000,
         YR <= 2026)


tot_36_38 <- aggregate(CARLENGTH ~ LFA + YR, data = df_36_38, FUN = length)

over_140_36_38 <- aggregate(CARLENGTH ~ LFA + YR, data = df_36_38,
                            FUN = function(x) sum(x >= 140, na.rm = TRUE))
pct_36_38 <- merge(tot_36_38, over_140_36_38,
                   by = c("LFA", "YR"),
                   suffixes = c("_total", "_over140"))
pct_36_38$pct_over140 <- 100 * pct_36_38$CARLENGTH_over140 / pct_36_38$CARLENGTH_total

pct_36_38$LFA <- as.factor(pct_36_38$LFA)
pct_36_38$YR  <- as.numeric(as.character(pct_36_38$YR))
pct_36_38$Percent_Rounded <- round(pct_36_38$pct_over140, 2)

write.csv(pct_36_38,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/Jumbos36_38.csv" )


ggplot(pct_36_38, aes(x = YR, y = pct_over140)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black") +
  facet_wrap(~ LFA, scales = "free_y") +
  labs(
    #title = "LFAs 36 & 38",
    x = "Year",
    y = "Percent ≥ 140 mm"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )


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
  cbind(DESCRIPTION, TRAPTYPE) ~ LFA + YR,
  data = df_33_35,
  FUN = function(x) paste(sort(unique(x)), collapse = ", ")
)

write.csv(table_33_35 ,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/sources3335.csv" )


table_36_38 <- aggregate(
  cbind(DESCRIPTION, TRAPTYPE) ~ LFA + YR,
  data = df_36_38,
  FUN = function(x) paste(sort(unique(x)), collapse = ", ")
)

write.csv(table_36_38 ,"C:/Users/HowseVJ/OneDrive - DFO-MPO/Bycatch Review/requests/sources368.csv" )

