library(tidyverse)

####  Import all files as character ####

path <- "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM trips"

lobster_files <- list.files(
  path = path,
  pattern = "Lobster Report\\.csv$",
  full.names = TRUE
)

# Read everything as character to avoid type conflicts
lobster_raw <- map_df(
  lobster_files,
  ~ read_csv(.x, col_types = cols(.default = "c"))
)

####  Remove fully blank rows ####

lobster_clean <- lobster_raw %>%
  mutate(across(
    where(is.character),
    ~ iconv(., from = "", to = "UTF-8", sub = "�") 
  )) %>%
  mutate(across(where(is.character), trimws)) %>%     
  filter(!if_all(everything(), ~ is.na(.) | . == "")) 

#### Make new length column  ####

# Identify all LENGTH_mm_Lab variants
length_lab_cols <- grep("^LENGTH_mm_Lab", names(lobster_clean), value = TRUE)

lobster_clean <- lobster_clean %>%
  mutate(
   
    LENGTH_mm_Lab_clean = coalesce(!!!syms(length_lab_cols)),
    LENGTH_mm_Lab_clean = na_if(LENGTH_mm_Lab_clean, "0"),
    LENGTH_mm_Lab_clean = as.numeric(LENGTH_mm_Lab_clean),
    LENGTH_mm_Boat      = as.numeric(LENGTH_mm_Boat),
        lobster_length = coalesce(LENGTH_mm_Lab_clean, LENGTH_mm_Boat)
  )


#### Remove bogus unnamed columns ####

lobster_clean <- lobster_clean %>%
  select(-starts_with("..."))

#### Standardize LFA codes ####

lobster_clean <- lobster_clean %>%
  mutate(
    LFA = case_when(
      LFA == "32"      ~ "L32",
      LFA == "LFA 32"  ~ "L32",
      LFA == "31B"     ~ "L31B",
      LFA == "34"      ~ "L34",
      LFA == "36"      ~ "L36",
      TRUE             ~ LFA
    )
  )

###Remove data where no pleopods were measured
lobster_clean <- lobster_clean %>%
  filter(!is.na(CG_STAGE))


### CHECK LENGTHS

ggplot(lobster_clean, aes(x = lobster_length)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Length Frequency Distribution",
       x = "Length (mm)",
       y = "Count")


## check Unique variables 
unique_values <- list(
  Hardness      = unique(lobster_clean$HARDNESS),
  Egg_stage     = unique(lobster_clean$EGG_STAGE),
  Pleopod       = unique(lobster_clean$PLEOPOD),
  Gastrolith    = unique(lobster_clean$GASTROLITH),
  Ovary_Colour  = unique(lobster_clean$OVARY_COLOUR),
  Ovary_Stage   = unique(lobster_clean$OVARY_STAGE),
  CG_STAGE  = unique(lobster_clean$CG_STAGE),
  LFA = unique(lobster_clean$LFA),
  TRIP_ID = unique(lobster_clean$TRIP_ID)
)

unique_values



#Gastrocheck 

lobster_clean <- lobster_clean  %>%
  mutate(GASTROLITH = na_if(GASTROLITH, "N/A"))

## CG CHECKS

lobster_clean%>%
  count(CG_STAGE, sort = TRUE)


cg_check<-cg_na_or_5 <- lobster_clean %>%
  filter(is.na(CG_STAGE) | CG_STAGE == "5")
write_csv(cg_check, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/cg_check.csv")


## Standardize LFAs


lobster_clean<- lobster_clean %>%
  mutate(
    LFA = case_when(
      LFA == "32"      ~ "L32",
      LFA == "LFA 32"  ~ "L32",
      LFA == "31B"     ~ "L31B",
      LFA == "34"      ~ "L34",
      LFA == "36"      ~ "L36",
      TRUE             ~ LFA   # keep everything else unchanged
    )
  )

##Remove bogus columns
lobster_clean <- lobster_clean %>%
  select(-starts_with("..."))



### FIRST MAKE SURE ALL POINTS IN SAME FORMAT


# Function to show unique patterns
check_formats <- function(x) {
  tibble(
    raw = x,
    pattern = str_replace_all(x, "[0-9]", "N")   # replace digits with N to reveal structure
  ) %>%
    distinct() %>%
    arrange(pattern)
}

# LAT
check_formats(lobster_clean$LAT)
# LONG
check_formats(lobster_clean$LONG)
# LAT_FINISH
check_formats(lobster_clean$LAT_FINISH)
# LON_FINISH
check_formats(lobster_clean$LON_FINISH)




lobster_clean <- lobster_clean %>%
  mutate(
    LONG = as.numeric(LONG),
    LON_FINISH = as.numeric(LON_FINISH),
    LONG = if_else(!is.na(LONG) & LONG > 0, -LONG, LONG),
    LON_FINISH = if_else(!is.na(LON_FINISH) & LON_FINISH > 0, -LON_FINISH, LON_FINISH)
  )

##Plot positions

lobster_clean %>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG)
  ) %>%
  ggplot(aes(x = LONG, y = LAT)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "All LAT vs LONG Values",
    x = "Longitude",
    y = "Latitude"
  )

coords <- lobster_clean%>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG)
  )

coords %>%
  mutate(
    lat_dev = abs(LAT - median(LAT, na.rm = TRUE)),
    lon_dev = abs(LONG - median(LONG, na.rm = TRUE)),
    total_dev = lat_dev + lon_dev
  ) %>%
  arrange(desc(total_dev)) %>%
  slice(1)

latcheck<-lobster_clean%>%
  filter(as.numeric(LAT) > 48)

## Make all positoins same format

convert_ddmm_to_dd <- function(x) {
  x <- as.numeric(x)
  deg <- floor(x / 100)
  minutes <- x - deg * 100
  deg + minutes / 60
}


lobster_clean <- lobster_clean%>%
  mutate(
    LAT = convert_ddmm_to_dd(LAT),
    LONG = -abs(convert_ddmm_to_dd(LONG))   # ensure longitude is negative
  )

### CHECKING UNLIKELIES 
under_75_stage2 <- lobster_clean %>%
  filter(
    lobster_length  < 75,
    CG_STAGE == "2"
  )

write_csv(under_75_stage2, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/under_75_stage2.csv")


over_90_stage1 <- lobster_clean %>%
  filter(
    lobster_length  > 90,
    CG_STAGE == "1"
  )
write_csv(over_90_stage1, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/over_90_stage1.csv")



write_csv(lobster_clean, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM trips/som_2025_cleaned.csv")
