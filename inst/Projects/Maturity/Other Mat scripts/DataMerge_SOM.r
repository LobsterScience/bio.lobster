
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

####  Import all files as character ####

path <- "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM Files R"

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
lab_cols <- grep("^LENGTH_mm_Lab", names(lobster_clean), value = TRUE)
lab_cols


lobster_clean <- lobster_clean %>%
  mutate(
    LENGTH_mm_Lab = coalesce(!!!syms(lab_cols)),
    LENGTH_mm_Lab = na_if(LENGTH_mm_Lab, "0"),
    LENGTH_mm_Lab = as.numeric(LENGTH_mm_Lab)  )

lab_cols <- grep("^LENGTH_mm_Lab", names(lobster_clean), value = TRUE)
lab_cols

all_same <- lobster_clean %>%
  select(all_of(lab_cols)) %>%
  mutate(across(everything(), ~na_if(as.character(.x), "0"))) %>%
  { rowSums(!is.na(.)) == 0 | apply(., 1, function(x) length(unique(na.omit(x))) == 1) } %>%
  all()
all_same

#remove bad cols
lobster_clean <- lobster_clean %>%
  select(-matches("^LENGTH_mm_Lab\\.\\.\\."))



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



###Carapace_length

lobster_clean <- lobster_clean %>%
  mutate(
    LENGTH_mm_Boat = as.numeric(LENGTH_mm_Boat),
    Carapace_length = coalesce(LENGTH_mm_Lab, LENGTH_mm_Boat)
  )

lobster_clean <- lobster_clean %>%
  mutate(Carapace_length = round(Carapace_length))


### CHECK LENGTHS

ggplot(lobster_clean, aes(x = Carapace_length)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(
    title = "Length Frequency Distribution",
    x = "Length (mm)",
    y = "Count")



oddLength<-lobster_clean %>%
  mutate(Carapace_length = as.numeric(Carapace_length)) %>%
  filter(Carapace_length < 40 | Carapace_length > 140)

write_csv(oddLength, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/checkLengths.csv")



### Address LAT / LONG inconsistencies 
lobster_clean <- lobster_clean %>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG) )

lobster_clean <- lobster_clean %>%
  mutate(
    LAT = ifelse(LAT > 90,
                 floor(LAT / 100) + (LAT %% 100) / 60,
                 LAT),
    LONG = ifelse(LONG > 180,
                  -(floor(LONG / 100) + (LONG %% 100) / 60),
                  LONG)  )


##Map to check 
#----------------------------------------------------------
#  Remove rows with missing coordinates
#----------------------------------------------------------

lobster_sf <- lobster_clean %>%
  filter(!is.na(LAT), !is.na(LONG))
#----------------------------------------------------------
#  Create sf object
#----------------------------------------------------------
lobster_sf <-lobster_sf %>%
  select(LAT, LONG, LFA, SAMPLE_DATE, TRIP_ID, ORG, LOBSTER_NUMBER) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326)

#----------------------------------------------------------
# Build popup text including LOBSTER_NUMBER
#----------------------------------------------------------

lobster_sf$popup <- paste0(
  "<b>Lobster #:</b> ", lobster_sf$LOBSTER_NUMBER, "<br>",
  "<b>Trip ID:</b> ", lobster_sf$TRIP_ID, "<br>",
  "<b>Date:</b> ", lobster_sf$SAMPLE_DATE, "<br>",
  "<b>LFA:</b> ", lobster_sf$LFA, "<br>",
  "<b>Org:</b> ", lobster_sf$ORG
)

#----------------------------------------------------------
# Interactive leaflet map
#----------------------------------------------------------

leaflet(lobster_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 4,
    color = "blue",
    fillOpacity = 0.7,
    popup = ~popup
  )


ggLobsterMap(area="all" ,addGrids=F,
    fill.colours = 'grey',bathy=T,color=NA,colourLFA = F,addLFAlines=T)+
  geom_sf(data=lobster_sf, fill=NA,color="red",linewidth=0.75)

##Remove rows that are all NA

checkNA<-lobster_clean %>%
  filter(if_all(everything(), is.na))

lobster_clean <- lobster_clean %>%
  filter(!if_all(everything(), is.na))


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


### CHECKING UNLIKELIES 
under_75_stage2 <- lobster_clean %>%
  filter(
    Carapace_length  < 75,
    CG_STAGE == "2"
  )

write_csv(under_75_stage2, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/under_75_stage2.csv")


over_90_stage1 <- lobster_clean %>%
  filter(
    Carapace_length   > 90,
    CG_STAGE == "1"
  )
write_csv(over_90_stage1, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/over_90_stage1.csv")


### Ovary_Colour naming 

lobster_clean <- lobster_clean %>%
  mutate(
    OVARY_COLOUR = str_trim(OVARY_COLOUR),        # remove extra spaces
    OVARY_COLOUR = str_to_lower(OVARY_COLOUR),    # normalize for matching
    OVARY_COLOUR = case_when(
      OVARY_COLOUR %in% c("white") ~ "White",
      OVARY_COLOUR %in% c("med green", "medium green") ~ "Medium Green",
      OVARY_COLOUR %in% c("light green") ~ "Light Green",
      OVARY_COLOUR %in% c("dark green") ~ "Dark Green",
      OVARY_COLOUR %in% c("pale yellow") ~ "Pale Yellow",
      OVARY_COLOUR %in% c("beige") ~ "Beige",
      OVARY_COLOUR %in% c("olive") ~ "Olive",
      OVARY_COLOUR %in% c("yellow") ~ "Yellow",
      OVARY_COLOUR %in% c("n/a", "") ~ NA_character_,
      TRUE ~ OVARY_COLOUR
    )
  )

wrongcol<-lobster_clean %>%
  filter(str_trim(OVARY_COLOUR) %in% c("b", ",", "NA"))
write_csv(wrongcol, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/wrongcol.csv")


### ovary_Stage

lobster_clean <- lobster_clean %>%
  mutate(
    OVARY_STAGE   = na_if(OVARY_STAGE  , "N/A")
  )

Ostage <- lobster_clean %>%
  filter(OVARY_STAGE %in% c( "5 or 6"))
write_csv(Ostage, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM to check/OvaryStage.csv")



############ ADD IN LFA 36 Data ############

lob_36<-read_csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM Files R/LFA36_FN2025.csv")

# remove all-NA columns
lob_36 <- lob_36 %>% select(where(~ !all(is.na(.x))))

lab_cols <- grep("^LENGTH_mm_Lab\\.\\.\\.", names(lob_36), value = TRUE)
lab_cols
match_check <- lob_36 %>%
  mutate(
    col1 = .[[lab_cols[1]]],
    col2 = .[[lab_cols[2]]],
    same = col1 == col2 | (is.na(col1) & is.na(col2))
  ) %>%
  summarise(all_match = all(same, na.rm = TRUE))

match_check

lob_36 <- lob_36 %>%
  mutate(across(all_of(lab_cols), as.character)) %>%
  mutate(
    LENGTH_mm_Lab = coalesce(!!!syms(lab_cols)),
    LENGTH_mm_Lab = na_if(LENGTH_mm_Lab, "0"),
    LENGTH_mm_Lab = as.numeric(LENGTH_mm_Lab)
  )

lob_36 <- lob_36 %>%
  select(-all_of(lab_cols))

lob_36 <- lob_36 %>%
  filter(!if_all(everything(), is.na))

### CHECK LENGTHS

ggplot(lob_36, aes(x = LENGTH_mm_Lab)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(
    title = "Length Frequency Distribution",
    x = "Length (mm)",
    y = "Count")

lob_36$Carapace_length<-lob_36$LENGTH_mm_Lab


##### CHECK VALUES ####

unique_values <- list(
  Hardness      = unique(lob_36$HARDNESS),
  Gastrolith    = unique(lob_36$GASTROLITH),
  Ovary_Colour  = unique(lob_36$OVARY_COLOUR),
  Ovary_Stage   = unique(lob_36$OVARY_STAGE),
  CG_STAGE  = unique(lob_36$CG_STAGE),
  LFA = unique(lob_36$LFA),
  TRIP_ID = unique(lob_36$TRIP_ID)
)
unique_values


### Ovary Colour
lob_36 <- lob_36 %>%
  mutate(
    OVARY_COLOUR = str_trim(OVARY_COLOUR),
    OVARY_COLOUR = str_to_lower(OVARY_COLOUR),
    OVARY_COLOUR = case_when(
      OVARY_COLOUR == "white" ~ "White",
      OVARY_COLOUR == "medium green" ~ "Medium Green",
      OVARY_COLOUR == "light green" ~ "Light Green",
      OVARY_COLOUR == "dark green" ~ "Dark Green",
      OVARY_COLOUR == "pale yellow" ~ "Pale Yellow",
      OVARY_COLOUR == "beige" ~ "Beige",
      OVARY_COLOUR == "olive" ~ "Olive",
      OVARY_COLOUR %in% c("n/a", "") ~ NA_character_,
      TRUE ~ OVARY_COLOUR   # fallback (shouldn't be needed)
    )
  )



####FORMAT LFA 36 to fit the other data
missing_cols <- setdiff(names(lobster_clean), names(lob_36))
lob_36[missing_cols] <- NA
lob_36 <- lob_36[names(lobster_clean)]

combined <- rbind(lobster_clean, lob_36)

##Check Values


## check Unique variables 
unique_values <- list(
  Hardness      = unique(combined$HARDNESS),
  Egg_stage     = unique(combined$EGG_STAGE),
  Pleopod       = unique(combined$PLEOPOD),
  Gastrolith    = unique(combined$GASTROLITH),
  Ovary_Colour  = unique(combined$OVARY_COLOUR),
  Ovary_Stage   = unique(combined$OVARY_STAGE),
  CG_STAGE  = unique(combined$CG_STAGE),
  LFA = unique(combined$LFA),
  TRIP_ID = unique(combined$TRIP_ID)
)
unique_values


combined$LFA[combined$LFA == "36"] <- "L36"


str(combined)

cols_to_numeric <- c(
  "SAMPLE_DATE",
  "LAB_DATE",
  "GRID",
  "DEPTH",
  "LOBSTER_NUMBER",
  "HARDNESS",
  "EGG_STAGE",
  "PLEOPOD",
  "WHOLE_LOBSTER",
  "LOBSTER_WEIGHT_g",
  "GASTROLITH",
  "OVARY_STAGE",
  "YELLOW_SPOTS",
  "OVARY_WEIGHT_g",
  "SPERM_PLUG",
  "OVARIAN_FACTOR",
  "CG_STAGE",
  "MOULT_STAGE",
  "LENGTH_mm_Lab",
  "Carapace_length"
)

combined[cols_to_numeric] <- lapply(combined[cols_to_numeric], function(x) {
  suppressWarnings(as.numeric(x))
})


### CHeck the ranges of each column
sapply(combined, function(col) {
  if (is.numeric(col)) {
    range(col, na.rm = TRUE)
  } else {
    c(min = NA, max = NA)
  }
})


##Make New DateColumn since Lab/Sample Vary 
combined <- combined %>%
  mutate(USE_DATE = coalesce(SAMPLE_DATE, LAB_DATE))

# Check the results
range(combined$USE_DATE) 
unique(combined$USE_DATE)


convert_date <- function(x) {
  x <- as.character(x)
    x <- sprintf("%06s", x)
  
  # Extract day, month, and year parts
  day <- substr(x, 1, 2)  
  month <- substr(x, 3, 4)  
  year <- substr(x, 5, 6)  
  parsed_date <- as.Date(paste0(day, "-", month, "-20", year), format = "%d-%m-%Y")
  return(parsed_date)
}
 
combined$USE_DATE <- convert_date(combined$USE_DATE)
range(combined$USE_DATE) 
unique(combined$USE_DATE)

combined<-as.data.frame(combined)

### Write CSV 

write_csv(combined, "C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM Files R/combineddata2025.csv")
