## Reviewing SOM Data

#### Curves for SOM ####
#maturity modelling
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)


#Import all data sources

mat_dfo = read.csv('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity/matClean.csv')
mat_new = read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles/SOM trips/som_2025_cleaned.csv")



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



mat_new_clean <- mat_new_clean %>%
  mutate(
    Carapace_mm = as.numeric(Carapace_mm)
  )

mat_dfo_clean <- mat_dfo_clean %>%
  mutate(LFA = paste0("L", as.character(LFA)))



mat_all <- bind_rows(
  mat_dfo_clean,
  mat_new_clean
)
