
#INSERT SPECIES CODE AND COMMON NAME (ALL CAPS)

spcd<- 2550

sp<- "LOBSTER"

# List of packages for session
.packages = c("dplyr",
              "lubridate",
              "sf",
              "tidyr",
              "flextable",
              "data.table",
              "ggplot2",
              "ftExtra",
              "janitor",
              "rgdal",
              "terra",
              "patchwork",
              "viridis",
              "ROracle",
              "knitr",
              "tinytex",
              "kableExtra",
              "haven",
              "gridExtra",
              "marmap", 
              "rnaturalearth",
              "rnaturalearthdata",
              "ggspatial")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


#import functions from marscal
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
#  "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
)
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

un.ID <- "HOWSEV"
pw.ID <- "Hilltop16"

#INSERT SPECIES CODE AND COMMON NAME (ALL CAPS)
spcd<- 2550
sp<- "LOBSTER"
##Data pull

con <- dbConnect(DBI::dbDriver("Oracle"), un.ID, pw.ID, 'ptran')
bycatch <- dbGetQuery(con, (" SELECT * from SCALLSUR.SCBYCATCH_STD"))

tows <- dbGetQuery(con, ("SELECT TOW_SEQ, CRUISE, TOW_NO, TOW_DATE, TOW_TYPE_ID, STRATA_ID, START_LAT, START_LONG, TOW_DIR, TOW_LEN, TOW_LEN_ID, DEPTH, BOTTOM_TEMP, BOTTOM_ID, MGT_AREA_ID from SCALLSUR.SCTOWS"))

#formatting of bycatch data
bycatch$COMMON[bycatch$COMMON == "BATHYPOLYPUS ARCTICUS"] <- "NORTH ATLANTIC OCTOPUS"
bycatch$COMMON[bycatch$COMMON == "SEMIROSSIA TENERA"] <- "LESSER BOBTAIL SQUID"
bycatch$COMMON[bycatch$COMMON == "BRILL/WINDOWPANE"] <- "WINDOWPANE FLOUNDER"
bycatch$COMMON[bycatch$COMMON == "AHLIA EGMONTIS"] <- "KEY WORM EEL"
bycatch$COMMON[bycatch$COMMON == "LEUCORAJA <35cm"] <- "LITTLE OR WINTER SKATE under 35cm"

bycatch<-bycatch %>% 
  dplyr::filter(SPECCD_ID == spcd)

bycatch$SLAT<-convert.dd.dddd(bycatch$START_LAT,format='dec.deg')
bycatch$SLONG<-convert.dd.dddd(bycatch$START_LON,format='dec.deg')
bycatch$ELAT<-convert.dd.dddd(bycatch$END_LAT,format='dec.deg')
bycatch$ELONG<-convert.dd.dddd(bycatch$END_LON,format='dec.deg')

#format date to allow to select data by year
bycatch$SEX_ID<- as.character(bycatch$SEX_ID) 
bycatch$TOW_DATE<- as.Date(bycatch$TOW_DATE)
bycatch <- bycatch %>%
  dplyr::mutate(year = lubridate::year(TOW_DATE), 
                month = lubridate::month(TOW_DATE), 
                day = lubridate::day(TOW_DATE))
bycatch$year<-as.character(bycatch$year)
bycatch$SEX_ID<-as.character(bycatch$SEX_ID)

bycatch$SEX_ID[bycatch$SEX_ID == "0"] <- "Not sexed"
bycatch$SEX_ID[bycatch$SEX_ID == "1"] <- "Male"
bycatch$SEX_ID[bycatch$SEX_ID == "2"] <- "Female"
bycatch$SEX_ID[bycatch$SEX_ID == "3"] <- "Berried Female"

#species = unique(bycatch$COMMON)  #to get a list of all species encountered 

#format tow data
tows$TOW_DATE<- as.Date(tows$TOW_DATE)
tows <- tows %>%
  dplyr::mutate(year = lubridate::year(TOW_DATE), 
                month = lubridate::month(TOW_DATE), 
                day = lubridate::day(TOW_DATE))

tows$year<-as.character(tows$year)
tows$SLAT<-convert.dd.dddd(tows$START_LAT,format='dec.deg')
tows$SLONG<-convert.dd.dddd(tows$START_LON,format='dec.deg')
tows<- tows %>% 
  dplyr::select(TOW_SEQ, CRUISE, TOW_NO, MGT_AREA_ID, year, month, day, SLAT, SLONG, DEPTH, TOW_DIR, TOW_LEN, BOTTOM_TEMP,)

#to set up loop for each species
#species = species[order(species)]
#species_data = list()
yrs = as.character(unique(bycatch$year))
yrs = yrs[order(yrs)]


## Normalizing data so tows where lobster were present
lobster_tows<-bycatch %>% 
  dplyr::select(TOW_SEQ, CRUISE, MGT_AREA_ID, TOW_NO, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_ID, TOTAL_SAMPLED_GEAR, ABUNDANCE_RAW, ABUNDANCE_STD, SLAT, SLONG, year, month, day) %>% 
  group_by(TOW_SEQ) %>% 
  dplyr::summarise(abun_raw = sum(ABUNDANCE_RAW), abun_std = sum(ABUNDANCE_STD))

### When abund_raw = 0 , no lobster were found, and when >0 then lobster present
## Tow_Seq is unique tows
lobster_tows_data<- tows %>% 
 left_join(y=lobster_tows , by = c("TOW_SEQ")) %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) #%>%  #Assumes all NAs are 0



#bycatch_by_tow<-bycatch %>% 
#  dplyr::select(TOW_SEQ, CRUISE, MGT_AREA_ID, TOW_NO, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_ID, TOTAL_SAMPLED_GEAR, ABUNDANCE_RAW, ABUNDANCE_STD, SLAT, SLONG, year, month, day) %>% 
#  group_by(TOW_SEQ, year, CRUISE, TOW_NO, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_ID, TOTAL_SAMPLED_GEAR) %>% 
#  dplyr::summarise(abun_raw = sum(ABUNDANCE_RAW), abun_std = sum(ABUNDANCE_STD))



#species_data<- bycatch_by_tow %>% 
 # right_join(y= tows, by = c("TOW_SEQ", "year", "CRUISE", "TOW_NO")) %>% 
 # mutate(across(where(is.numeric), ~ replace_na(.x, 0))) #%>%  #Assumes all NAs are 0