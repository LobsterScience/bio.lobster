##Summarizing Scallop Survey Data

########### This Chunk pulls out all of the bycatch data by Tow, without subsetting the tows by lobster presence ################
require(bio.lobster)
require(devtools)
require(bio.utilities)
require(dplyr)
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
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #  ,"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
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
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

all_tows_data = subset(lobster_tows_data,year>=1999)  ## only take the years we use for abundance indicator

#######Pair position of Tow start to LFA polygons to summarize by Year and LFA #########

#polygons LFA
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

rL = read.csv(file.path(layerDir,"Polygons_LFA.csv"))
rL$label=rL$LFA

rdef=Mar.utils::df_to_sf(rL,lat.field = "Y",lon.field = "X") 
rdef<-merge(rdef,unique(rL[,c("PID", "LFA")]), all.x=T, by="PID")

##Convert dataframe to Sf
all_tows_sf <- st_as_sf(all_tows_data , coords = c("SLONG", "SLAT"), crs = 4326)
all_tows_sf$SLONG <- all_tows_data$SLONG
all_tows_sf$SLAT <- all_tows_data$SLAT
# Check same coordinate system
rdef <- st_transform(rdef, crs = st_crs(all_tows_sf))

## Join them spatially
all_tows_with_LFA <- st_join(all_tows_sf, rdef, join = st_within)

##convert back to DF
all_tows_with_LFA_df <- as.data.frame(all_tows_with_LFA)
all_tows_with_LFA_df$geometry <- st_geometry(all_tows_with_LFA)

### summarize the dataframe by year and LFA

all_tows_with_LFA_df$year<-as.numeric(all_tows_with_LFA_df$year)


### Determine Mean lobster count per year #### REMAKE WITH STRATA PAIRING
mean_abun <- all_tows_with_LFA_df %>%
  group_by(LFA, year) %>%
  filter(LFA !=34)%>%
  filter(LFA !=37)%>%
  summarize(Mean_Abundance = mean(abun_raw))

scalsurMA<-ggplot(mean_abun, aes(x = year, y = Mean_Abundance)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ LFA, scales="free_y") +
  labs(x = "Year", y = "Mean Abundance of Lobster Recruits") +
  scale_x_continuous(limits = c(min(mean_abun$year), max(mean_abun$year) + 1))
  theme_set(theme_test(base_size = 14))

ggsave(filename = "meanrecruitabundancelobster.png", plot = scalsurMA, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 16, height = 14, units = "in", dpi = 300)




# Create Decade column with custom breaks, including 2019-2024
sumS <- all_tows_with_LFA_df %>%
  mutate(Decade = cut(year, breaks = c(1999, 2010, 2019, 2025), right = FALSE, labels = c("1999-2009", "2010-2018", "2019-2024")))


yearly_tow_counts <-sumS%>%
  group_by(year, Decade, LFA) %>%
  summarise(
    total_animals = sum(abun_raw, na.rm = TRUE),
    tows_with_nonzero_abun = sum(abun_raw != 0, na.rm = TRUE),
    total_tows = n_distinct(TOW_SEQ)
  )

median_tows_per_year <- yearly_tow_counts %>%
  group_by(Decade, LFA) %>%
  summarise(
    median_tows_per_year = median(total_tows, na.rm = TRUE)
  )

scalsum <- yearly_tow_counts %>%
  group_by(Decade, LFA) %>%
  summarise(
    totalNLOb= sum(total_animals, na.rm = TRUE),
    towswithLob= sum(tows_with_nonzero_abun, na.rm = TRUE),
    total_towscomplete = sum(total_tows, na.rm = TRUE)
  ) %>%
  left_join(median_tows_per_year, by = c("Decade", "LFA"))

scalsum <- scalsum%>%
  filter(LFA != 34)%>%
  filter(LFA!=37)

scalsum<-as.data.frame(scalsum)

write.csv(scalsum, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/scalsummarytable.csv")


#### Map of the tow locations by year
sumSmap<-sumS%>%
 filter(LFA != 34)
#sumSmap<-as.data.frame(sumSmap)
#sumSmap<-sumSmap%>%select(TOW_SEQ, year,LFA, SLONG,SLAT,Decade)

sumSmap = st_as_sf(sumSmap,coords = c('SLONG','SLAT'),crs=4326)



# Split the data by decade
decade_1999_2009 <- sumSmap[sumSmap$Decade == "1999-2009", ]
decade_2010_2018 <- sumSmap[sumSmap$Decade == "2010-2018", ]
decade_2019_2024 <- sumSmap[sumSmap$Decade == "2019-2024", ]


##point size has to be changed in gglobstermap
ss1999<-ggLobsterMap('custom',xlim=c(-68,-63),ylim=c(43.75,46),addGrids = F,addPoints = T,pts=decade_1999_2009,fw='~year')+
  theme_test(base_size=14)+
  scale_x_continuous(labels = function(x) sprintf("%.0f", x))
  ggsave(filename = "scallopcoverage1999.png", plot = ss1999, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 16, height = 14, units = "in", dpi = 300)

ss2010<-ggLobsterMap('custom',ylim=c(43.75,46),xlim=c(-68,-63),addGrids = F,addPoints = T,pts=decade_2010_2018,fw='~year')+
  theme_test(base_size=14)+
  scale_x_continuous(labels = function(x) sprintf("%.0f", x))
ggsave(filename = "scallopcoverage2010.png", plot = ss2010, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 16, height = 14, units = "in", dpi = 300)

ss2019<-ggLobsterMap('custom',ylim=c(43.75,46),xlim=c(-68,-63),addGrids = F,addPoints = T,pts=decade_2019_2024,fw='~year')+
  theme_test(base_size=14)+
  scale_x_continuous(labels = function(x) sprintf("%.0f", x))
ggsave(filename = "scallopcoverage2019.png", plot = ss2019, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 16, height = 14, units = "in", dpi = 300)



###### Length Frequencies for Scal Survey 

xle = z %>% pivot_longer(starts_with('P'))
xle$Length = as.numeric(substr(xle$name,3,8))
#xle= na.zero(xle,cols='value')
xxa = aggregate(value~Length+Year,data=xle,FUN=mean)
ggplot(subset(xxa),aes(x=Length,y=value))+geom_bar(stat='identity')+facet_wrap(~Year,scales = 'free_y')+xlab('Carapace Length')+ylab('Density') +theme_test(base_size = 14)+geom_vline(xintercept = 82.5,color='red')




###### Mean abundance of recruit lobster from Scallop Survey by LFA and Year
## Take Bycatch data and filter by size
recruitBycatch<-bycatch[bycatch$MEAS_VAL >= 70 & bycatch$MEAS_VAL <= 82, ]
recruitBycatch <- recruitBycatch[complete.cases(recruitBycatch), ]

#######Pair position of Tow start to LFA polygons to summarize by Year and LFA #########

#polygons LFA
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

rL = read.csv(file.path(layerDir,"Polygons_LFA.csv"))
rL$label=rL$LFA

rdef=Mar.utils::df_to_sf(rL,lat.field = "Y",lon.field = "X") 
rdef<-merge(rdef,unique(rL[,c("PID", "LFA")]), all.x=T, by="PID")

##Convert dataframe to Sf
recruits_sf <- st_as_sf(recruitBycatch , coords = c("SLONG", "SLAT"), crs = 4326)
recruits_sf $SLONG <- recruitBycatch$SLONG
recruits_sf $SLAT <- recruitBycatch$SLAT
# Check same coordinate system
rdef <- st_transform(rdef, crs = st_crs(recruits_sf))

## Join them spatially
recruits_sf_LFA <- st_join(recruits_sf, rdef, join = st_within)

##convert back to DF
recruits_sf_df <- as.data.frame(recruits_sf_LFA)
recruits_sf_df$geometry <- st_geometry(recruits_sf_LFA)

### summarize the dataframe by year and LFA

recruits_sf_df$year<-as.numeric(recruits_sf_df$year)

### if needed do it

scalsurRecruit<-ggplot(recruits_sf_df, aes(x = year, y = mean_abundance)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ LFA, scales="free_y") +
  labs(x = "Year", y = "Mean Abundance of Lobster") +
  scale_x_continuous(limits = c(min(mean_abun$year), max(mean_abun$year) + 1))
theme_set(theme_test(base_size = 14))

