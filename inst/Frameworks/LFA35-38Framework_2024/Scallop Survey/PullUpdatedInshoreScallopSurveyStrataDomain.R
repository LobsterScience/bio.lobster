#### Import Inshore Scallop Survey Regular Domain shapefiles
## J.Sameoto Sept 2024 
## Note SPA 2 (South West and North West Banks -- at mouth of BoF only irregularly surveyed -- Scal_Area == "SPA2"

library(sf)

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
strata <- st_read(paste0(temp2, "/inshore_survey_strata/Inshore_Survey_Strata_update_sept2024/Scallop_Strata.shp"))

#note coordinate reference system 
st_crs(strata)

#view spatially 
plot(strata)

## scal_area = management zone that is related to scallop
##strata_ID