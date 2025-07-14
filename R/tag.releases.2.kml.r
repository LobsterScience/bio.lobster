p = bio.lobster::load.environment()
require(LobTag2)
get_table("releases", db = "Oracle")

releases$label=paste0(releases$TAG_ID, " (",releases$YEAR, ")")
releases$TAG_NUM <- as.numeric(releases$TAG_NUM)

releases=releases[order(releases$TAG_NUM), ]

names(releases)[names(releases) == "MANAGEMENT_AREA"] <- "LFA"

selected_cols <- c("AFFILIATION", "CAPTAIN", "PORT", "LFA", "DAY", "MONTH", "YEAR",
                   "TAG_ID", "CARAPACE_LENGTH", "SEX", "SHELL", "CLAW", "LAT_DD", "LON_DD", "label")

releases<- releases[, selected_cols]

#remove swlss
releases=subset(releases, AFFILIATION %ni% "SWLSS")

write.csv(releases, file="all.tag.releases.csv")


#----------------------------------
# Creating a kmz using Google Earth
#----------------------------------

#Then Import into Google Earth using file -> import. Click through wizard
#Choose "Import All"
#Choose "Yes" for style template (will adjust in next step). create one or use existing.
#If creating new- choose field "label" as Field Name
#Google Earth will be slow- big data file
#all.tag.releases.csv will appear in your "Temporary Places", right click on it and choose "properties"
#Click "Style, Color" tab and click "Share Style"
#increase label to 1.2 scale, icon to 1.5 scale
#Hit the icon button on the top right and choose icon you want (usually lobster science 'logo')
#Click OK


