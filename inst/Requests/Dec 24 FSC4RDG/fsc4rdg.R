#git\bio.lobster\inst\Requests\AquacultureSiting
#C:\Users\cooka\Documents\git\bio.lobster\inst\Requests\SMB2022.r


#Ideas to investigate
#Done    #map of both areas
#done     #% of licenses fishing in that grid over time (are commercial harvesters avoiding that grid)
    #Also the number of licences and the proportion of income for those individuals who fish there....sustainability impacts
#Done   #% of commercial landings from that grid over time
#Done    #estimate  density of licences per area (km2) either LFA or grid (using those depth pruned polygons)
#Done    #estimate density of traps per area (km2) either LFA or grid (using those depth pruned polygons)
#Done    #compare densities from St peters and SMB to all other grids or regions within the Maritimes
#Requested #Get the number of traps from megan that are available to be fished in the areas and some estimate of how many are actually in the smaller SMB and SPB
#Done    #Show the trends in CPUE from commercial fisheries in the areas that have been harvested by FSC/ML. Compare the grid containing the area of FSC focus to adjacent reporting grids. 
#Done    #compare those trends to adjacent areas (( I think code exists for that)
#Done    #discuss the extent of fishing grounds available to the commercial harvesters (ie can the move elsewhere if CPUE drops, SMB yes SPB no)
#Done   #% of licenses fishing in that grid over time (are commercial harvesters avoiding that grid)
#Done    #% of commercial landings from that grid over time

#Manon / Megan Folkins Requests
    #St-Mary’s a nursery area?  I think I have the answer on that, but I will let you address it.
#Done    #Number of survey tows in the LFA 34 St-Mary’s area – she would like survey-based catch rate trends based on this, but unless you have a good number of tows in there…… I can’t see how informative this would be (if wrong, correct me).
#Done    #Soft shell incidence throughout the year for both LFA 29 and 34 if available.
#N/A    #She has reviewed the information already provided by Adam and there is no need to address life cycle or management measures.   
#N/A    #She mentioned that Verna was aware of a CSAS request on Alternate Harvest Scenarios, but she thinks it has been pulled – this was an attempt at getting it prioritized.       


require(bio.lobster)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(sf)
require(PBSmapping)
require(dplyr)
require(tidyr)

p = bio.lobster::load.environment()

la() #load_all

#Choose one
assessment.year = p$current.assessment.year 
#assessment.year = p$current.assessment.year-1 

#If you only want to update logs and CCIR for the last two years, run this:
#p$yr=p$current.assessment.year

figdir = file.path(project.datadirectory("bio.lobster","requests","fsc4rdg",assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )


#If you only want to update logs for the last two years, run this:
#p$yr=p$current.assessment.year

# update data through ROracle
NewDataPull =F
if(NewDataPull){
    lobster.db('fsrs.redo')
    lobster.db('logs.redo')
    lobster.db('annual.landings.redo')
    #lobster.db('vlog.redo') #These are static now, no need to update
    logs=lobster.db('season.dates.redo') #updates season dates as required
    logs=lobster.db('process.logs.redo')
    per.rec= lobster.db("percent_reporting")
}

#Focusing on LFA 29 and 34
p$lfas=c("29", "34")


#For Individual LFAs with grids labelled, highlighting grids with SPB and SMB
png(filename=file.path(figdir, "MapLFA32.png") ,width=6.5, height=6.5, units = "in", res = 800)
LobsterMap('29', labels=c('lfa','grid'), grid.labcex=0.6)
#insert after grids plotted- #addPolys(LFAgrid[LFAgrid$SID=="344",], col="darkgoldenrod1") #grids 69,81,92 in SMB
dev.off()

#######-----------------------------------------
# Primary Indicator- Commercial CPUE
#######-----------------------------------------
p$lfas= c("29", "34")

logs=lobster.db("process.logs")

logs= subset(logs,SYEAR %in% 2014:p$current.assessment.year-1 & LFA %in% p$lfas) 


#--------------------------------------------------------------------------
# # Compare annual median catch rates from the area in question and the LFA at large
#--------------------------------------------------------------------------

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2008:p$current.assessment.year-1 & LFA %in% p$lfas) 

#by grid
#-----------------------------------------------
#St Peters Bay- (Grid 344)

# First, calculate median CPUE by SYEAR and LFA = "29"
df_lfa29 <- a %>%
    filter(LFA == "29", !GRID_NUM %in% c(344)) %>%
    group_by(SYEAR) %>%
    summarize(median_cpue = median(CPUE, na.rm = TRUE), .groups = "drop")

# Second, calculate median CPUE by SYEAR for LFA = "29" and GRID_NUM in (344)
df_344 <- a %>%
    filter(LFA == "29" & GRID_NUM %in% c(344)) %>%
    group_by(SYEAR) %>%
    summarize(median_cpue = median(CPUE, na.rm = TRUE), .groups = "drop")

# Create the plot with both lines
png(filename=file.path(figdir, "StPetersBay.cpue.in and out.png"),width=6, height=4, units = "in", res = 800)
ggplot() +    # Line for all LFA 29
    geom_line(data = df_lfa29, aes(x = SYEAR, y = median_cpue, color = "LFA 29 (no Grid 344)"), size = 1) +    
    # Line for Grid 344
    geom_line(data = df_344, aes(x = SYEAR, y = median_cpue, color = "Grid 344"), size = 1) + 
    # Points for all LFA 29
    geom_point(data = df_lfa29, aes(x = SYEAR, y = median_cpue, color = "LFA 29 (no Grid 344)")) +    
    # Points for grid 344
    geom_point(data = df_344, aes(x = SYEAR, y = median_cpue, color = "Grid 344")) +   
    # Labels and title
    labs(title = "CPUE by YEAR",
         x = "Year",
         y = "CPUE (kg/trap haul)",
         color = "Legend") +   
    scale_x_continuous(breaks = seq(min(a$SYEAR), max(a$SYEAR), by = 1)) +  # Customizing the x-axis to ensure it's ordered
    scale_y_continuous(limits = c(0, NA)) +     # Setting the y-axis to start from 0
    # Custom color scale
    scale_color_manual(values = c("LFA 29 (no Grid 344)" = "blue", "Grid 344" = "red")) +
    # Minimal theme for a clean plot
    #theme_minimal() +
    # Move the legend to the bottom-right corner
    theme(legend.position = c(0.85, 0.15),
          legend.title = element_blank())  # Remove the legend title
dev.off()

#St Marys Bay- (Grids 69, 81., 92) 

# # Compare annual median catch rates from the area in question and the LFA at large
# 
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2008:(p$current.assessment.year-1) & LFA %in% p$lfas) 

# First, calculate median CPUE by SYEAR and LFA = "34"
df_lfa34 <- a %>%
    filter(LFA == "34", !GRID_NUM %in% c(69, 81, 92)) %>%
    group_by(SYEAR) %>%
    summarize(median_cpue = median(CPUE, na.rm = TRUE), .groups = "drop")

# Second, calculate median CPUE by SYEAR for LFA = "34" and GRID_NUM in (69, 81, 92)
df_smb <- a %>%
    filter(LFA == "34" & GRID_NUM %in% c(69, 81, 92)) %>%
    group_by(SYEAR) %>%
    summarize(median_cpue = median(CPUE, na.rm = TRUE), .groups = "drop")

# Create the plot with both lines
png(filename=file.path(figdir, "SMB.cpue.in and out.png"),width=6, height=4, units = "in", res = 800)
ggplot() +    # Line for all LFA 34
    geom_line(data = df_lfa34, aes(x = SYEAR, y = median_cpue, color = "LFA 34 (no SMB)"), size = 1) +    
    # Line for St Marys Bay (GRID 69, 81, 92)
    geom_line(data = df_smb, aes(x = SYEAR, y = median_cpue, color = "St Marys Bay"), size = 1) + 
    # Points for all LFA 34
    geom_point(data = df_lfa34, aes(x = SYEAR, y = median_cpue, color = "LFA 34 (no SMB)")) +    
    # Points for St Marys Bay (GRID 69, 81, 92)
    geom_point(data = df_smb, aes(x = SYEAR, y = median_cpue, color = "St Marys Bay")) +   
    # Labels and title
    labs(title = "CPUE by YEAR",
         x = "Year",
         y = "CPUE (kg/trap haul)",
         color = "Legend") +   
    scale_x_continuous(breaks = seq(min(a$SYEAR), max(a$SYEAR), by = 1)) +  # Customizing the x-axis to ensure it's ordered
    scale_y_continuous(limits = c(0, NA)) +     # Setting the y-axis to start from 0
    # Custom color scale
    scale_color_manual(values = c("LFA 34 (no SMB)" = "blue", "St Marys Bay" = "red")) +
    # Minimal theme for a clean plot
    #theme_minimal() +
    # Move the legend to the bottom-right corner
    theme(legend.position = c(0.85, 0.15),
          legend.title = element_blank())  # Remove the legend title
dev.off()

#--------------------------------------------------------------------------
#Compare proportion of all LFA licences that are active in area of interest by year
#--------------------------------------------------------------------------

#St Peters

# Assuming 'a' is your data frame
# 1. Count unique LICENCE_ID for all LFA = "34"
df_lfa29 <- a %>%
    filter(LFA == "29") %>%
    group_by(SYEAR) %>%
    summarize(count_unique_licence_all = n_distinct(LICENCE_ID), .groups = "drop")

# 2. Count unique LICENCE_ID for LFA = "29" and GRID_NUM in (344)
df_344 <- a %>%
    filter(LFA == "29" & GRID_NUM %in% c(344)) %>%
    group_by(SYEAR) %>%
    summarize(count_unique_licence_344 = n_distinct(LICENCE_ID), .groups = "drop")

# Join both data frames by SYEAR
df_combined <- df_lfa29 %>%
    left_join(df_344, by = "SYEAR")

# Order by SYEAR from lowest to highest
df_combined <- df_combined %>%
    arrange(SYEAR)

df_combined <- df_combined %>%
    mutate(proportion_stmarys = count_unique_licence_344 / count_unique_licence_all)

#Calculate the proportion of St Marys Bay licences compared to All LFA 34
df_combined <- df_combined %>%
    mutate(proportion_stmarys = count_unique_licence_344 / count_unique_licence_all)

# Order by SYEAR from lowest to highest
df_combined <- df_combined %>%
    arrange(SYEAR)

# Plot using ggplot

png(filename=file.path(figdir, "Grid344.prop.license.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_combined, aes(x = SYEAR, y = proportion_stmarys)) +
    geom_line(color = "blue", size = 1) +  # Line for proportion
    geom_point(color = "blue", size = 2) +  # Points for each data point
    labs(title = "Proportion of LFA 29 Licences Active in Grid 344",
         x = "Year",
         y = "Proportion") +
    scale_y_continuous(limits = c(0, .5)) +  # Ensure y-axis starts at 0 and ends at 1
    theme_minimal() +
    theme(legend.position = c(0.85, 0.15),  # Place legend at bottom right
          legend.title = element_blank())  # Remove legend title
dev.off()


#SMB

# 1. Count unique LICENCE_ID for all LFA = "34"
df_lfa34 <- a %>%
    filter(LFA == "34") %>%
    group_by(SYEAR) %>%
    summarize(count_unique_licence_all = n_distinct(LICENCE_ID), .groups = "drop")

# 2. Count unique LICENCE_ID for LFA = "34" and GRID_NUM in (69, 81, 92)
df_lfa34_grid69_81_92 <- a %>%
    filter(LFA == "34" & GRID_NUM %in% c(69, 81, 92)) %>%
    group_by(SYEAR) %>%
    summarize(count_unique_licence_stmarys = n_distinct(LICENCE_ID), .groups = "drop")

# Join both data frames by SYEAR
df_combined <- df_lfa34 %>%
    left_join(df_lfa34_grid69_81_92, by = "SYEAR")

# Order by SYEAR from lowest to highest
df_combined <- df_combined %>%
    arrange(SYEAR)

df_combined <- df_combined %>%
    mutate(proportion_stmarys = count_unique_licence_stmarys / count_unique_licence_all)

#Calculate the proportion of St Marys Bay licences compared to All LFA 34
df_combined <- df_combined %>%
    mutate(proportion_stmarys = count_unique_licence_stmarys / count_unique_licence_all)

# Order by SYEAR from lowest to highest
df_combined <- df_combined %>%
    arrange(SYEAR)

# Plot using ggplot

png(filename=file.path(figdir, "SMB.prop.license.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_combined, aes(x = SYEAR, y = proportion_stmarys)) +
    geom_line(color = "blue", size = 1) +  # Line for proportion
    geom_point(color = "blue", size = 2) +  # Points for each data point
    labs(title = "Proportion of LFA 34 Licences Active in SMB",
         x = "Year",
         y = "Proportion") +
    scale_y_continuous(limits = c(0, .15)) +  # Ensure y-axis starts at 0 and ends at 1
    theme_minimal() +
    theme(legend.position = c(0.85, 0.15),  # Place legend at bottom right
          legend.title = element_blank())  # Remove legend title
dev.off()


#Looking at it another way
#by year- for any license that fished in inner SMB (69, 81) what proportion of their days were in those grids

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2008:(p$current.assessment.year-1) & LFA %in% p$lfas) 

#Test Case with Stacey Denton - LICENCE_ID=="110298"
a=subset(a,LICENCE_ID=="110298" )
# Step 1: Calculate unique DATE_FISHED counts for each LICENCE_ID
df_unique_dates <- a %>%
    group_by(SYEAR, LICENCE_ID) %>%
    summarize(
        unique_dates_stmarys = n_distinct(DATE_FISHED[GRID_NUM %in% c(69, 81)]),  # Dates within St Marys Bay grids
        unique_dates_other = n_distinct(DATE_FISHED[!GRID_NUM %in% c(69, 81)]),  # Dates outside St Marys Bay grids
        total_dates = n_distinct(DATE_FISHED),  # Total unique dates for each LICENCE_ID
        .groups = "drop"
    )

# Step 2: Filter out LICENCE_IDs with no unique DATE_FISHED in GRID_NUM 69, 81, 92
df_filtered <- df_unique_dates %>%
    filter(unique_dates_stmarys > 0)  # Keep only LICENCE_IDs with at least one unique date in the specified grids

# Step 3: Calculate the proportion of DATE_FISHED in the St Marys Bay grids (69, 81, 92)
df_proportion <- df_filtered %>%
    mutate(
        proportion_stmarys = unique_dates_stmarys / total_dates  # Proportion of dates in St Marys Bay grids
    )

# Step 4: Calculate the mean proportion of DATE_FISHED in St Marys Bay by SYEAR
df_mean_proportion <- df_proportion %>%
    group_by(SYEAR) %>%
    summarize(mean_proportion_stmarys = mean(proportion_stmarys), .groups = "drop")

# Step 5: Plot using ggplot
ggplot(df_mean_proportion, aes(x = SYEAR, y = mean_proportion_stmarys)) +
    geom_line(size = 1) +   # Line plot
    geom_point(size = 2) +  # Points for each year
    labs(title = "Mean Proportion of DATE_FISHED in St Marys Bay by SYEAR",
         x = "SYEAR",
         y = "Mean Proportion of DATE_FISHED in St Marys Bay") +
    scale_y_continuous(limits = c(0, 1)) +  # Y-axis range starts at 0
    theme_minimal() +
    theme(legend.position = "bottom",   # Legend position
          legend.title = element_blank())  # Remove legend title

#A more simplistic approach to looking at changing fishing patterns over time
#proportion of total trap hauls in area of interest by year

#Grid 344

df_proportion_traps <- a %>%
    filter(LFA == "29") %>%  # Filter for LFA 34
    group_by(SYEAR) %>%  # Group by SYEAR
    summarize(
        traps_smb = sum(NUM_OF_TRAPS[GRID_NUM %in% c(344)]),  # NUM_OF_TRAPS for Grid 344
        traps_all = sum(NUM_OF_TRAPS),  # Total NUM_OF_TRAPS for LFA == 34
        .groups = "drop"
    ) %>%
    mutate(
        proportion_smb = traps_smb / traps_all  # Proportion of NUM_OF_TRAPS in Grid 344
    )

# Step 2: Create the ggplot line plot with a dotted trend line
png(filename=file.path(figdir, "prop.commercial.traps.Grid 344.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_proportion_traps, aes(x = SYEAR, y = proportion_smb)) +
    geom_line(size = 1, color = "blue") +  # Line color set to blue
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dotted") +  # Add a dotted trend line (red)
    labs(
        x = "Year",
        y = "Proportion",
        title = "Proportion of Commercial Traphauls from Grid 344"  # Title
    ) +
    #theme_minimal() +  # Clean theme
    scale_y_continuous(limits = c(0, NA)) +  # Ensure y-axis starts from 0
    theme(
        legend.position = "bottom",  # Position the legend in the bottom right
        legend.title = element_blank()  # Remove the legend title
    )
dev.off()

#SMB
df_proportion_traps <- a %>%
    filter(LFA == "34") %>%  # Filter for LFA 34
    group_by(SYEAR) %>%  # Group by SYEAR
    summarize(
        traps_smb = sum(NUM_OF_TRAPS[GRID_NUM %in% c(69, 81, 92)]),  # NUM_OF_TRAPS for St Marys Bay grids
        traps_all = sum(NUM_OF_TRAPS),  # Total NUM_OF_TRAPS for LFA == 34
        .groups = "drop"
    ) %>%
    mutate(
        proportion_smb = traps_smb / traps_all  # Proportion of NUM_OF_TRAPS in St Marys Bay grids
    )

# Step 2: Create the ggplot line plot with a dotted trend line
png(filename=file.path(figdir, "prop.commercial.traps.SMB.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_proportion_traps, aes(x = SYEAR, y = proportion_smb)) +
    geom_line(size = 1, color = "blue") +  # Line color set to blue
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dotted") +  # Add a dotted trend line (red)
    labs(
        x = "Year",
        y = "Proportion",
        title = "Proportion of Commercial Traphauls from SMB"  # Title
    ) +
   # theme_minimal() +  # Clean theme
    scale_y_continuous(limits = c(0, NA)) +  # Ensure y-axis starts from 0
    theme(
        legend.position = "bottom",  # Position the legend in the bottom right
        legend.title = element_blank()  # Remove the legend title
    )
dev.off()




#-----------------------------------------------------------------
# Proportion of commercial landings from each of the areas of interest
#-----------------------------------------------------------------

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2008:(p$current.assessment.year-1) & LFA %in% p$lfas) 


#SMB
# Step 1: Calculate the proportion of WEIGHT_KG for GRID_NUM in c(69, 81, 92) compared to the total for LFA == 34
df_proportion <- a %>%
    filter(LFA == "34") %>%  # Filter for LFA 34
    group_by(SYEAR) %>%  # Group by SYEAR
    summarize(
        weight_stmarys = sum(WEIGHT_KG[GRID_NUM %in% c(69, 81, 92)]),  # WEIGHT_KG for St Marys Bay grids
        weight_all = sum(WEIGHT_KG),  # Total WEIGHT_KG for LFA == 34
        .groups = "drop"
    ) %>%
    mutate(
        proportion_stmarys = weight_stmarys / weight_all  # Proportion of WEIGHT_KG in St Marys Bay grids
    )
# Step 2: Create the ggplot line plot with a dotted trend line
png(filename=file.path(figdir, "prop.commercial.land.SMB.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_proportion, aes(x = SYEAR, y = proportion_stmarys)) +
    geom_line(size = 1, color = "blue") +  # Line color set to blue
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dotted") +  # Add a dotted trend line (red)
    labs(
        x = "Year",
        y = "Proportion",
        title = "Proportion of Commercial Landings from SMB"  # Title
    ) +
    #theme_minimal() +  # Clean theme
    scale_y_continuous(limits = c(0, NA)) +  # Ensure y-axis starts from 0
    theme(
        legend.position = "bottom",  # Position the legend in the bottom right
        legend.title = element_blank()  # Remove the legend title
    )
dev.off()

#Grid 344
# Step 1: Calculate the proportion of WEIGHT_KG for GRID_NUM in c(69, 81, 92) compared to the total for LFA == 34
df_proportion <- a %>%
    filter(LFA == "29") %>%  # Filter for LFA 34
    group_by(SYEAR) %>%  # Group by SYEAR
    summarize(
        weight_stmarys = sum(WEIGHT_KG[GRID_NUM %in% c(344)]),  # WEIGHT_KG for Grid 344
        weight_all = sum(WEIGHT_KG),  # Total WEIGHT_KG for LFA == 29
        .groups = "drop"
    ) %>%
    mutate(
        proportion_stmarys = weight_stmarys / weight_all  # Proportion of WEIGHT_KG in grid 344
    )
# Step 2: Create the ggplot line plot with a dotted trend line
png(filename=file.path(figdir, "prop.commercial.land.grid 344.png"),width=6, height=4, units = "in", res = 800)
ggplot(df_proportion, aes(x = SYEAR, y = proportion_stmarys)) +
    geom_line(size = 1, color = "blue") +  # Line color set to blue
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dotted") +  # Add a dotted trend line (red)
    labs(
        x = "Year",
        y = "Proportion",
        title = "Proportion of Commercial Landings from Grid 344"  # Title
    ) +
    #theme_minimal() +  # Clean theme
    scale_y_continuous(limits = c(0.2, NA)) +  # Ensure y-axis starts from 0
    theme(
        legend.position = "bottom",  # Position the legend in the bottom right
        legend.title = element_blank()  # Remove the legend title
    )
dev.off()


#-----------------------------------------------------------------
# Look at Soft Lobsters from LFA 34 by month
#-----------------------------------------------------------------

atSea1 = connect.command(con, "select * from lobster.lobster_bycatch_assoc")
atSea1=atSea1[ , which(names(atSea1) %in% c("SPECSCD_ID","SHELL", "COMAREA_ID","BOARD_DATE" ))]
atSea2 = connect.command(con, "select * from lobster.lobster_bycatch_SWLSS_2023")  
atSea2=atSea2[ , which(names(atSea2) %in% c("SPECSCD_ID","SHELL", "COMAREA_ID","BOARD_DATE" ))]

atSea=rbind(atSea1, atSea2)                         

atsea=subset(atSea, SPECSCD_ID=="2550" & is.finite(SHELL))

# Step 1: Filter records where COMAREA_ID == "L34"
atsea_filtered <- atsea %>%
    filter(COMAREA_ID == "L34")

# Step 2: Create a new column to categorize records as "soft" or "hard"
atsea_filtered <- atsea_filtered %>%
    mutate(SHELL_TYPE = case_when(
        SHELL %in% c(1, 2, 3, 7) ~ "soft",
        SHELL %in% c(4, 5, 6) ~ "hard",
        TRUE ~ "other"  # Handling unexpected cases, can be omitted if not necessary
    ))

# Step 3: Extract the month from BOARD_DATE, adjust the month to make Dec = 1
atsea_filtered <- atsea_filtered %>%
    mutate(Month = month(BOARD_DATE, label = TRUE, abbr = TRUE)) %>%
    mutate(Month = factor(Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))

# Step 4: Calculate the proportion of soft records for each month
atsea_monthly_proportion <- atsea_filtered %>%
    group_by(Month) %>%
    summarise(
        total_records = n(),
        soft_records = sum(SHELL_TYPE == "soft"),
        soft_proportion = soft_records / total_records
    )

# Step 5: Create a line plot of the proportions of soft records by month

png(filename=file.path(figdir, "prop.soft.month.lfa34.png"),width=6, height=4, units = "in", res = 800)
ggplot(atsea_monthly_proportion, aes(x = Month, y = soft_proportion, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Proportion of Soft Lobster by Month",
        x = "Month",
        y = "Proportion"
    ) +
    #theme_minimal() +
    scale_y_continuous(limits = c(0, 0.5)) +  # Ensure y-axis starts from 0
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(filename=file.path(figdir, "prop.soft.month.lfa34.zoomed.png"),width=6, height=4, units = "in", res = 800)
ggplot(atsea_monthly_proportion, aes(x = Month, y = soft_proportion, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Proportion of Soft Lobster by Month (Zoomed In)",
        x = "Month",
        y = "Proportion"
    ) +
    #theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# # Plots unbiased annual CPUE for all LFAs in Maritimes region
# # Good context for relative CPUE levels and trends
# 
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% (2008:p$current.assessment.year)) 

aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0
#by time
for(i in 1:length(aa)){
    tmp<-aa[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$time = ceiling(tmp$time/7) #convert to week of season
    if(nrow(tmp)>5){
        m=m+1
        g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
        g$lfa=unique(aa[[i]]$LFA)
        g$yr = unique(aa[[i]]$SYEAR)
        g = t(g)[,2]
        cpue.lst[[m]] <- g
    }
}
cc =as.data.frame(do.call(rbind,cpue.lst))
cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
cc = cc[order(cc$lfa,cc$yr),]
cc$yr = as.numeric(cc$yr)
cc$fyr = as.factor(cc$yr)
last_bar_color="black"
point_colors <- ifelse(cc$yr <max(cc$yr), last_bar_color, "orange")
cc1 = cc

png(filename=file.path(figdir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
    geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
    scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
    labs(y= "CPUE", x = "Year")
dev.off()



#Unbiased cpue patterns by week of season 
#Used as a baseline for determining traps required in an LFA for FSC
#-----------------------------------------

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2009:p$current.assessment.year)


aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0

aa = split(a,f=list(a$LFA,a$SYEAR))
source("C:/bio/bio.lobster/R/rm.from.list2.r") #had a problem with original rm.from.list. Modified under new name
aa = rm.from.list2(aa)
cpue.lst<-list()

#by time
for(i in 1:length(aa)){
    tmp<-aa[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp = tmp[order(tmp$time),]
    tmp$time = ceiling(tmp$time/7) #convert to week of season
    g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size = 5))
    g$lfa=unique(aa[[i]]$LFA)
    g$yr = unique(aa[[i]]$SYEAR)
    # g = t(g)[,1]
    cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
write.csv(cc, file=file.path(figdir,"cpue.weekly.all.lfas.csv"))


mean= aggregate(cc, CPUE~yr+lfa,mean )


for (l in c("29","34")){
    png(filename=file.path(figdir, paste0("weekly_cpue_",l,".png")),width=8, height=5.5, units = "in", res = 800)
    print(
        ggplot(subset(cc,lfa==l),aes(x=t,y=CPUE))+geom_point()+
            geom_smooth(se=F)+facet_wrap(~yr)+
            labs(title =paste0("LFA ",l))+
            labs(y= "CPUE (kg/th)", x = "Week of Season") +
            theme(plot.title = element_text(hjust = 0.5))+
            # stat_summary(fun='mean', geom="line")
            geom_hline(data=mean[mean$lfa==l,], aes(yintercept= CPUE), color='red', linewidth=0.6)
        
    )
    dev.off()
}





# Fishery footprint- Useful in comparing years, etc
#------------------------------------------------------------
#Data Prep
{
    
grid.dir=file.path(figdir, "grids")
dir.create( grid.dir, recursive = TRUE, showWarnings = FALSE )
setwd(grid.dir)


layerDir=file.path("C:","bio","bio.lobster.data", "mapping_data")
r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<=p$current.assessment.year)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:p$current.assessment.year
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=p$current.assessment.year)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
num.yr=length(2005:p$current.assessment.year)
b$LFA=rep(c(33,34,35,36,38),each=num.yr)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<=p$current.assessment.year, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=num.yr)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list2(sL)
cpue.lst<-list()
cpue.ann = list()

for(i in 1:length(sL)){
    tmp<-sL[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    g<-biasCorrCPUE(tmp,by.time = F)
    cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
}

cc =as.data.frame(do.call(rbind,cpue.lst))

cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))

cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)


###########################################
#part the effort to grids

partEffort = list()

for(i in 1:length(sL)){
    tmp = sL[[i]]
    tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
    pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
    pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
    pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
    pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
    
    partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)

#pe = merge(partEffort,r,by.x=c('GRID_NUM','LFA'),by.y=c('GRID_NO','LFA'))

saveRDS(partEffort,'TrapHaulsWithinGrid.rds')


#############################################
# PartitionLandings to Grids

partLandings = list()

for(i in 1:length(sL)){
    tmp = sL[[i]]
    tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
    pTH = aggregate(WEIGHT_KG~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
    pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
    partLandings[[i]] = pTH
}

partLandings = do.call(rbind, partLandings)

saveRDS(partLandings,'LandingsWithinGrid.rds')

###################################################
##Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=p$current.assessment.year)

gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gg,'SDLOGSWithinGrid.rds')

#############merge
#Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=p$current.assessment.year)

gKL = aggregate(LICENCE_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gKL,'LicencesWithinCommunity.rds')

#############merge


Tot = merge(merge(merge(partEffort,partLandings),gg),gKL)

Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,BTTH,BL,SD_LOG_ID,LICENCE_ID))
names(Tot)= c('FishingYear','LFA','Grid','TrapHauls','Landings','Trips','NLics')

#Following lines respect the "Rule of Five", not using for RDG / IFM purposes
Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)
#saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
#Tot = readRDS('PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')

Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

GrMap = readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
#ex.grids=c("27-357.1", "27-357.3", "27-357.4", "27-357.6", "27-358.1", "27-356.1", "311-338.3", "311-338.4", "311-337.4")
#GrMap=GrMap[row.names(GrMap) %ni% ex.grids,] #Removes polygons offshore in LFA 27
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))


GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
b=r

o=GrMap

ggplot(b)+
    geom_sf()+
    geom_sf(data=coa,fill='grey')+
    geom_sf(data=o,fill='red')+
    coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
             ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
             expand = FALSE)


gTot$CPUE = gTot$Landings/gTot$TrapHauls
g27p = subset(gTot, FishingYear%in%2019:p$current.assessment.year)
}

#One figure 27-38 for context

ok1 = function(x=g27p){
    ggplot(x,aes(fill=CPUE))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral') +
        facet_wrap(~FishingYear)+
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=GrMap,fill=NA)+
        coord_sf(xlim = c(st_bbox(x)$xmin,st_bbox(x)$xmax),
                 ylim = c(st_bbox(x)$ymin,st_bbox(x)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(x)$xmin,st_bbox(x)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(x)$ymin,st_bbox(x)$ymax,length.out=2),2)))
}

png(filename=file.path(grid.dir, "grid.cpue.all.lfas.png"), width=1200, height=900, res=175)
print(ok1())
dev.off()

#One time series CPUE map for each LFA
for (xx in c("29", "34")){
    png(filename=file.path(grid.dir, paste(xx,"grid.cpue.png", sep="_")), width=1600, height=900, res=175)
    gpxx=subset(g27p, LFA==xx)
    print(ok1(x=gpxx))
    dev.off()	
}

#Comparative CPUE Maps

#Slice out individual years for comparison
gl = subset(g27p,FishingYear==p$current.assessment.year-1)

gp = subset(g27p,FishingYear==p$current.assessment.year)

gl$geometry<- NULL

gg = merge(gp,gl[,c('LFA','GRID_NO','CPUE')],by=c('LFA','GRID_NO'))


ls=unique(gg$LFA)
print(paste0('Looking at the following LFA(s):', ls,' for the following years: ', gl$FishingYear[1], ' & ',gp$FishingYear[1] ))

percent_diff <- function(row) {
    row$geometry<- NULL
    
    abs_diff <- (as.numeric(row[1]) - as.numeric(row[2]))
    mean_val <- mean(as.numeric(row))
    percent_diff <- (abs_diff / mean_val) * 100
    return(percent_diff)
}

gg$percentChange =  apply(gg[,c('CPUE.x','CPUE.y')],1,percent_diff)


require(colorspace)
lab=paste(gl$FishingYear[1], sprintf('\u2192'),gp$FishingYear[1], sep=" " )
tt=gg
cpue.diff= function(x, tsize=6, vj=20){
    ggplot(tt,aes(fill=percentChange))+
        geom_sf() +
        scale_fill_continuous_diverging(palette='Purple-Green') +
        labs(fill = "    CPUE\n% Change")+
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=GrMap,fill=NA)+
        coord_sf(xlim = c(st_bbox(tt)$xmin,st_bbox(tt)$xmax),
                 ylim = c(st_bbox(tt)$ymin,st_bbox(tt)$ymax),
                 expand = FALSE)+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.margin=grid::unit(c(8,2,8,2), "mm"))+
        scale_x_continuous(breaks = c(round(seq(st_bbox(tt)$xmin,st_bbox(tt)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))+
        #annotate("text", x=(st_bbox(tt)$xmax)-0.2, y=(st_bbox(tt)$ymin)+0.05, label= lab, size=tsize )
        #annotate("text", x = Inf, y = -Inf, label = lab, vjust = vj, hjust = 1.2, size=tsize)
        #annotate("text", x = Inf, y = -Inf, label = lab, vjust = vj, hjust = 1.2, size=tsize)
        annotate("text", x = Inf, y = -Inf, label = lab, hjust = 1.2, vjust = -1, size = tsize)   
}
#Add the following to ensure that text appears in the final pngs
library(showtext)
showtext_auto()
showtext_opts(dpi = 200)


#One figure 27-32 for context
png(filename=file.path(cpue.dir, "cpue_diff.png"), width=1300, height=1000, res=175)
print(cpue.diff())
dev.off()	

#Each Separate

for (xx in ls){
    tt=subset(gg, LFA==xx)
    if(xx=="27") {scale_y_continuous(breaks = c(round(seq(st_bbox(tt)$ymin,st_bbox(tt)$ymax,length.out=2),2)))}
    if(xx=="27"){tsize=3}else{tsize=6}
    if(xx=="27"){vj=30}else{vj=20}
    print(cpue.diff(tsize=tsize, vj=vj))
    if(xx=="27"){png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=700, height=900, res=175)}else
    {png(filename=file.path(cpue.dir, paste(xx,"cpue.diff.png", sep="_")), width=1600, height=900, res=175)}
    print(cpue.diff(tsize=tsize, vj=vj))
    dev.off()	
}


la()
setwd(grid.dir)

##from inst/IP/FootprintMapping/1.EstimatingTrapHaulsFromSlipsandSplit2Grids.r


###################################################################################################################
#Changed all instances of g27p to g27 as we do not want privacy screeen for internal information to RDG / IFM

makeNewFootPrints <- function(x,data,grmap=GrMap){
    LFA1=x 
    gTot=data
    gTot$LandingsT = gTot$Landings/1000
    g27 = subset(gTot,LFA==LFA1 & FishingYear>2018)
    if (LFA1=="29") {g27 = subset(gTot,LFA==LFA1 & FishingYear>2018 & FishingYear<p$current.assessment.year )}
    g27p = subset(gTot,LFA==LFA1 & FishingYear>2018 &PrivacyScreen==1)
    g27n = subset(gTot,LFA==LFA1 & FishingYear>2018 &PrivacyScreen==0) #no privacy screen (ignores "rule of 5")
    
    options(scipen=10000) #forces no scientific notation in legend scales for easier interpretion
    ok1 = ggplot(g27,aes(fill=TrapHauls))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Trap Hauls") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    width_inch <- 8.5 * 0.7  # 80% of the paper width
    height_inch <- 11 * 0.2  # 80% of the paper height
    
    # Save the plot with the specified dimensions
    nm = paste('LFA', LFA1,'- Logbook reported number of trap hauls by grid.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    g27$TrapHaulspkm2 = as.numeric(g27$TrapHauls/g27$area)
    
    ok1 = ggplot(g27,aes(fill=TrapHaulspkm2))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Trap Hauls / km2") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA', LFA1,'- Logbook reported number of trap hauls (TH) by grid corrected by the area of the grid (km2).png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    
    ok1 = ggplot(g27,aes(fill=LandingsT))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Landings (mt)") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA',LFA1,'- Logbook reported Landings (t) by grid.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    g27$Landingspkm2 = as.numeric(g27$LandingsT/g27$area)
    
    ok1 = ggplot(g27,aes(fill=Landingspkm2))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Landings/km2") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA',LFA1,'- Logbook reported Landings (t) by grid adjusted by area (km2).png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    
    ok1 = ggplot(g27,aes(fill=Trips))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral') +
        facet_wrap(~FishingYear)+
        ##geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA',LFA1,'- Logbook reported number of trips by grid.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    g27$Tripspkm2 = as.numeric(g27$Trips/g27$area)
    
    ok1 = ggplot(g27,aes(fill=Tripspkm2))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Trips/km2") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA',LFA1,'- Logbook reported number of trips by grid adjusted by area (km2).png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    ok1 = ggplot(g27,aes(fill=NLics))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Licences") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA', LFA1,'- Logbook reported number of licences fishing by grid.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    g27$Licencespkm2 = as.numeric(g27$NLics/g27$area)
    
    ok1 = ggplot(g27,aes(fill=Licencespkm2))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="Licence/km2") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    # Save the plot with the specified dimensions
    nm = paste('LFA',LFA1,'- Logbook reported number of licences fishing by grid corrected by area (km2).png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
    
    g27$CPUE = g27$Landings/g27$TrapHauls
    ok1 = ggplot(g27,aes(fill=CPUE))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral', name="kg/TrapHaul") +
        facet_wrap(~FishingYear)+
        #geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                 ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    nm = paste('LFA',LFA1,'- Logbook reported catch (kg) per unit effort (trap haul) by grid.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
   
    nameref=paste0("LFA",LFA1,"_grid_data.csv")
    file.to.save=sf::st_drop_geometry(g27)
    write.csv(file.to.save, file=nameref) 
    
    #Something still broken below. BZ  
    
    #Trends
    g27 = subset(gTot,LFA==LFA1 & FishingYear>2005 )
    g27p = subset(gTot,LFA==LFA1 & FishingYear>2005 &PrivacyScreen==1)
    g27n = subset(gTot,LFA==LFA1 & FishingYear>2005 &PrivacyScreen==0)
    
    g27$CPUE = g27$Landings/g27$TrapHauls
    
    ui = unique(g27$GRID_NO)
    out = list()
    j=0
    for(i in 1:length(ui)){
        b=subset(g27,GRID_NO==ui[i])
        bp = subset(g27,GRID_NO==ui[i]& FishingYear==max(b$FishingYear))
        #bp=aggregate(CPUE~FishingYear,data=b,FUN=mean)
        base = mean(bp$CPUE[which(bp$FishingYear %in% 2006:2010)])
        p4b = p3b = p2b = p1b = b1
        p1b$PercentDiff = percentDifference(c(mean(bp$CPUE[6:9]),base))
        p2b$PercentDiff = percentDifference(c(mean(bp$CPUE[10:13]),base))
        p3b$PercentDiff = percentDifference(c(mean(bp$CPUE[14:18]),base))
        p4b$PercentDiff = percentDifference(c(mean(bp$CPUE[17:19]),base))
        p1b$label = '2011-2014'
        p2b$label = '2015-2018'
        p3b$label = '2019-2020'
        p4b$label = '2020-2023'
        if(nrow(bp)<14){
            p1b$PercentDiff =p2b$PercentDiff = p3b$PercentDiff = p4b$PercentDiff = NA
        }
        bp = do.call(rbind,list(p1b,p2b,p3b,p4b))
        out[[i]] = bp
    }    
   
    o = do.call(rbind,out)
    
  
    
    ok1 = ggplot(o,aes(fill=PercentDiff))+
        geom_sf() +
        scale_fill_distiller(trans='identity',palette='Spectral') +
        facet_wrap(~label)+
        ##geom_sf(data=g27n,fill='white')+  
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
        coord_sf(xlim = c(st_bbox(o)$xmin,st_bbox(o)$xmax),
                 ylim = c(st_bbox(o)$ymin,st_bbox(o)$ymax),
                 expand = FALSE)+
        scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
        scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
    
    
    #nm = paste('LFA',LFA1,'- Logbook reported grid estimates of percent difference in catch per unit effort (CPUE) relative to mean CPUE between 2006 and 2010.png',sep="")
   # ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 

   
    }





makeNewFootPrints(x='29',data=gTot)
makeNewFootPrints(x='34',data=gTot)


