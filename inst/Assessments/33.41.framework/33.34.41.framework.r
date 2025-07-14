require(bio.lobster)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(dplyr)
require(sf)
require(PBSmapping)
require(grid)
require(patchwork)

p = bio.lobster::load.environment()
p$lfas=c("33", "34")

la()

#adjust as required
assessment.year = "2024" 

figdir = file.path(project.datadirectory("bio.lobster","assessments","33.41.framework","2025"))

figdir = "C:/Users/zissersonb/OneDrive - DFO-MPO/Cook, Adam (DFO_MPO)'s files - LFA33_34_41_Framework/Documents/Figures/"
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
setwd(figdir)

layerDir=file.path(code_root,"bio.lobster.data", "mapping_data")

# update data through ROracle

#If you only want to update logs for the last two years, run this:
#p$yr=p$current.assessment.year


NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  lobster.db('seasonal.landings.redo')
  #lobster.db('vlog.redo') #These are static now, no need to update
  logs=lobster.db('season.dates.redo') #updates season dates as required
  logs=lobster.db('process.logs.redo')
  
  per.rec= lobster.db("percent_reporting")
}

# Map
#----------------------------------------

png(filename=file.path(figdir, "map.33-41.png"),width=5, height=5, units = "in", res = 800)
LobsterMap('33-41')
dev.off()

# Landings


theme_csas <- function(base_size = 11, base_family = "", text_col = "grey20",
                       panel_border_col = "grey70") {
    half_line <- base_size / 2
    theme_light(base_size = base_size, base_family = "") +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.length = unit(half_line / 2.2, "pt"),
            strip.background = element_rect(fill = NA, colour = NA),
            strip.text.x = element_text(colour = text_col),
            strip.text.y = element_text(colour = text_col),
            axis.text = element_text(colour = text_col),
            axis.title = element_text(colour = text_col),
            legend.title = element_text(colour = text_col, size = rel(0.9)),
            panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
            legend.key.size = unit(0.9, "lines"),
            legend.text = element_text(size = rel(0.7), colour = text_col),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.background = element_rect(colour = NA, fill = NA),
            plot.title = element_text(colour = text_col, size = rel(1)),
            plot.subtitle = element_text(colour = text_col, size = rel(.85))
        )
}

# CPUE / Landings / Effort

for (l in p$lfas){

land = lobster.db('seasonal.landings')
land$YEAR = as.numeric(substr(land$SYEAR,6,9))
land$LANDINGS <- land[[sprintf("LFA%s", l)]]

CPUE.data<-CPUEModelData2(p,redo=T)
cpueData= CPUEplot(CPUE.data,lfa= l,yrs=1981:max(land$YEAR),graphic='R')$annual.data

fishData = merge(cpueData,land[,c("YEAR","LANDINGS")])
fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

aaa=fishData
aaa = aaa[1:(nrow(aaa)-1),] #Full data without final year


ymax=12000
scaleright = max(aaa$EFFORT2)/ymax

g1 <- ggplot(data = aaa, aes(x = YEAR,y=LANDINGS)) +
    geom_bar(stat='identity',fill='black') +
    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',shape=16)+
    geom_line(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',linetype='dashed')+
    scale_y_continuous(name='Landings', sec.axis= sec_axis(~.*scaleright/1000, name= 'Effort',breaks = seq(0,(max(aaa$EFFORT2/scaleright)*5),by=5000)))+
    labs(x = "Year") +
    theme_csas()


g2 <- ggplot(data = aaa, aes(x = YEAR)) +
    geom_line(aes(y=CPUE),colour='black',linetype='dashed')+
    geom_point(aes(y = CPUE),size=2.5) +
    ylim(0, 1.5)+
    labs(x = "Year", y = " CPUE") +
    theme_csas() 


#Effort Example

library(dplyr)
library(ggplot2)

# Step 1: Median fishing days per VR_NUMBER per year (2005–2024)
fishing_days_summary <- logs %>%
    filter(SYEAR >= 2005, SYEAR < 2025) %>%
    group_by(SYEAR, VR_NUMBER) %>%
    summarize(fishing_days = n_distinct(DATE_FISHED), .groups = "drop") %>%
    group_by(SYEAR) %>%
    summarize(median_fishing_days = median(fishing_days, na.rm = TRUE))

# Step 2: Total NUM_OF_TRAPS per year (sum across all VR_NUMBERs)
trap_summary <- logs %>%
    filter(SYEAR >= 2005, SYEAR < 2025) %>%
    group_by(SYEAR) %>%
    summarize(total_traps = sum(NUM_OF_TRAPS, na.rm = TRUE))

# Step 3: Join both summaries
combined_data <- fishing_days_summary %>%
    left_join(trap_summary, by = "SYEAR")

# Step 4: Scaling factor for secondary axis
# Choose a factor so both lines fit nicely (trial-based or using max ratios)
scale_factor <- max(combined_data$median_fishing_days) / max(combined_data$total_traps)

# Step 5: Plot with dual axis
ggplot(combined_data, aes(x = SYEAR)) +
    # Primary y-axis: Median fishing days
    geom_line(aes(y = median_fishing_days), color = "steelblue") +
    geom_point(aes(y = median_fishing_days), color = "steelblue", size = 3) +
    
    # Secondary y-axis: Total traps scaled to overlay
    geom_line(aes(y = total_traps * scale_factor), color = "firebrick") +
    geom_point(aes(y = total_traps * scale_factor), color = "firebrick", size = 3) +
    
    scale_y_continuous(
        name = "Median Number of Fishing Days",
        limits = c(0, 70),
        sec.axis = sec_axis(~ . / scale_factor, name = "Total Number of Traps")
    ) +
    labs(
        title = "Fishing Days (Median per Vessel) and Total Traps by Year (2005–2024)",
        x = "Year"
    ) +
    theme_minimal() +
    theme(
        axis.title.y.left = element_text(color = "steelblue"),
        axis.text.y.left = element_text(color = "steelblue"),
        axis.title.y.right = element_text(color = "firebrick"),
        axis.text.y.right = element_text(color = "firebrick")
    )

# Fishery footprint-
#------------------------------------------------------------
{
r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
r = st_as_sf(r)

#get logs
a =  lobster.db('process.logs')
a = subset(a,SYEAR>=2010 & SYEAR<assessment.year) #subsetting data to 2010-2024 (2025 still has outstandinmg logs, etc)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:assessment.year
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=assessment.year)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
num.yr=length(2005:assessment.year)
b$LFA=rep(c(33,34,35,36,38),each=num.yr)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<=assessment.year, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
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
Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)


saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')

Tot = readRDS('PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

GrMap = readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
GrMap1 = GrMap

GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
attr(GrMap1$area, "units") <- "km^2"
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


gTot$CPUE= as.numeric(gTot$Landings)/as.numeric(gTot$TrapHauls)

library(dplyr)

# Perform left join on LFA and FishingYear
gTot <- gTot %>%
    group_by(FishingYear, LFA) %>%
    mutate(Prop.lfa.land = Landings / sum(Landings) *100) %>%
    ungroup()  # Ungroup to prevent unintended behavior in further operations


g27p = gTot
g27p <- g27p %>%
    mutate(LFA = case_when(
        LFA == "311" ~ "31A",
        LFA == "312" ~ "31B",
        TRUE ~ LFA  # Keep other values unchanged
    ))
g27p$Landings.t=g27p$Landings/1000

#add privacy screen

g27p = subset(g27p, select = -c(PrivacyScreen, V2, grid) ) #check that this works

g27p= subset(g27p, LFA %in% c("33", "34") )


# Following is mapping code to test above if needed
#----------------------------------------------------
#r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
#b=r
#o=GrMap2
#ggplot(b)+
#  geom_sf()+
#  geom_sf(data=coa,fill='grey')+
#  geom_sf(data=o,fill='red')+
#  coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
#  ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
#  expand = FALSE)


#Figure Creation

ok1 = function(x = g27p, fill_var = "CPUE", log_scale = FALSE, LFA="all") {
    # Subset by LFA only if it's not "all" or NULL
    if (!is.null(LFA) && LFA != "all") {
        x <- x %>% filter(LFA == !!LFA)
    }
    
    
    # Apply log transformation if requested
    if (log_scale) {
        x[[fill_var]] <- log(x[[fill_var]] + 1)  # Adding 1 to avoid log(0) issues
    }
    
    ggplot(x, aes(fill = !!sym(fill_var))) +
        geom_sf() +
        scale_fill_distiller(trans = 'identity', palette = 'Spectral') +
        facet_wrap(~FishingYear) +
        geom_sf(data = coa, fill = 'grey') +
        geom_sf(data = GrMap, fill = NA) +
        coord_sf(
            xlim = c(st_bbox(x)$xmin, st_bbox(x)$xmax),
            ylim = c(st_bbox(x)$ymin, st_bbox(x)$ymax),
            expand = FALSE
        ) +
        scale_x_continuous(breaks = c(round(seq(st_bbox(x)$xmin, st_bbox(x)$xmax, length.out = 2), 2))) +
        scale_y_continuous(breaks = c(round(seq(st_bbox(x)$ymin, st_bbox(x)$ymax, length.out = 2), 2)))
}

## Might want to send directly to One Drive??



#Loops to generate maps for all fisheries data for 33 & 34, separately and combined

mapdir=file.path(figdir,"33.34.fishery.maps")
dir.create( mapdir,recursive = TRUE, showWarnings = FALSE )


vars=c("Landings", "Landings.t", "CPUE","Trips","NLics", "TrapHauls","Prop.lfa.land")
lvars=c("33", "34","all")

for (var in vars){
    for (lvar in lvars){

ok1(g27p, fill_var = var, LFA=lvar)

png(filename=file.path(mapdir, lvar, paste0("grid.",var,".",lvar,".png")), width=1200, height=1050, res=200)
print(ok1(g27p, fill_var = var, LFA=lvar,log_scale = FALSE))
dev.off()
}
}

#For Landings, will also use log scale to make area more comparable

lvars=c("33", "34","all")
var="Landings"


    for (lvar in lvars){
        
        ok1(g27p, fill_var = var, LFA=lvar)
        
        png(filename=file.path(mapdir, lvar, paste0("grid.",var,".",lvar,".log.scaled.",".png")), width=1200, height=1050, res=200)
        print(ok1(g27p, fill_var = var, LFA=lvar,log_scale = TRUE))
        dev.off()
    }

}

