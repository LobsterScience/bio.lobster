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

figdir = file.path(project.datadirectory("bio.lobster","requests","gulf.whales"))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
setwd(figdir)

# Fishery footprint
#------------------------------------------------------------


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
    
    #############################################
    #determine the proration for each grid for each year (what % of total LFA traphauls)

    # Calculate the percentage of BTTH by GRID_NUM within each SYEAR and LFA
    perc_th_result <- partEffort %>%
        group_by(SYEAR, LFA) %>%
        mutate(total_BTTH_in_LFA = sum(BTTH)) %>%
        mutate(perc_th = (BTTH / total_BTTH_in_LFA) * 100) %>%
        ungroup() %>%
        # Use dplyr::select() explicitly to avoid conflicts
        dplyr::select(GRID_NUM, LFA, SYEAR, perc_th)
    
    # Merge the new perc_th column back into the original partEffort dataframe
    partEffort_with_perc_th <- partEffort %>%
        left_join(perc_th_result, by = c("GRID_NUM", "LFA", "SYEAR"))
    
    # Step 1: Calculate the maximum sum of NUM_OF_TRAPS for each DATE_FISHED within each LFA and SYEAR
    max_traps_per_day <- a %>%
        group_by(LFA, SYEAR, DATE_FISHED) %>%
        summarise(sum_num_of_traps = sum(NUM_OF_TRAPS), .groups = 'drop') %>%
        group_by(LFA, SYEAR) %>%
        summarise(max_sum_num_of_traps = max(sum_num_of_traps), .groups = 'drop')
    
    # Step 2: Merge this result into partEffort_with_perc_th by LFA and SYEAR
    partEffort_with_traps <- partEffort_with_perc_th %>%
        left_join(max_traps_per_day, by = c("LFA", "SYEAR"))
    
    # Step 3: Create the new column 'traps_fished' by multiplying perc_th by max_sum_num_of_traps
    partEffort_with_traps <- partEffort_with_traps %>%
        mutate(traps_fished = (perc_th/100) * max_sum_num_of_traps)

   #########################################################
   #Save 
        
    # Create the new tibble grid.trap.data with the relevant columns and filter SYEAR between 2013 and 2017
    grid.trap.data <- partEffort_with_traps %>%
        dplyr::select(LFA, GRID_NUM, SYEAR, traps_fished) %>%
        dplyr::rename(grid = GRID_NUM) %>%
        dplyr::filter(SYEAR >= 2013 & SYEAR <= 2017)  # Filter for SYEAR between 2013 and 2017
 
    #get polygons for grids 
    layerDir=file.path("C:","bio","bio.lobster.data", "mapping_data")
    r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
    
    r_clean <- r %>%
        dplyr::select(-V2, -grid) %>%  # Remove V2 and grid columns
        dplyr::rename(grid = GRID_NO)  # Rename GRID_NO to grid       

    saveRDS(grid.trap.data, file="grid.trap.data.RDS")  
    saveRDS(r_clean, file="grid.geometry.RDS")
  