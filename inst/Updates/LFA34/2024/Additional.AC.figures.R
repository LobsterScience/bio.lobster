# LFA 34 Script to create some figures for AC presentation. Others created by Adam through assessment update.

p = bio.lobster::load.environment()
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)
require(dplyr)
la()



#If running script after the fishery year, run this line
#p$current.assessment.year=p$current.assessment.year-1


# define place for figures to go
figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA34",p$current.assessment.year),"AC_Pres")
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
p$lfas = c("34") # specify lfas for data summary

setwd(figdir)

#If you only want to update logs for the last two years, run this:
#p$yr=p$current.assessment.year

# update data through ROracle
NewDataPull =F
#NewDataPull =T

if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  lobster.db('seasonal.landings.redo')
  #lobster.db('vlog.redo')
  logs=lobster.db('process.logs.redo')
  per.rec= lobster.db("percent_reporting")
}

#Run a report of missing vs received logs and save a csv copy

fl.name=paste("percent_logs_reported", Sys.Date(),"csv", sep=".")
per.rec= lobster.db("percent_reporting")
per.rec <- per.rec[order(per.rec$YEARMTH), ]
p1 <- per.rec[
    substr(per.rec$YEARMTH, 1, 4) == as.character(p$current.assessment.year-1) &
        substr(per.rec$YEARMTH, 5, 6) %in% c("11", "12"),
]
p2<- per.rec[grepl(p$current.assessment.year, per.rec$YEARMTH), ]
per.rec=rbind(p1, p2)
columns_to_keep <- c("YEARMTH", grep("L33|L34", colnames(per.rec), value = TRUE))
per.rec <- per.rec[, columns_to_keep]
per.rec <- per.rec[!apply(per.rec[, -which(names(per.rec) == "YEARMTH")], 1, function(x) all(is.na(x))), ]


L33_miss <- sum(per.rec$L33MISS)
L33_recd <- sum(per.rec$L33RECD)
L33_percent <- 100 * L33_miss / (L33_miss + L33_recd)

L34_miss <- sum(per.rec$L34MISS, na.rm = TRUE)
L34_recd <- sum(per.rec$L34RECD, na.rm = TRUE)
L34_percent <- 100 * L34_miss / (L34_miss + L34_recd)

summary_tbl <- data.frame(
    LobsterArea = c("L33", "L34"),
    Miss = c(L33_miss, L34_miss),
    Recd = c(L33_recd, L34_recd),
    PercentMiss = c(L33_percent, L34_percent)
)

summary_tbl


# Make per.rec into character data frame
per.rec_out <- per.rec %>%
    mutate(across(everything(), as.character))

# Add a blank row for separation
blank_row <- as.data.frame(matrix("", ncol = ncol(per.rec_out)))
colnames(blank_row) <- colnames(per.rec_out)

# Make summary into the same shape by adding missing columns
summary_out <- summary_tbl %>%
    mutate(across(everything(), as.character)) %>%
    # add missing cols as blanks so it can bind
    tibble::add_column(
        YEARMTH = "",
        L33MISS = "",
        L33RECD = "",
        L33PERCENT = "",
        L34MISS = "",
        L34RECD = "",
        L34PERCENT = "",
        .before = 1
    )

# Bind everything together
final_out <- bind_rows(per.rec_out, blank_row, summary_out)
write.csv(final_out, file=paste0(figdir,"/",fl.name),na="", row.names=F)


# Map ################

png(filename=file.path(figdir, "MapLFA34.png"),width=5, height=5, units = "in", res = 800)
LobsterMap('34')
dev.off()


# CPUE ###############

logs=lobster.db("process.logs")

h = lobster.db('annual.landings')
i = lobster.db('seasonal.landings')
g = lobster.db('process.logs')

ref = data.frame(LFA=c(27:30,'31A','31B',32,33),lrp=c(.14,.12,.11,.28,.16,.16,.14,.14),usr=c(.27,.25,.22,.56,.41,.32,.29,.28))

g = subset(g, SYEAR<=p$current.assessment.year)

#bring in voluntary log data to populate <2005
fn.root =  file.path( project.datadirectory('bio.lobster'), "data")
fnODBC  =  file.path(fn.root, "ODBCDump")
get.vlog=load(file.path( fnODBC, "processed.vlog.rdata"),.GlobalEnv)
v = subset(vlog,SYEAR<2005, select=c("SYEAR","W_KG","N_TRP","LFA"))
names(v)=c("SYEAR","WEIGHT_KG","NUM_OF_TRAPS","LFA")
v$LFA[v$LFA%in%c("27N","27S")] = "27"
v$LFA[v$LFA%in%c("33W","33E")] = "33"
va = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=v,FUN=sum)

gag = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=g,FUN=sum)
ga=rbind(va, gag)
ga$cpue = ga$WEIGHT_KG/ga$NUM_OF_TRAPS
l = unique(ga$LFA)
o = list()
for(j in 1:length(l)){
    n = subset(ga,LFA==l[j])
    running.median = with(rmed(n$SYEAR,n$cpue),data.frame(SYEAR=yr,running.median=x))
    o[[j]]=merge(n,running.median,all=T)
}
o = dplyr::bind_rows(o)


crd= subset(o,LFA==p$lfas[1])	
names(crd)=c("YEAR", "LFA", "NUM_OFTRAPS","WEIGHT_KG", "CPUE", "running.median")
crd = crd[is.finite(crd$CPUE),]


png(filename=file.path(figdir, "CPUE_LFA34.png"),width=8, height=5.5, units = "in", res = 800)
par(mar=c(4.0,5.5,2.0,3.0))
xlim=c(1990,max(crd$YEAR))
plot(crd$YEAR,crd$CPUE,xlab='Year',ylab='CPUE (kg/TH)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
dev.off()

write.csv(crd,file.path(figdir,file='CatchRateRefs34.csv'))


# Plots unbiased annual CPUE for all LFAs in Maritimes region
# Good for context in presentations at AC

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 

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

#-----------------------------------------
{   
    
    ##by week
    a = lobster.db('process.logs')
    a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
    a=subset(a, LFA %in% p$lfas)
    
    
    aa = split(a,f=list(a$LFA,a$SYEAR))
    aa = rm.from.list(aa)
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
    
    mean= aggregate(cc, CPUE~yr+lfa,mean )
    
    
    l=p$lfas
    png(filename=file.path(figdir, paste0("weekly_cpue_lfa34.png")),width=8, height=5.5, units = "in", res = 800)
    print(
        ggplot(subset(cc, lfa == l), aes(x = t, y = CPUE)) +
            geom_point(size = 0.8, alpha = 0.6) +  # smaller semi-transparent points
            geom_smooth(se = FALSE, color = "blue", linewidth = 0.3) +  # thin blue trend line
            facet_wrap(~yr) +
            labs(
                title = paste0("LFA ", l),
                y = "CPUE (kg/trap haul)",
                x = "Week of Season"
            ) +
            geom_hline(
                data = subset(mean, lfa == l),
                aes(yintercept = CPUE),
                color = "red",
                linetype = "dashed",
                linewidth = 0.4
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
                axis.title = element_text(size = 11),
                strip.text = element_text(size = 9)
            )
    )
    dev.off()
    
}


#######-----------------------------------------


# Landings and Effort ############

    land = lobster.db('seasonal.landings')
    
    
    #if running this section without having done the CPUE analysis during the same session, run the line below 
    #load (file=paste0(figdir, "/cpueData.Rdata") )
    
    
    land$YEAR = as.numeric(substr(land$SYEAR,6,9))
    land$LANDINGS = land$LFA34
    fishData = merge(crd,land[,c("YEAR","LANDINGS")])
    fishData$EFFORT2 = fishData$LANDINGS * 1000 / crd$CPUE
    
    
    
    # plot Landings
    
    png(filename=file.path(figdir, "Landings_LFA34.png"),width=8, height=5, units = "in", res = 800)
    par(mar = c(5.1, 5.6, 4.1, 5.1), las = 1, mgp = c(4, 0.8, 0), cex.axis = 0.8)
    
    # --- Landings (bars) ---
    plot(fishData$YEAR, fishData$LANDINGS,
         xlab = "Year", ylab = "Landings (t)", type = "h", main = "LFA 34",
         ylim = c(0, max(fishData$LANDINGS, na.rm = TRUE) * 1.2), col = "grey", lwd = 10, lend = 3)
    
    # Highlight most recent year
    lines(
        rep(max(fishData$YEAR), 2), c(0, tail(fishData$LANDINGS, 1)),
        type = "l", col = rgb(1, 0.6, 0), lwd = 10, lend = 3
    )
    dev.off()
    
    # plot Landings / Effort Together
    
    png(filename = file.path(figdir, "Landings_Effort_LFA34.png"),
        width = 8, height = 5, units = "in", res = 800)
    
    # Increase left margin and adjust spacing between labels & title
    # cex.axis = 0.8 → smaller axis text
    # mgp = c(4, 0.8, 0) → shifts y-axis title farther from axis
    par(mar = c(5.1, 5.6, 4.1, 5.1), las = 1, mgp = c(4, 0.8, 0), cex.axis = 0.8)
    
    # --- Landings (bars) ---
    plot(fishData$YEAR, fishData$LANDINGS,
         xlab = "Year", ylab = "Landings (t)",
         type = "h", main = "LFA 34",
         ylim = c(0, max(fishData$LANDINGS, na.rm = TRUE) * 1.2),
         col = "grey", lwd = 10, lend = 3)
    
    # Highlight most recent year
    lines(
        rep(max(fishData$YEAR), 2),
        c(0, tail(fishData$LANDINGS, 1)),
        type = "l", col = rgb(1, 0.6, 0), lwd = 10, lend = 3
    )
    
    # --- Effort (right axis) ---
    par(new = TRUE)
    plot(fishData$YEAR, fishData$EFFORT2 / 1000,
         ylab = "", xlab = "", type = "b",
         pch = 16, axes = FALSE,
         ylim = c(0, max(fishData$EFFORT2 / 1000, na.rm = TRUE)),
         cex.axis = 0.8
    )
    points(max(fishData$YEAR),
           tail(fishData$EFFORT2, 1) / 1000,
           pch = 21, bg = rgb(1, 0.6, 0)
    )
    axis(4, cex.axis = 0.8)
    mtext("Effort ('000s Trap Hauls)", side = 4, line = 3.5, las = 0, cex = 0.9)
    
    dev.off()
   
    write.csv(fishData,file.path(figdir,paste('FisheryPlot3.csv',sep='')))
  
    
    # to compare weekly fishing effort year to year (include in AC presentation if desired)
    logs=lobster.db("process.logs")
    
    logs34=logs[logs$LFA=="34",]
    logs34$unique_days=paste(logs34$LICENCE_ID, logs34$DATE_FISHED, sep=':')
    
    #-----------------------------------------------------------------------
    
       # to plot weekly fishing effort year to year (include in AC presentation if desired)
    png(filename=file.path(figdir, "Weekly_Comparison_effort.png"),width=8, height=5.5, units = "in", res = 1200)
    days=aggregate(unique_days~WOS+SYEAR, data=logs34, length)
    days.y0=days[days$SYEAR==max(days$SYEAR),]
    days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
    plot(x=days$WOS,y=days$unique_days, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(0,27))
    #plot past 10 years in light gray)
    for (i in c(0:10)){
        day=days[days$SYEAR==(max(days$SYEAR)-i),]  
        lines(day$WOS, day$unique_days, col="gray83") 
    }
    lines(days.y0$WOS, days.y0$unique_days, col="red")
    #text(paste(days.y0$SYEAR[1]), x=26, y=300, col="red", cex=1.5)
    lines(days.y1$WOS, days.y1$unique_days, col="blue")
    #text(paste(days.y1$SYEAR[1]), x=26, y=800, col="blue", cex=1.5)
    legend(x=20, y=6500, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray88", "blue", "red"), bty='n')
    
    dev.off()
    
    # to plot weekly fishing CPUE year to year (include in AC presentation if desired)
    png(filename=file.path(figdir, "Weekly_Comparison_cpue.png"),width=8, height=5.5, units = "in", res = 1200)
    days=aggregate(CPUE~WOS+SYEAR, data=logs34, FUN="median")
    days.y0=days[days$SYEAR==max(days$SYEAR),]
    days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
    plot(x=days$WOS,y=days$CPUE, type='n', main= "Average CPUE by Week", xlab="Week of Season", ylab="CPUE (kg/trap)", xlim=c(0,27))
    for (i in c(0:10)){
        day=days[days$SYEAR==(max(days$SYEAR)-i),]  
        lines(day$WOS, day$CPUE, col="gray82")
    }
    #axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
    lines(days.y0$WOS, days.y0$CPUE, col="red")
    #text(paste(days.y0$SYEAR[1]), x=26, y=1, col="red", cex=1.5)
    lines(days.y1$WOS, days.y1$CPUE, col="blue")
    #text(paste(days.y1$SYEAR[1]), x=26, y=1.4, col="blue", cex=1.5)
    legend(x=20, y=4.5,cex=1, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray82", "blue", "red"), bty='n')
    dev.off()
    




    # Fishery footprint- Useful in comparing years, etc
    #------------------------------------------------------------
    
    
    layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
    r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
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
    sL = rm.from.list(sL)
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
    
    #Remove grids fall under "rule of 5"
    Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)
    Tot=subset(Tot, PrivacyScreen==1)
    
    saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
    
    Tot = readRDS('PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
    Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
    Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)
    
    
    #making plots of Tot
    
    GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
    coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
    
    
    GrMap1 = GrMap
    GrMap1$area = st_area(GrMap1)/1000000
    GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
    st_geometry(GrMap1)<- NULL
    gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))
    
    GrMap2 =merge(GrMap,gg)
    
    gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)
    
    
    r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
    b=subset(r,LFA %in% c(27:33, 311, 312))
    
    o=subset(GrMap,LFA %in% c(27:33, 311, 312))
    
    ggplot(b)+
        geom_sf()+
        geom_sf(data=coa,fill='grey')+
        geom_sf(data=o,fill='red')+
        coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
                 ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
                 expand = FALSE)
    
    
    gTot$CPUE = gTot$Landings/gTot$TrapHauls
    
    #Can run this line to only take certain LFAs / years
    g27p = subset(gTot, LFA%in% p$lfas & FishingYear%in%2016:p$current.assessment.year)
    
    ok1 = ggplot(subset(g27p, FishingYear == p$current.assessment.year), aes(fill = CPUE)) +
        geom_sf() +
        scale_fill_distiller(trans = 'identity', palette = 'Spectral') +
        geom_sf(data = coa, fill = 'grey') +
        geom_sf(data = GrMap, fill = NA) +
        coord_sf(
            xlim = c(st_bbox(g27p)$xmin, st_bbox(g27p)$xmax),
            ylim = c(st_bbox(g27p)$ymin, st_bbox(g27p)$ymax),
            expand = FALSE
        ) +
        theme_grey(base_size = 12) +
        theme(
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "grey96", colour = NA)  # lighter grey background
        )
    
    output_png <- file.path(figdir, "cpue_grid.png")
    png(filename = output_png, width = 1200, height = 900, res = 175)
    print(ok1)
    dev.off()
    
    #Slice out individual years
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
    
    
    library(ggplot2)
    library(sf)
    library(grid)
    
    # -----------------------------
    # 1️⃣ Create label
    # -----------------------------
    lab <- paste0(gl$FishingYear[1], " -> ", gp$FishingYear[1])
    
    # -----------------------------
    # 2️⃣ Build the original map
    # -----------------------------
    cpue.diff = {
        ggplot(subset(gg, PrivacyScreen == 1), aes(fill = percentChange)) +
            geom_sf() +
            scale_fill_continuous_diverging(palette = "Purple-Green") +
            labs(fill = "CPUE\n% Change") +
            geom_sf(data = coa, fill = "grey") +
            geom_sf(data = GrMap, fill = NA) +
            coord_sf(
                xlim = c(st_bbox(g27p)$xmin, st_bbox(g27p)$xmax),
                ylim = c(st_bbox(g27p)$ymin, st_bbox(g27p)$ymax),
                expand = FALSE
            ) +
            theme_grey(base_size = 12) +
            theme(
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                panel.background = element_rect(fill = "grey85", colour = NA),  # lighter grey background
                plot.margin = grid::unit(c(8, 2, 8, 2), "mm")
            ) +
            annotate(
                "text",
                x = (st_bbox(g27p)$xmax) - 0.9,
                y = (st_bbox(g27p)$ymin) + 0.1,
                label = lab,
                size = 8
            )
    }
    
    
    # -----------------------------
    # 3️⃣ Export PDF and overlay label
    # -----------------------------
    output_png <- file.path(figdir, "cpue.diff.png")
    
    png(filename = output_png, width = 1200, height = 900, res = 175)
    print(cpue.diff)
   
    dev.off()
    