
# Feb 2017
# CPUE spatial temporal trend in BoF


#######################################################################
# Set the working directory & read the data

#setwd("C:/Users/gaudettej/Documents/1-Lobster Folder/LFA36 - Fishing Season Adjustment")

# Import the data from a tab delimited ascii file
#CPUEraw <- read.csv("C:/Users/gaudettej/Documents/1-Lobster Folder/LFA36 - Fishing Season Adjustment/LFA35_38_MARFIS_LOGS.csv",header=TRUE)
#Grid_Group <- read.csv("C:/Users/gaudettej/Documents/1-Lobster Folder/LFA36 - Fishing Season Adjustment/Grid_Group.csv",header=TRUE)

fp=file.path(project.datadirectory('bio.lobster'),'data','FNFASeasonExt')

CPUEraw <- read.csv(file.path(fp,"LFA35_38_MARFIS_LOGS.csv"),header=TRUE)
Grid_Group <- read.csv(file.path(fp,"Grid_Group.csv"),header=TRUE)




#Library Loading  #######################################################
# Load packages from R and support functions that we wrote
library(lattice)  
library(mgcv)
library(ggplot2)
library(MASS) #for NLS regression and predict function
library(magrittr) #for Pipping/Chaining
library(dplyr)
library(tidyr)
#library()
library(nls2) #for plotting confidence intervals with nls
library(lubridate) #formatting and extracting date functions
source("C:/Highstat/HighstatLibV10.R")# Library Loadin


#Inspect the file
names(CPUE)
str(CPUE)  #date is in the wrong format



#Housekeeping and some QA/QC########################################################

#Tidying the date.frame to have one observation per row (i.e. one CPUE for a given fishing trip)
#In addition, need convert catch weight from lbs to KG
#And need to create a "Grid_Group" factor


sub_grid <- CPUEraw %>% gather(key="GRID_abc",value="GRID",starts_with("GRID"))
sub_th <- CPUEraw %>% gather(key="TH_abc", value="TH", starts_with("TH"))
sub_weight <- CPUEraw %>% gather(key="WEIGHT_abc", value="WEIGHT_lbs", starts_with("WEIGHT"))

CPUE <- data.frame(select(sub_grid, -c(6:11)),select(sub_th, TH),select(sub_weight,WEIGHT_lbs)) #joining data.frames

CPUE <-  inner_join(CPUE,Grid_Group, by="GRID") #Creating a new factor called Grid_Group


#DATE: Need to properly format column DATE with lubridate package
CPUE$DATE <- ymd(CPUE$DATE) #lubridate method


CPUE <- CPUE %>% mutate(cpue_entry  = WEIGHT_lbs/TH,
                        year     =    year(DATE),
                        month    =    month(DATE),
                        week  = week(DATE),
                        yday     =    yday(DATE))
                 #mutate() %>%
                 #mutate(week  = week(DATE)) %>%
                 #
                 

#Convert LFA,  GRID, YEAR as factor
CPUE$GRID<- as.factor(CPUE$GRID)
CPUE$LFA<-as.factor(CPUE$LFA)
#CPUE$year<-as.factor(CPUE$year)

#Removing Non-assigned values:

cCPUE <- CPUE[complete.cases(CPUE),] #dataset with complete values only


#Some data glitches: 1) "out of season" fishing 
 

cCPUE2<-cCPUE[!( ( cCPUE$LFA=="38"                         &  (cCPUE$month>=7 & cCPUE$month<11))  | 
                 ((cCPUE$LFA=="36" & cCPUE$year!="2015")   &  (cCPUE$month>=7 & cCPUE$month<11))  |   #NB: Seasonbal extension in 2015 for LFA36
                 ( cCPUE$LFA=="36"                         &  (cCPUE$yday>=14 & cCPUE$month< 4))  |   #Winter closure LFA36 (Jan 14 to March 31)
                 ( cCPUE$LFA=="35"                         &  (cCPUE$month>=8 & cCPUE$yday<287))  |   #Summer closure LFA35 (Aug 1st to Oct 14)
                 ( cCPUE$LFA=="35"                         &  (cCPUE$month<3))  ),]                   #Winter closure LFA35 (Jan 1st to last day of Feb)

#Some data glitches: 2) grids in wrong LFA (e.g. 50 and 40 are sometimes link to LFA35)
cCPUE3<-cCPUE2[!((cCPUE2$Grid_Group=="GrandManan_Coast" | cCPUE2$Grid_Group=="LowBOF_Deep" | cCPUE2$Grid_Group=="SWNB_Coast") & cCPUE2$LFA=="35"),] #should better test that for each LFA
#Remove improper log entries from NB_UpperCoast and LFA36 and LFA38 (except for grid 6 which LFA36 can fish)
cCPUE4<-cCPUE3[!((cCPUE3$Grid_Group=="NB_UpperCoast" & cCPUE3$GRID!=6) & (cCPUE3$LFA=="36" | cCPUE3$LFA=="38")),] 
cCPUE5<-cCPUE4[!((cCPUE4$Grid_Group=="NB_UpperCoast" & cCPUE4$GRID==6) & (cCPUE4$LFA=="38")),]

cCPUE6<-cCPUE5[!((cCPUE5$Grid_Group=="NovaS_UpperCoast" | cCPUE5$Grid_Group=="NovaS_MidCoast") & (cCPUE5$LFA=="36" | cCPUE5$LFA=="38")),] 
okCPUE<-cCPUE6[!((cCPUE6$Grid_Group=="MidBOF_Deep" | cCPUE6$Grid_Group=="UpBOF_Deep") & cCPUE6$LFA=="38"),] 


#Trap Limits per LFAs taking into account Cat. A Partnerships that allows 1.5x TLim. LFA35 has 2, LFA36 has 26, LFA38 has 54##########


PartLic3536<- okCPUE %>% filter (LFA!=38) %>% group_by(LICENCE,DATE,LFA, year) %>% 
                                                  summarise (THdt=sum(TH)) %>% 
                                                  filter(THdt>=450) %>% group_by(LFA,year) %>%  #filter(THdt>300 | THdt<450)
                                                  summarise(n_PartLic= n_distinct(LICENCE))

               #LFA     n_PartLic reporting 450 Traps in LFAs35-36
               #1	35	 23
               #2	36 	 26

PartLic38<- okCPUE %>% filter (LFA==38) %>% group_by(LICENCE,DATE,LFA, year) %>% 
                                                  summarise (THdt=sum(TH)) %>% 
                                                  filter(THdt>=550) %>% group_by(LFA,year) %>%  #filter(THdt>375 | THdt<560)
                                                  summarise(n_PartLic= n_distinct(LICENCE))


               #   LFA	year	 n_PartLic reporting 550 Traps in LFA38
               #1	38	2005	 2
               #2	38	2006	 6
               #3	38	2007	 6
               #4	38	2008	 18
               #5	38	2009	 9
               #6	38	2010	 19
               #7	38	2011	 21
               #8	38	2012	 17
               #9	38	2013	 31
               #10	38	2014	 40
               #11	38	2015	 34
               #12	38	2016	 32


#Multiple Entries 
               # Could used mean CPUE if same TH and WEIGHT_lbs are different and below max limit
               #...but for simplicity, would delete all multiple entries under a same DATE and GRID for a LICENCE
               #Sum if

#QAQC Actions: #########
#1) Limit the analysis to Category A Partnership without Partnership (so assume TH limit = TLimit)


dTH <- okCPUE %>% group_by(DATE, year, LFA, Grid_Group, FV, LICENCE) %>%                   
     mutate    (     dailyTH  =  sum(TH),
                     dailyCatch = sum(WEIGHT_lbs),
                     n_TH = n_distinct(TH)) %>%
                    #dailyCPUE = dailyCatch/dailyTH) #this would give tripCPUE and can overlap multiple grids
     ungroup () %>% arrange (desc(n_TH))
                     

okTH <- dTH [!(  (dTH$LFA=="38"       &          dTH$dailyTH>=375)  | 
                 (dTH$LFA!="38"       &          dTH$dailyTH>300 )) ,] 


      

okTH <- okTH %>% ungroup() %>% arrange(desc(cpue_entry))  #if dplyr function "arrange" doesn't work, try "ungroup" the dat.frame


#2) Remove multiple log entry for a given DATA and GRID
okXentries<- okTH %>% distinct(LICENCE,DATE,GRID) %>% group_by(DATE, LFA, Grid_Group, LICENCE) %>%                   
                                                                 mutate    (     n_entries = n()) %>%     
                                                                                 ungroup () %>% arrange (desc(n_entries),DATE,LICENCE) # so 886 entries




     #ID LICENCE with recurrent multi-entries#######
     Whois<- okTH %>% group_by(LICENCE,DATE,GRID) %>% summarize(n=n(),
                                                                THvar=sd(TH),
                                                                sumTH=sum(dailyTH),
                                                                sumCatch=sum(dailyCatch),
                                                                dailyCPUE=sumCatch/sumTH,
                                                                avgCPUE=mean(cpue_entry),
                                                                CPUErange=round((dailyCPUE-avgCPUE)^2)) %>% filter(n>=2) %>% ungroup() %>%arrange(desc(CPUErange))
                                                                 
     RepeatOffender<- Whois %>% group_by (LICENCE) %>% summarise (No_times=n()) %>% arrange(desc(No_times))
     ##### 



#3) Check for excessive CPUE due to low TH log for a fishing trip in a given grid.

OutCPUE <- okXentries %>% filter(cpue_entry<50,dailyTH>=25 ) %>% arrange(DATE) %>% mutate(tripCPUE=dailyCatch/dailyTH)

OutCPUE <- OutCPUE %>% select(-(16:20))



#Objects cleaning ####

rm(CPUE,CPUEraw,Grid_Group)
rm(list = ls(pattern="sub"))
rm(list = ls(pattern="cCPUE"))

# Saving QAQCed data.frame ####
write.table(OutCPUE,file="CPUE_logdata")
CPUE <- read.table ("C:/Users/gaudettej/Documents/1-Lobster Folder/LFA36 - Fishing Season Adjustment/CPUE_logdata",header=TRUE)


#Data Exploration:#####################################################################

#I did not go through all filters assuming correct --AMCook Feb 2017

CPUE = read.table(file.path(fp,'CPUE_logdata'))



#Summarizing over FV, Time/Spatial Scales###############################################
# 

# Group CPUE by LICENCE

#Individual (LICENCE) fishing metric performance on a daily basis within Grid  ## Should be the same unless double entries of a same trip
SP <- subset(CPUE, month>=4 & month<8)

#Change plot order of facet grid:
SP$Grid_Group <-factor(SP$Grid_Group,levels = c("GrandManan_Coast","SWNB_Coast","NB_MidCoast","NB_UpperCoast", "NovaS_MidCoast","NovaS_UpperCoast", "LowBOF_Deep", "MidBOF_Deep", "UpBOF_Deep" ))



#Compilling data Opt.1 ################
di <- SP %>%  group_by(DATE, year, LFA, Grid_Group, GRID, LICENCE) %>%   #Daily Individual licence = di                
                    summarise (     diTH  =  sum(TH),
                                    n_TH = n_distinct(TH),  #Should be =1 only
                                    diCatch = sum(WEIGHT_lbs),
                                    CPUE = diCatch/diTH,
                                    time = mean(yday),
                                    #week = mean(week),
                                    month = mean(month))  

#Average CPUE per GRID per DAY across LICENCE
               dgrid <- di %>%   group_by(DATE, year, Grid_Group, GRID, month) %>%
                                        summarise (    gCPUE= mean(CPUE),
                                                       gTH= sum(diTH),
                                                       gCATCH = sum(diCatch),
                                                       cpuetest=round(gCATCH/gTH-gCPUE, digits=0) , #some glitches with value +/- 1 lbs_per_TH
                                                       n_LICENCE = n_distinct(LICENCE)) %>%
                                        mutate   (  week  = week(DATE)) %>% group_by (Grid_Group, GRID, week) %>%
                                        mutate   (wk_trips = sum(n_LICENCE)) %>%
                                        filter (wk_trips>=20) #Keeping only the catch records with sufficient fishing activity over a week period
               
               
               #Average CPUE per Grid_Group across Grids
               dgroup <- dgrid %>%   group_by(DATE, year, Grid_Group) %>%
                                        summarise (    gpCPUE= mean(gCPUE),
                                                       gpTH= sum(gTH),
                                                       gpCATCH = sum(gCATCH),
                                                       n_Grid = n_distinct(GRID),  # QAQC: many avgCPUE based on n=1 grid 
                                                       cpuecheck=round(((gpCATCH/gpTH-gpCPUE)/gpCPUE), digits=0)) %>%  
                                        mutate(  yday  = yday(DATE),
                                                 week  = week(DATE),
                                                 month = month(DATE)) 

               #Average CPUE in a given Grid_Group across Grid
               AvgCPUEyear <- dgroup %>% group_by(Grid_Group, yday) %>%
                                        summarize (    CPUE= mean(gpCPUE),
                                                       sdCPUE= sd(gpCPUE),
                                                       n=n_distinct(year),
                                                       time= mean(yday))
               
               
# Compelling data Opt.2 ################
# CPUE based per week instead of yday
# Since the option 1 has many single daily-GRID CPUE for the whole Grid_Group
               
wk_fisher2 <- SP %>%  group_by(year, LFA, Grid_Group, GRID, LICENCE, week) %>%                   
                    summarise (     wkTH  =  sum(TH),
                                    wkCatch = sum(WEIGHT_lbs),
                                    CPUE = mean(cpue_entry),
                                    sdCPUE = sd(cpue_entry),
                                    n_trips = n_distinct(cpue_entry),
                                    Date = mean(DATE))
                          

#Average CPUE per GRID per WEEK  across LICENCE


wk_grid2 <- wk_fisher2  %>%   group_by(year, Grid_Group, GRID, week) %>%
                                        summarise (    gCPUE  = mean(CPUE),
                                                       sdCPUE = sd(CPUE),
                                                       gTH= sum(wkTH),
                                                       gCATCH = sum(wkCatch),
                                                       n_Licences = n_distinct(LICENCE),
                                                       n_trips = sum(n_trips), 
                                                       Date = mean(Date)) 
               
               
# Average CPUE per Grid_Group across Grids
wk_group2 <- wk_grid2 %>%   group_by(year, week, Grid_Group) %>%
                                        summarise (    CPUE= mean(gCPUE),
                                                       sdCPUE= sd(gCPUE),
                                                       TH= sum(gTH),
                                                       CATCH = sum(gCATCH),
                                                       n_Grid = n_distinct(GRID),  # QAQC:  
                                                       n_trips = sum(n_trips), 
                                                       time = mean(week),
                                                       Date = mean(Date)) %>%
                                        filter (n_trips>5) #K 
                                        
           
               
#choose:
 di$week = week(di$DATE)
 di$yday = yday(di$DATE)


weeki <- di %>% group_by (year, LFA, Grid_Group, GRID, LICENCE, month, week) %>%
     summarise (     wkTH  =  sum(diTH),
                     wkCatch = sum(diCatch),
                     wkCPUE = wkCatch/wkTH,
                     n_wk_trips = n_distinct(yday),
                     time = mean(week))



wkgrid <- weeki %>%   group_by(year, LFA, Grid_Group, GRID, month, week) %>%
     summarize (    CPUE= mean(wkCPUE),
                    TH= mean(wkTH),
                    CATCH = mean(wkCatch)
                    )

wkgroup <- wkgrid %>%   group_by(year, LFA, Grid_Group, month, week) %>%
     summarize (    CPUE= mean(CPUE),
                    TH= sum(TH),
                    CATCH = sum(CATCH),
                    n_Grid = n_distinct(GRID))




# Plotting GGPLOT2 #####################
library(ggplot2)
library(plotly) # convert to interactive graph with function ggplotly

# Switch for choosing dataset
 plotdata <- wkgroup 
#plotdata <- di

p <- ggplot(plotdata, aes(time,CPUE, group=Grid_Group))  #+ geom_point(aes(color=Grid_Group, shape=Grid_Group, group=Grid_Group))) #color=Grid_Group, shape=Grid_Group, group=Grid_Group)

#Defining reference month line-dividers:
vline.data <- data.frame(z=c(60,91,121,152,182,213))

          # facet_wrap ######     
          p +  facet_wrap(na.omit(~Grid_Group)) + #to change order of the Grid_Group in facet, go to section: "Summarizing over FV, Time/Spatial Scales Summarizing" above  
               geom_point(data=plotdata, aes(y=CPUE)) +
               stat_smooth(method=gam,formula=y~s(x), se=TRUE,aes(color=Grid_Group,  fill=Grid_Group) ) +
               theme_bw() + labs(x = "Day of Year", y = "Daily Mean CPUE") +
               coord_cartesian(xlim=c(1,220),ylim = c(0, 5)) +
               #scale_x_continuous(breaks=c(30,60,90,120,150,180,210)) +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               theme(legend.position = "none") +
               geom_vline(aes(xintercept= z),data=vline.data, color="grey",linetype="dotted", size=0.01) +
               annotate("text", x= 17 , y=5, label = "Jan") +
               annotate("text", x= 48 , y=5, label = "Feb") +
               annotate("text", x= 76 , y=5, label = "Mars") + 
               annotate("text", x= 107, y=5, label = "Apr") + 
               annotate("text", x= 136, y=5, label = "May") + 
               annotate("text", x= 167, y=5, label = "Jun") + 
               annotate("text", x= 198, y=5, label = "Jul")
     
          # single plot #####
          p2 <- p + stat_smooth(method=gam,formula=y~s(x), se=TRUE,aes(color=Grid_Group,  fill=Grid_Group) ) +  #different "tensor type" for smooth terms: s, te, ti, t2 http://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html
               #geom_errorbar(data = AvgCPUEyear, aes(y=meanCPUE,ymin=meanCPUE-sdCPUE, ymax = meanCPUE + sdCPUE)) +
               geom_point(data=AvgCPUEyear, aes(y=meanCPUE,color=Grid_Group, alpha=0.1) ) +
               theme_bw() + labs(x = "Day of Year", y = "Daily Mean CPUE") +
               coord_cartesian(xlim=c(60,220),ylim = c(0, 5)) +
               scale_x_continuous(breaks=c(30,60,90,120,150,180,210)) +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               #theme(legend.position = "none") + #no legend
               geom_vline(aes(xintercept= z),data=vline.data, color="grey",linetype="dotted", size=0.01) +
               annotate("text", x= 17 , y=5, label = "Jan") +
               annotate("text", x= 48 , y=5, label = "Feb") +
               annotate("text", x= 76 , y=5, label = "Mars") + 
               annotate("text", x= 107, y=5, label = "Apr") + 
               annotate("text", x= 136, y=5, label = "May") + 
               annotate("text", x= 167, y=5, label = "Jun") + 
               annotate("text", x= 198, y=5, label = "Jul")

ggplotly()


#keep the only required object:
rm(list=setdiff(ls(), "di"))

CPUE = read.table(file.path(fp,'CPUE_logdata'))
SP <- subset(CPUE, month>=4 & month<8)
SP$Grid_Group <-factor(SP$Grid_Group,levels = c("GrandManan_Coast","SWNB_Coast","NB_MidCoast","NB_UpperCoast", "NovaS_MidCoast","NovaS_UpperCoast", "LowBOF_Deep", "MidBOF_Deep", "UpBOF_Deep" ))


SP <- SP[(SP$Grid_Group=="SWNB_Coast" | SP$Grid_Group=="NB_MidCoast" | SP$Grid_Group=="NB_UpperCoast"  ),] #Not sure this remove 
SP <- SP %>% filter (SP$Grid_Group=="SWNB_Coast" | SP$Grid_Group=="NB_MidCoast" | SP$Grid_Group=="NB_UpperCoast"  )


# Smooth Terms and GAM tensors ####################################  ##########################################
#different "tensor type" for smooth terms: s, te, ti, t2 
#http://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html

#GAM Models #######################################################
require(gamm4)
#GM1 <- gamm(formula = CPUE~ s(yday)+Grid_Group,random=list(year=~1),data=SP )
#GM2 <- gamm(formula = CPUE~ (Grid_Group)+year+s(yday),random=list(LICENCE=~1),data=SP )
GM3 <- gamm(formula = CPUE~ Grid_Group+s(yday),random=list(LICENCE=~1,year=~1),data=SP )
GM3 <- gamm(formula = ~ Grid_Group+s(yday),random=list(LICENCE=~1,year=~1),data=SP )
#~(year|FV_NAME) vessel catch rates vary from year to year, both random slope and intercept

GM3b <- gamm4(log(WEIGHT_lbs) ~ s(yday,by=Grid_Group),offset = log(SP$TH+1), random = ~(year|FV_NAME),data=SP,family=gaussian )


GM4 <- gamm(log(WEIGHT_lbs)  ~ s(yday, by= as.numeric(Grid_Group == "SWNB_Coast"),    bs = "cr") + #as.num converts to dummy var.
                                   s(yday, by= as.numeric(Grid_Group == "NB_MidCoast"),   bs = "cr") + #cubic reg. splines "cr" have shorter computing time than default thin spline smoother  
                                   s(yday, by= as.numeric(Grid_Group == "NB_UpperCoast"), bs = "cr"),  #see p. 434 from Mixed Effect Modelling in Ecology from Zuur
                          random=list(LICENCE=~1,year=~1),offset = log(SP$TH+1),data=SP ) #unable to allocate vector of size 2.3Gb with two random factors

GM4a <- gamm ( formula  =  CPUE  ~  s(time, by= as.numeric(Grid_Group == "GrandManan_Coast"),  bs = "cr") + 
                                    s(time, by= as.numeric(Grid_Group == "SWNB_Coast"),        bs = "cr") +  
                                    s(time, by= as.numeric(Grid_Group == "NB_MidCoast"),       bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "NovaS_MidCoast"),    bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "NB_UpperCoast"),     bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "NovaS_UpperCoast"),  bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "LowBOF_Deep"),       bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "MidBOF_Deep"),       bs = "cr") +
                                    s(time, by= as.numeric(Grid_Group == "UpBOF_Deep"),        bs = "cr"),  
                          random=list(Grid_Group=~1),data=wk_group2 ) #removed year as a random factor


#Figure from Zuur et al 2009 p. 434 #########################
source("C:/Highstat/Zuur et al 2009 - Mixed Effects Models in R/AllRCode/supportroutines4.R")

out8D<-mygamplot2(GM4a$gam) #just copy the name from the book coding of Zuur et al 2009

OUTTrend<-out8D[out8D[,5]<=10,]

library(lattice)
time2<-OUTTrend[,1]
fit2<-OUTTrend[,2]
ul2<-OUTTrend[,3]
ll2<-OUTTrend[,4]
id2<-OUTTrend[,5]
ID<-levels(SP$Grid_Group)
IDFull<-rep(ID,each=100)

xyplot(fit2~time2|IDFull,type="l",col=1,xlab="Time (years)",
       ylab="Trends",
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       panel = function(x, y,subscripts) {
            panel.grid(h=-1, v= 2)
            I<-order(x)
            llines(x[I], y[I],col=1)
            zup<-ul2[subscripts]
            zlow<-ll2[subscripts]
            llines(x[I], zup[I],col=1,lty=2)
            llines(x[I], zlow[I],col=1,lty=2)},
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")))




AIC(GM1$lme,GM2$lme,GM3$lme)
          #df      AIC
          #GM1$lme  6 84440.31
          #GM2$lme  7 81411.46
          #GM3$lme  7 73800.78

summary(GM3$gam)
anova(GM3$gam)
plot(GM3$gam)








