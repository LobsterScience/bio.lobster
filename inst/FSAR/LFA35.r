### FSAR for LFA 35 figures
require(bio.lobster)
require(bio.utilities)
require(devtools)

#landings
a=lobster.db('seasonal.landings')
a$yr= as.numeric(substr(a$SYEAR,6,9))
aaa = a
#raw cpue
            b = lobster.db('process.logs')
            b = subset(a,SYEAR %in% 2004:2023 & LFA =='35') 
            
            aa = split(b,f=list(b$LFA,b$SYEAR))
            cpue.lst<-list()
            
            for(i in 1:length(aa)){
              tmp<-aa[[i]]
              tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
              names(tmp)<-c('time','catch','effort')
              tmp$date<-as.Date(tmp$time)
              first.day<-min(tmp$date)
              tmp$time<-julian(tmp$date,origin=first.day-1)
              tmp$time = ceiling(tmp$time/7) #convert to week of season
              g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
              g$lfa=unique(aa[[i]]$LFA)
              g$yr = unique(aa[[i]]$SYEAR)
              g = t(g)[,1]
              cpue.lst[[i]] <- g
            }
            
            cc =as.data.frame(do.call(rbind,cpue.lst))
            cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
            cc$yr = as.numeric(cc$yr)
            cp = as.data.frame(do.call(cbind,rmed(cc$yr,cc$CPUE)))
#effort
      ef = merge(cc,aaa)
      ef$Effort = ef$LFA35/(ef$CPUE)
#standardized cpue
      lobster.db('logs.redo')
      lobster.db('process.logs.redo')
      lobster.db('temperature.data.redo')
      TempModelData(redo=T)
      TempModelling = TempModel(areas='lfa', annual.by.area=F, redo.data=F)
      saveRDS(TempModelling,file=file.path(project.datadirectory('bio.lobster'),'tempmodelling.rds'))
      
      # step through this function CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
      mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)
      
      CPUE.data<- cpue.data
      t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))
      
          CPUE.data$fYEAR=as.factor(CPUE.data$SYEAR)
         CPUEModelResults = CPUEmodel(mf1,CPUE.data,t=t,d=1)
          outs =CPUEModelResults$pData
          oo = as.data.frame(do.call(cbind,rmed(outs$YEAR,outs$mu)))
      
#commB
          require(bio.survey)
          require(bio.lobster)
          
          p = bio.lobster::load.environment()
          p$libs = NULL
          p$yrs = 1970:2022
          p1 = p
          
          
            p$series =c('summer')# p$series =c('georges');p$series =c('fall')
            p$years.to.estimate = p$yrs
            p$length.based = T
            p$by.sex = T
            p$size.class = c(83,300)
            p$sex = c(1,2)
            p$bootstrapped.ci=T
            p$strata.files.return=F
            p$vessel.correction.fixed=1.2
            p$strat = NULL
            p$clusters = c( rep( "localhost", 7) )
            p$strata.efficiencies = F
            p = make.list(list(yrs=p$years.to.estimate),Y=p)
            p$define.by.polygons = T
            p$lobster.subunits=F
            p$area = 'LFA35-38'
            p$reweight.strata = T #this subsets 
            aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
            
            p$by.sex = p$length.based = F
            
            aoutf= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
            aoutf = subset(aoutf,yr<1999)
            aout = subset(aout,yr>1998)
            df = as.data.frame(rbind(aoutf,aout))
            df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
            df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.746
            df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.746
            df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.746
            
            xx = as.data.frame(do.call(cbind,rmed(df$yr,df$w.Yst)))
  #relf
            a = lobster.db('annual.landings')
            b = lobster.db('seasonal.landings')
            b$YR = substr(b$SYEAR,6,9)
            a = subset(a,YR<1976)
            b = subset(b,YR>1975 & YR<=2022)
            
            a$L3538 = rowSums(a[,12:14])
            b$L3538 = rowSums(b[,4:6])
            c358 = as.data.frame(rbind(a[,c('YR','L3538')],b[,c('YR','L3538')]))
            names(c358)[1] = 'yr'
            df  =merge(df,c358)
            df$rL = df$L3538/(df$w.ci.Yst.l+df$L3538)
            df$rU =df$L3538/ (df$w.ci.Yst.u+df$L3538)
            df$rM = df$L3538/(df$w.Yst+df$L3538)
            pp = as.data.frame(do.call(cbind,rmed(df$yr,df$rM)))


#recN
            p$length.based = T
            p$by.sex = T
            p$size.class = c(70,82)
            p$sex = c(1,2)
            p$bootstrapped.ci=T
            p$strata.files.return=F
            p$vessel.correction.fixed=1.2
            p$strat = NULL
            p$clusters = c( rep( "localhost", 7) )
            p$strata.efficiencies = F
            p = make.list(list(yrs=p$years.to.estimate),Y=p)
            p$define.by.polygons = T
            p$lobster.subunits=F
            p$area = 'LFA35-38'
            p$reweight.strata = T #this subsets 
            recout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
            recxx = as.data.frame(do.call(cbind,rmed(recout$yr,recout$n.Yst)))
            

#scallop survey reworked in 2023
  lobster.db('scallop.redo')
  sc = scallopSurveyIndex(redo=T,size_range = c(70,82), lfa=35)[[1]]
  
#plotting as per csasdown 4 panel plot
#add in the theme_csas
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


#format from FSAR branch of CSASdown
# Catch and eff

aap = aaa[nrow(aaa),]
aaa = aaa[1:(nrow(aaa)-1),]

efp = ef[nrow(ef),]
ef = ef[-nrow(ef),]
ymax=5000
scaleright = max(ef$Effort)/ymax
g1 <- ggplot(data = aaa, aes(x = yr,y=LFA35)) +
  geom_bar(stat='identity',fill='black') +
  geom_bar(data=aap,aes(x=yr,y=LFA35),stat='identity',fill='gray66') +
  geom_point(data=efp,aes(x=yr,y=Effort/scaleright),colour='grey66',shape=17,size=3)+
  geom_line(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',lwd=2)+
    scale_y_continuous(name='Catch (t)', sec.axis= sec_axis(~.*scaleright, name= 'Effort',breaks = seq(0,2000,by=250)))+
  labs(x = "Year") +
theme_csas()

# standarzized cpue
g2 <- ggplot(data = outs, aes(x = YEAR)) +
  geom_point(aes(y = mu),size=2) +
  geom_line(data=oo,aes(x=yr,y=x),colour='grey45',lwd=1.25)+
  labs(x = "Year", y = "Standardized CPUE") +
  geom_hline(yintercept=mean(outs$mu[7:14]) * .4,colour='grey',lwd=1.25,linetype='dashed')+
  geom_hline(yintercept=mean(outs$mu[7:14]) * .2,colour='grey',lwd=1.25,linetype='dotted')+
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())

g3 <- ggplot(data = outs, aes(x = YEAR)) +
  geom_point(aes(y = mu),size=0,colour='white') +
  labs(x = "Year", y = "Fishing Mortality") +
  theme_csas() +
  theme(axis.text.y = element_blank(),axis.title.y = ggtext::element_markdown())

#g3 = ggplot() +                      # Draw ggplot2 plot with text only
#  annotate("text",
#           x = 1,
#           y = 1,
##           size = 4,
#           label = "") + 
#  theme_void()

g4 <- ggplot(data = recout, aes(x = yr)) +
  geom_point(aes(y = n.Yst),size=2) +
  geom_line(data=recxx,aes(x=yr,y=x),colour='grey45',lwd=1.25)+
  labs(x = "Year", y = "Recruit Abundance") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())
cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO", align = "hv")

#second plot
# cpue
g1a <- ggplot(data = cc, aes(x = yr,y = CPUE)) +
  geom_point(size=2)+
  geom_line(data=cp,aes(x=yr,y=x),colour='gray45',lwd=1.25)+
  labs(x = "Year", y = "Raw CPUE") +
  theme_csas()

#
g2a <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2)+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='gray45',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance") +
  theme_csas()

# relative F
g3a <- ggplot(data = df, aes(x = yr)) +
  geom_point(aes(y = rM)) +
  geom_line(data=pp,aes(x=yr,y = x),  colour = "grey45",lwd=1.25) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
  labs(x = "Year", y = "Relative Fishing mortality") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())


g4a <- ggplot(data = df, aes(x = yr)) +
  geom_point(aes(y = w.Yst)) +
  geom_line(data=xx,aes(x=yr,y = x),  colour = "grey45",lwd=1.25) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
  labs(x = "Year", y = "Commercial Biomass Index") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())

cowplot::plot_grid(g1a, g2a, g3a, g4a, ncol = 2, labels = "AUTO", align = "hv")

