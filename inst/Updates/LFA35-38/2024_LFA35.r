### LFA 35 figures
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(ggplot2)
require(ggtext)

p=list()
assessment.year = 2024 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

#### Do redos on landings and logs where necessary

#landings
a=lobster.db('seasonal.landings')
a$yr= as.numeric(substr(a$SYEAR,6,9))
aaa = a
#raw cpue
#lobster.db('logs.redo')    
        b = lobster.db('process.logs')
            b = subset(b,SYEAR %in% 2004:2024 & LFA =='35') ### CHECK LFA
            
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
      ef$Effort = ef$LFA35/(ef$CPUE) ###CHECK LFA

#commB
          require(bio.survey)
          require(bio.lobster)
          p=list()
          #p = bio.lobster::load.environment()
          p$libs = NULL
          p$yrs = 1970:2024
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
            b = subset(b,YR>1975 & YR<=2024)
            
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
            recxx = as.data.frame(do.call(cbind,rmed(recout$yr,recout$n.yst)))
            
  

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


# Catch and eff
aaa<-aaa%>% filter(!is.na(yr))
aap = aaa[nrow(aaa),]
aae = aaa[1:(nrow(aaa)-1),]

efp = ef[nrow(ef),]
ef = ef[-nrow(ef),]
ymax=5000
scaleright = max(ef$Effort)/ymax
g1 <- ggplot(data = subset(aae,yr>2004), aes(x = yr,y=LFA35)) + #CHECK LFA
  geom_bar(stat='identity',fill='black', width=0.75) +
   geom_point(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',shape=16,size=2.5)+
  geom_bar(data=aap,aes(x=yr,y=LFA35),stat='identity',fill='steelblue3', width=0.75) + #CHECK LFA
  geom_point(data=efp,aes(x=yr,y=Effort/scaleright),colour='steelblue3',shape=17,size=2.5)+
  geom_line(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',lwd=1,linetype='dashed')+
  scale_y_continuous(name='Catch (t)', sec.axis= sec_axis(~.*scaleright, name= "Effort ('000s Trap Hauls)", breaks = seq(0,2000,by=250)))+
  labs(x = "Year") +
theme_csas() 


### RV Survey recruit abundance
recout<-na.omit(recout)
ggplot(data = recout, aes(x = yr)) +
  geom_point(aes(y=n.yst),size=2) +
  geom_line(data=recxx,aes(x=yr,y=x),colour='red',lwd=1.25)+
  labs(x = "Year", y = "Recruit Abundance") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())


# cpue
ccp = cc[nrow(cc),]
ccf = cc[-nrow(cc),]
g1a <- ggplot(data = ccf, aes(x = yr,y = CPUE)) +
  geom_point(size=2)+
  geom_point(data=ccp,aes(x=yr,y=CPUE),colour='blue',shape=17,size=3)+
  geom_line(data=cp,aes(x=yr,y=x),colour='blue',lwd=1.25)+
  labs(x = "Year", y = "Raw CPUE") +
  theme_csas()


# relative F
g3a <- ggplot(data = df, aes(x = yr)) +
  geom_point(aes(y = rM)) +
  geom_line(data=pp,aes(x=yr,y = x),  colour = "red",lwd=1.25) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
  labs(x = "Year", y = "Relative Fishing mortality") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())


g4a <- ggplot(data = df, aes(x = yr)) +
  geom_point(aes(y = w.Yst)) +
  geom_line(data=xx,aes(x=yr,y = x),  colour = "red",lwd=1.25) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
  labs(x = "Year", y = "Commercial Biomass Index") +
  theme_csas() +
  theme(axis.title.y = ggtext::element_markdown())



###OUTSTANDING LOGS
percentReport=lobster.db('percent_reporting')


PR_2024 <- percentReport[grepl("^2024", percentReport$YEARMTH), ]
PR_2024<-PR_2024[order(PR_2024$YEARMTH), ]
PR_2024<-PR_2024[c("YEARMTH", "L35PERCENT")]

