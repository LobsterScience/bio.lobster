#lfa 33
x = read.csv('C:/Users/cooka/Downloads/yr.cpue.fsfs.csv')
x1 = ASMFC_triggers(x$cpue,x$yr)
x2 = ASMFC_triggers(x$fsrs_rec,x$yr)
x3 = ASMFC_triggers(x$fsrs_leg,x$yr)

v = data.frame(yrs=c(x1$yrs,x2$yrs,x3$yrs),ind =c(x1$index,x2$index,x3$index),index_name=c(rep('cpue',23),rep('fsrs_rec',23),rep('fsrs_legal',23)))

vv = aggregate(ind~yrs,data=v,FUN=mean)
x4 = ASMFC_triggers(vv$ind,vv$yrs)
x4 = data.frame(yrs=x4$yrs,ind=x4$index,index_name=rep('Combined',21))
v = rbind(v,x4)
names(v)[1:2] = c('Years','Scaled_Index')

ggplot(v,aes(x=Years,y=Scaled_Index))+geom_point()+geom_line()+facet_wrap(~index_name)+geom_hline(yintercept = 0.65,color='red',linetype='dashed')


### lfa 34

fd = file.path(project.figuredirectory('bio.lobster'),'LFA34Update')

x1 = read.csv(file.path(fd,'ILTSCommB.csv'))
x2 = read.csv(file.path(fd,'LFA34-DFOCommercialB.csv'))
x3 = read.csv(file.path(fd,'LFA34-NEFSCFallCommercialB.csv'))
x4 = read.csv(file.path(fd,'LFA34-NEFSCSpringCommercialB.csv'))
x5 = lobster.db('process.logs')
x5=subset(x5,LFA==34 & SYEAR<2024)
x5 = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=x5,FUN=sum)
x5$CPUE = x5$WEIGHT_KG/x5$NUM_OF_TRAPS

x1 = subset(x1,Year>2000)
x2 = subset(x2,yr>2000)
x3 = subset(x3,yr>2000)
x4 = subset(x4,yr>2000)
x5 = subset(x5,SYEAR>2000)

x1=ASMFC_triggers(x1$B,x1$Year)
x2=ASMFC_triggers(x2$w.Yst,x2$yr)
#x3=ASMFC_triggers(x3$w.Yst,x3$yr)
#x4=ASMFC_triggers(x4$w.Yst,x4$yr)
x5=ASMFC_triggers(x5$CPUE,x5$SYEAR)

v = data.frame(yrs=c(x1$yrs,x2$yrs,x5$yrs),ind =c(x1$index,x2$index,x5$index),index_name=c(rep('ITLS',length(x1$index)),rep('DFO_RV',length(x2$index)),rep('CPUE',length(x5$index))))
vv = aggregate(ind~yrs,data=v,FUN=mean)
x4 = ASMFC_triggers(vv$ind,vv$yrs)
x4 = data.frame(yrs=x4$yrs,ind=x4$index,index_name=rep('Combined',19))
v = rbind(v,x4)
names(v)[1:2] = c('Years','Scaled_Index')

ggplot(v,aes(x=Years,y=Scaled_Index))+geom_point()+geom_line()+facet_wrap(~index_name)+geom_hline(yintercept = 0.65,color='red',linetype='dashed')+theme_test()
