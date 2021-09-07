tab= aggregate(NUM_OF_TRAPS~SYEAR+LFA,data=subset(logs,SYEAR>2018),FUN=sum)
tab2= aggregate(WEIGHT_KG~SYEAR+LFA,data=subset(logs,SYEAR>2018),FUN=sum)
tab3=cbind(tab, tab2)
tab3$CPUE_KG=round(tab3$WEIGHT_KG/tab3$NUM_OF_TRAPS, 2)
tab4=tab3[c("SYEAR","LFA", "CPUE_KG")]