a = lobster.db('process.logs.unfiltered')
a = subset(a, SYEAR <2025 & SYEAR>2005)


x1 = aggregate(WEIGHT_KG~SYEAR+LICENCE_ID+LFA,data=subset(a,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=sum)
g = lobster.db('slips')
g$YR = lubridate::year(g$DATE_LANDED)
g$MN = lubridate::month(g$DATE_LANDED)
g$SYEAR = g$YR
g$SYEAR = ifelse(g$LFA %in% c(33,34,36,38 ) & g$MN %in% c(11,12),g$SYEAR+1,g$SYEAR)
g$SYEAR = ifelse(g$LFA %in% c(35 ) & g$MN %in% c(10,11,12),g$SYEAR+1,g$SYEAR)

sx = aggregate(cbind(SLIP_WEIGHT_LBS)~SYEAR+LICENCE_ID+LICENCE_SUBTYPE+LICENCE_TYPE+LFA,data=g,FUN=sum)

xx = merge(x1,sx)
ggplot(xx,aes(x=WEIGHT_KG,y=SLIP_WEIGHT_LBS))+geom_point()+geom_smooth(method='lm')+facet_wrap(~LFA,scales = 'free')


ggplot(aes(x=WEIGHT_KG,y=SLIP_WEIGHT_LBS * 0.45359237))+geom_point()+geom_smooth(method='lm')+facet_wrap(~SYEAR,scales = 'free')
ggplot(subset(xx,LFA==34), aes(WEIGHT_KG, SLIP_WEIGHT_LBS * 0.45359237)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ SYEAR, ncol = 4, scales = "free") +
  labs(
    x = "Log Weight (kg)",
    y = "Slip weight (kg)"
  ) +geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8)+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 9),
    panel.spacing = unit(1, "lines")
  )

xx$pdiff = (xx$WEIGHT_KG-(xx$SLIP_WEIGHT_LBS*0.45359237)) / xx$WEIGHT_KG *100

ggplot(xx,aes(x=as.factor(SYEAR),y=pdiff))+geom_boxplot(outlier.shape = NA)+facet_wrap(~LFA)+
  geom_hline(yintercept = 0,colour='red', linewidth=0.5)+
  geom_hline(yintercept = 0,colour='red', linewidth=0.5)+
coord_cartesian(ylim = c(-30,30))+labs(y='Logs-Slips / Logs')
