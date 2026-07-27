require(tidyr)
require(dplyr)
require(bio.lobster)
require(devtools)
require(ggplot2)
library(viridis)
la()
a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1975 & YR<2026)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]
gg = ggplot(sa,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab('Landings (kt)')


a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1975& SYEAR<2026)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
names(sa1)[1] = "YR"

o = bind_rows(sa,sa1)
 o1 = subset(o,LFA %ni% 'LFA38B')

mo1 = aggregate(Landings~LFA,data=subset(o1,YR %in% 1975:2025),FUN=function(x) c(median(x),quantile(x,c(0.25,0.75))))
o1$Lkt = o1$Landings/1000

o1 <- o1 %>%
  group_by(LFA) %>%
  mutate(is_max = Lkt == max(Lkt,na.rm=T))
ggplot(subset(o1),aes(x=YR,y=Lkt,fill=is_max))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' ,nrow=2)+xlab('Fishing Year')+ylab('Landings (kt)')+
  scale_x_continuous(breaks=round(seq(1975,2025,length=4)))+theme_test()+ theme(legend.position = 'none')+
scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "grey10")) 
#    geom_hline(data=mo1,aes(yintercept=Landings[,1]/1000),linetype='solid',colour='red')
#  geom_hline(data=mo1,aes(yintercept=Landings[,2]/1000),linetype='dashed',colour='red')+
#  geom_hline(data=mo1,aes(yintercept=Landings[,3]/1000),linetype='dashed',colour='red')


  




ggplot(o1, aes(fill=LFA, x=YR, y=Landings)) + 
  geom_bar(position="stack", stat="identity",colour='black')+theme_test()+xlab('Fishing Year')+ylab('Landings (t)')+
 scale_fill_viridis(discrete=T) + theme_test(base_size = 15)



ggplot(subset(o1, LFA %in% c('LFA34','LFA35','LFA36','LFA38')), aes(fill=LFA, x=YR, y=Landings)) + 
  geom_bar(position="stack", stat="identity",colour='black')+theme_test()+xlab('Fishing Year')+ylab('Landings (t)')+
  scale_fill_viridis(discrete=T) + theme_test(base_size = 13)+
  theme(
    plot.background = element_rect(
      fill = "pink",
      colour = NA
    )
  ,
panel.background = element_rect(
  fill = "pink",
  colour = NA
),
legend.background = element_rect(
  fill = "pink",
  colour = NA
)
)

o2 = subset(o1,LFA %in% c('LFA33','LFA32','LFA31B','LFA31A','LFA30','LFA29','LFA27'))
o2$LFA. <-factor(o2$LFA,levels=(c('LFA27','LFA29','LFA30','LFA31A','LFA31B','LFA32','LFA33')))
ggplot(subset(o2,), aes(fill=LFA., x=YR, y=Landings)) + 
  geom_bar(position=position_stack(reverse = T), stat="identity",colour='black')+theme_test()+xlab('Fishing Year')+ylab('Landings (t)')+
  scale_fill_viridis(discrete=T,direction=-1) + theme_test(base_size = 13)+ theme(
    plot.background = element_rect(
      fill = "#F0FFF0",
      colour = NA
    )
    ,
    panel.background = element_rect(
      fill = "#F0FFF0",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "#F0FFF0",
      colour = NA
    )
    
  )





l38 = aggregate(Landings~YR,data=subset(o,LFA %in% c('LFA38','LFA38B')),FUN=sum)


ggplot(l38,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity',width=1)

library(dplyr)
library(tidyr)

slope_df <- o1 %>%
  filter(YR >= 2000 & YR <= 2010) %>%
  group_by(LFA) %>%
  arrange(YR) %>%
  summarise(
    {
      max_slope <- -Inf
      x_start <- y_start <- x_end <- y_end <- NA
      for (i in 1:(n() - 5)) {
        y1 <- YR[i]
        y2 <- YR[i + 5]
        l1 <- Landings[i]
        l2 <- Landings[i + 5]
        slope <- (l2 - l1) / ( y1)
        if (slope > max_slope) {
          max_slope <- slope
          x_start <- y1
          y_start <- l1/1000
          x_end <- y2
          y_end <- l2/1000
        }
      }
      tibble(x_start, y_start, x_end, y_end)
    },
    .groups = "drop"
  )

slope_df <- slope_df %>%
  mutate(
    slope_label = paste0( round((y_end - y_start) / ( y_start), 2)*100, "%"),
    label_x = 1988,
    label_y = (y_end) *.8
  )

ggplot(subset(o1,LFA %ni% c('LFA27','LFA29', 'LFA28','LFA30','LFA31A','LFA31B','LFA32')),aes(x=YR,y=Lkt,fill=is_max))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' )+xlab('Fishing Year')+ylab('Landings (kt)')+
  scale_x_continuous(breaks=round(seq(1975,2024,length=4)))+theme_test()+ theme(legend.position = 'none')+
  scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "grey10")) +
  geom_segment(
    data = subset(slope_df,LFA %ni% c('LFA27','LFA28','LFA29', 'LFA30','LFA31A','LFA31B','LFA32')),
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    color = "red", size = 1.2,
    inherit.aes = FALSE
  ) +
  
  geom_text(
    data = subset(slope_df,LFA %ni% c('LFA27','LFA28','LFA29', 'LFA30','LFA31A','LFA31B','LFA32')),
    aes(x = label_x, y = label_y, label = slope_label),
    color = "red", fontface = "bold",
    inherit.aes = FALSE
  ) 

#prop landings
library(dplyr)

o1_prop <- o1 %>%
  group_by(YR) %>%
  mutate(prop_landings = Landings / sum(Landings, na.rm = TRUE)) %>%
  ungroup()

o134 = subset(o1_prop,LFA=='LFA35')
sf = max(o134$Landings, na.rm=T) / max(o134$prop_landings, na.rm=T)

ggplot(o134, aes(x = YR)) +
  geom_bar(aes(y = Landings), color = "blue", stat='identity') +
  geom_line(aes(y = prop_landings * sf), color = "red", size = 1) +
  scale_y_continuous(
    name = "Landings",
    sec.axis = sec_axis(~ . / sf, name = "Proportion of Maritimes Region Landings")
  ) +
  labs(x='Fishing Season')+
  theme_minimal(base_size = 14)+
      theme(
      axis.title.y.left  = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "red")
    )
  


###VALUE

d = lobster.db('process_slips')
b = lobster.db('inflation')
i = which(b$year==2001)
b$nInf = b$amount[1:nrow(b)]/b$amount[i]

bw = merge(d,b,by.x='SYEAR',by.y='year')
bw$infPr = bw$PRICE/bw$nInf #adjusted to 2001
bw$Inf_Val = bw$infPr * bw$WT_LBS
bw$T = bw$WT_LBS/2204.62
bwa = aggregate(cbind(value,Inf_Val,T)~LFA+SYEAR,data=bw,FUN=function(x) c(mean(x),quantile(x,probs=c(0.25,0.75))))
ok = aggregate(cbind(value,Inf_Val,T)~LFA+SYEAR,data=bw,FUN=sum)

#ok = aggregate(value~SYEAR+LFA,data=b,FUN=sum)
ok$value = ok$value/1000000
ok$Ld = ok$Inf_Val/1000000
ok <- ok %>%
  group_by(LFA) %>%
  mutate(is_max = Ld == max(Ld,na.rm=T))

ggplot(subset(ok,SYEAR>1997 & SYEAR<2026 & LFA %in% 34),aes(x=SYEAR,y=Ld))+geom_bar(stat='identity',width=1)+xlab('Fishing Year')+ylab('Landed Value (millions), Inflation Adj')+
  scale_x_continuous(breaks=round(seq(1998,2025,length=4)))+theme_test()+ theme(legend.position = 'none')+
scale_y_continuous(breaks=round(seq(100,330,length=4)))+theme_test()+ theme(legend.position = 'none')+
  coord_cartesian(ylim=c(100,330))


ggplot(subset(ok,SYEAR>1997 & SYEAR<2026 & LFA %in% 34),aes(x=SYEAR,y=value))+geom_bar(stat='identity',width=1)+xlab('Fishing Year')+ylab('Landed Value (millions)')+
  scale_x_continuous(breaks=round(seq(1998,2025,length=4)))+theme_test()+ theme(legend.position = 'none')+
  scale_y_continuous(breaks=round(seq(100,500,length=4)))+theme_test()+ theme(legend.position = 'none')+
  coord_cartesian(ylim=c(100,500))


v = lobster.db('seasonal.landings')
v$SYEAR = as.numeric(substr(v$SYEAR,6,9))

ok1 = subset(ok,LFA==34)
scale_factor <- max(ok1$T, na.rm = TRUE) / max(ok1$value, na.rm = TRUE)

g1 = ggplot(ok1, aes(x = SYEAR)) +
  geom_col(aes(y = T), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = value * scale_factor), color = "red", linewidth = 1) +
  geom_line(aes(y = Ld * scale_factor), color = "purple", linewidth = 1) +
  scale_y_continuous(
    name = "Landings (t)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Dollar Value (millions)")
  ) +
  theme_minimal()+
  labs(x='Fishing Season')

g2 = ggLobsterMap('SWN',addLFALabels = T,colourLFA = '34')

layout=rbind(c(1,1,1),c(1,1,2))

gridExtra::grid.arrange(g1,g2,layout_matrix=
                          (layout))
#nlics
g = lobster.db('slips')
i = which(g$LICENCE_SUBTYPE=='PARTNERSHIP A')
g$LICENCE_SUBTYPE[i] = 'STACKED'
g$YR = lubridate::year(g$DATE_LANDED)
g$MN = lubridate::month(g$DATE_LANDED)
g$SYEAR = g$YR
g$SYEAR = ifelse(g$LFA %in% c(33,34,36,38 ) & g$MN %in% c(11,12),g$SYEAR+1,g$SYEAR)
g$SYEAR = ifelse(g$LFA %in% c(35 ) & g$MN %in% c(10,11,12),g$SYEAR+1,g$SYEAR)
g$N = 1
ga = aggregate(N~LFA+SYEAR+LICENCE_TYPE+LICENCE_SUBTYPE+LICENCE_ID,data=g,FUN=sum)

gap = aggregate(N~LFA+SYEAR+LICENCE_ID,data=ga,FUN=length)
gg = subset(gap,N>1)

drop_keys <- with(gg, paste(LFA, SYEAR, LICENCE_ID))

ga_final <- ga[!(
  ga$LICENCE_SUBTYPE == "STACKED" &
    paste(ga$LFA, ga$SYEAR, ga$LICENCE_ID) %in% drop_keys
), ]



require(tidyr)

gw = subset(ga_final,LFA %ni% 'LOBSTER - GREY ZONE' & LICENCE_SUBTYPE %in% c('PARTNERSHIP A','CATEGORY B','CATEGORY A','STACKED')) %>%
  pivot_wider(
    id_cols = c(LFA, SYEAR),
    names_from = LICENCE_SUBTYPE,
    values_from = N,
    values_fill = 0,
    values_fn = length
  )
names(gw)[2] = 'SYEAR'
gw$STACKED = gw$STACKED*2
gw$Tot_catA = apply(gw[,c('CATEGORY A','STACKED')],1,sum)

gs = subset(gw,LFA==34)
ggplot(subset(gs,SYEAR>2001 & SYEAR<2026),aes(x=SYEAR))+geom_line(aes(y=Tot_catA),color='red')+
geom_line(aes(y=STACKED),color='purple')+
  theme_minimal()+
  labs(x='Fishing Season',y='N Licences')


#scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "red")) 




b = subset(b,LFA==36)
b$mn = lubridate::month(b$Date)
