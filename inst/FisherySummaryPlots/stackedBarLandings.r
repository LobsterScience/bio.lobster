require(tidyr)
require(dplyr)
require(bio.lobster)
require(devtools)
require(ggplot2)
library(viridis)
la()
a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1975 & YR<2025)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]
gg = ggplot(sa,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab('Landings (kt)')


a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1975& SYEAR<2025)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
names(sa1)[1] = "YR"

o = bind_rows(sa,sa1)
 o1 = subset(o,LFA %ni% 'LFA38B')

mo1 = aggregate(Landings~LFA,data=subset(o1,YR %in% 1975:2023),FUN=function(x) c(median(x),quantile(x,c(0.25,0.75))))
o1$Lkt = o1$Landings/1000

o1 <- o1 %>%
  group_by(LFA) %>%
  mutate(is_max = Lkt == max(Lkt,na.rm=T))
ggplot(o1,aes(x=YR,y=Lkt,fill=is_max))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' )+xlab('Fishing Year')+ylab('Landings (kt)')+
  scale_x_continuous(breaks=round(seq(1975,2024,length=4)))+theme_test()+ theme(legend.position = 'none')+
scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "grey10")) +
    geom_hline(data=mo1,aes(yintercept=Landings[,1]/1000),linetype='solid',colour='red')+
  geom_hline(data=mo1,aes(yintercept=Landings[,2]/1000),linetype='dashed',colour='red')+
  geom_hline(data=mo1,aes(yintercept=Landings[,3]/1000),linetype='dashed',colour='red')


  




ggplot(o1, aes(fill=LFA, x=YR, y=Landings)) + 
  geom_bar(position="stack", stat="identity",colour='black')+theme_test()+xlab('Fishing Year')+ylab('Landings (t)')+
 scale_fill_viridis(discrete=T) + theme_test(base_size = 15)


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




###VALUE

b = lobster.db('process_slips')

ok = aggregate(value~SYEAR+LFA,data=b,FUN=sum)
ok$Ld = ok$value/1000000

ok <- ok %>%
  group_by(LFA) %>%
  mutate(is_max = Ld == max(Ld,na.rm=T))
ggplot(subset(ok,SYEAR<2025 & LFA %ni% 41),aes(x=SYEAR,y=Ld,fill=is_max))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' )+xlab('Fishing Year')+ylab('Landed Value (millions)')+
  scale_x_continuous(breaks=round(seq(1975,2024,length=4)))+theme_test()+ theme(legend.position = 'none')+
  scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "red")) 




b = subset(b,LFA==36)
b$mn = lubridate::month(b$Date)
