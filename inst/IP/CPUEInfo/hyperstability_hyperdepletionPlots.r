########
####example hyperstability plot
require(ggplot2)
x = seq(0,1,by=.01)
df = data.frame(x)
ggplot(df,aes(x))+
  stat_function(fun=function(x) x^.25)+
  stat_function(fun=function(x) x^.5)+
  stat_function(fun=function(x) x^1)+
  stat_function(fun=function(x) x^2)+
  stat_function(fun=function(x) x^4)+
  theme_test(base_size = 14) + theme(base_size=14, axis.text.x=element_blank(),axis.text.y=element_blank())+
  labs(x='Biomass',y='CPUE')+
  annotate("text",x=.25,y=.9,label='Hyperstability')+
  annotate("text",x=.8,y=.1,label='Hyperdepletion')+
  annotate("text",x=.13,y=.67,label=expression(paste(beta,"= 0.25",sep=" ")))+
  annotate("text",x=.23,y=.53,label=expression(paste(beta,"= 0.5",sep=" ")))+
  annotate("text",x=.33,y=.39,label=expression(paste(beta,"= 1",sep=" ")))+
  annotate("text",x=.43,y=.25,label=expression(paste(beta,"= 2",sep=" ")))+
  annotate("text",x=.53,y=.11,label=expression(paste(beta,"= 4",sep=" ")))

