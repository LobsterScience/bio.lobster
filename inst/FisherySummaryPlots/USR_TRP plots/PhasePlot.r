library(ggplot2)

LRP <- 11
USR <- 40
TRP <- 58
Frr <- 0.55

ggplot() +
  annotate("rect", xmin = 0, xmax = LRP,
           ymin = 0, ymax = 1.05,
           fill = "#DD9271") +
  annotate("rect", xmin = LRP, xmax = USR,
           ymin = 0, ymax = 1.05,
           fill = "#DDD27F") +
  annotate("rect", xmin = USR, xmax = 100,
           ymin = 0, ymax = 1.05,
           fill = "#66EE00") +
  
  geom_vline(xintercept = c(LRP, USR,TRP), linetype = "dotted",linewidth=1.1) +
  
  geom_segment(aes(x = LRP, y = 0,
                 xend = USR, yend = Frr),
              linewidth =1.1) +
  
  geom_segment(aes(x = USR, y = Frr,
                   xend = 100, yend = Frr),
               linewidth = 0.8) +
  
  annotate("text", x = LRP - 3.5, y = 0.63,
           label = "LRP", angle = 90) +
  annotate("text", x = USR - 3.5, y = 0.63,
           label = "USR", angle = 90) +
  annotate("text", x = 75, y = Frr + 0.04,
           label = "RR") +
  annotate("text", x = TRP - 3.5, y = 0.63,
           label = "TRP", angle = 90) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.05)) +
  labs(x = "Biomass", y = "Fishing Mortality") +
  theme_classic(base_size=14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
          )

 ggplot() +
  annotate("rect", xmin = 0, xmax = LRP,
           ymin = 0, ymax = 1.05,
           fill = "#DD9271") +
  annotate("rect", xmin = LRP, xmax = 114,
           ymin = 0, ymax = 1.05,
           fill = "white") +
  geom_vline(xintercept = c(LRP), linetype = "dotted") +
  annotate("text", x = LRP - 3.5, y = 0.13,
           label = "LRP", angle = 90)  +
  coord_cartesian(xlim = c(0, 114), ylim = c(0, Frr+0.05)) +
  labs(x = "Biomass", y = "Fishing Mortality") +
  theme_classic(base_size=14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
 
 
 LRP=11

 #uncertainty in USR
  set.seed(123)
 usr_lines <- data.frame(
   x = rnorm(1000, mean = 45, sd = 5)
 )
 usr_lines <- subset(usr_lines, x >= 25 & x <= 58)
 ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotation_raster(
     raster = as.raster(grad),
     xmin = LRP, xmax = 114,
     ymin = 0, ymax = 1.05
   )+
   geom_vline(xintercept = c(LRP), linetype = "dotted") +
   
   geom_vline(
     data = usr_lines,
     aes(xintercept = x),
     colour = "black",
     alpha = 0.03,
     linewidth = 0.8
   )+ annotate("text", x = LRP - 3.5, y = 0.13,
            label = "LRP", angle = 90)  +
   annotate("text", x = USR - 3.5, y = 0.13,
            label = "USR", angle = 90)  +
   
   coord_cartesian(xlim = c(0, 114), ylim = c(0, Frr+0.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )
 
 
 #USR and Arrows
 USR=40
 ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotation_raster(
     raster = as.raster(grad),
     xmin = LRP, xmax = 114,
     ymin = 0, ymax = 1.05
   )+
   geom_vline(xintercept = c(LRP,USR), linetype = "dotted",linewidth=1.2) +
    annotate("text", x = LRP - 3.5, y = 0.13,
               label = "LRP", angle = 90)  +
   annotate("text", x = USR - 3.5, y = 0.13,
            label = "USR", angle = 90)  +
   # arrow pointing left from USR
   geom_segment(
     aes(x = USR, xend = 58,
         y = 0.25, yend = 0.25),
     arrow = arrow(length = unit(0.2, "cm"))
   ) +
   
   # arrow pointing right from USR
   geom_segment(
     aes(x = USR, xend = 28,
         y = 0.25, yend = 0.25),
     arrow = arrow(length = unit(0.2, "cm"))
   )+
   
   coord_cartesian(xlim = c(0, 114), ylim = c(0, Frr+0.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )
 
 
 
 
 
 
 ######### 
 #different USRs and different RR change
 p40 = ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotate("rect", xmin = LRP, xmax = 40,
            ymin = 0, ymax = 1.05,
            fill = "#DDD27F") +
   annotate("rect", xmin = 40, xmax = 100,
            ymin = 0, ymax = 1.05,
            fill = "#66EE00") +
   
   geom_vline(xintercept = c(LRP, 40), linetype = "dotted",linewidth=1.1) +
   
   geom_segment(aes(x = LRP, y = 0,
                    xend = 40, yend = Frr),
                linewidth =1.1) +
   
   geom_segment(aes(x = 40, y = Frr,
                    xend = 100, yend = Frr),
                linewidth = 0.8) +
   
   annotate("text", x = LRP - 3.5, y = 0.63,
            label = "LRP", angle = 90) +
   annotate("text", x = 40 - 3.5, y = 0.63,
            label = "USR", angle = 90) +
   annotate("text", x = 75, y = Frr + 0.04,
            label = "RR") +
   #annotate("text", x = TRP - 3.5, y = 0.63,
   #         label = "TRP", angle = 90) +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )
 
 
 p25 = ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotate("rect", xmin = LRP, xmax = 25,
            ymin = 0, ymax = 1.05,
            fill = "#DDD27F") +
   annotate("rect", xmin = 25, xmax = 100,
            ymin = 0, ymax = 1.05,
            fill = "#66EE00") +
   
   geom_vline(xintercept = c(LRP, 25), linetype = "dotted",linewidth=1.1) +
   
   geom_segment(aes(x = LRP, y = 0,
                    xend = 25, yend = Frr),
                linewidth =1.1) +
   
   geom_segment(aes(x = 25, y = Frr,
                    xend = 100, yend = Frr),
                linewidth = 0.8) +
   
   annotate("text", x = LRP - 3.5, y = 0.63,
            label = "LRP", angle = 90) +
   annotate("text", x = 25 - 3.5, y = 0.63,
            label = "USR", angle = 90) +
   annotate("text", x = 75, y = Frr + 0.04,
            label = "RR") +
   #annotate("text", x = TRP - 3.5, y = 0.63,
   #         label = "TRP", angle = 90) +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )
 
 p60 = ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotate("rect", xmin = LRP, xmax = 60,
            ymin = 0, ymax = 1.05,
            fill = "#DDD27F") +
   annotate("rect", xmin = 60, xmax = 100,
            ymin = 0, ymax = 1.05,
            fill = "#66EE00") +
   
   geom_vline(xintercept = c(LRP, 60), linetype = "dotted",linewidth=1.1) +
   
   geom_segment(aes(x = LRP, y = 0,
                    xend = 60, yend = Frr),
                linewidth =1.1) +
   
   geom_segment(aes(x = 60, y = Frr,
                    xend = 100, yend = Frr),
                linewidth = 0.8) +
   
   annotate("text", x = LRP - 3.5, y = 0.63,
            label = "LRP", angle = 90) +
   annotate("text", x = 60 - 3.5, y = 0.63,
            label = "USR", angle = 90) +
   annotate("text", x = 75, y = Frr + 0.04,
            label = "RR") +
   #annotate("text", x = TRP - 3.5, y = 0.63,
   #         label = "TRP", angle = 90) +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )
 
 
 p75 = ggplot() +
   annotate("rect", xmin = 0, xmax = LRP,
            ymin = 0, ymax = 1.05,
            fill = "#DD9271") +
   annotate("rect", xmin = LRP, xmax = 75,
            ymin = 0, ymax = 1.05,
            fill = "#DDD27F") +
   annotate("rect", xmin = 75, xmax = 100,
            ymin = 0, ymax = 1.05,
            fill = "#66EE00") +
   
   geom_vline(xintercept = c(LRP, 75), linetype = "dotted",linewidth=1.1) +
   
   geom_segment(aes(x = LRP, y = 0,
                    xend = 75, yend = Frr),
                linewidth =1.1) +
   
   geom_segment(aes(x = 75, y = Frr,
                    xend = 100, yend = Frr),
                linewidth = 0.8) +
   
   annotate("text", x = LRP - 3.5, y = 0.63,
            label = "LRP", angle = 90) +
   annotate("text", x = 75 - 3.5, y = 0.63,
            label = "USR", angle = 90) +
   annotate("text", x = 85, y = Frr + 0.04,
            label = "RR") +
   #annotate("text", x = TRP - 3.5, y = 0.63,
   #         label = "TRP", angle = 90) +
   scale_x_continuous(expand=c(0,0))+
   scale_y_continuous(expand=c(0,0))+
   coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.05)) +
   labs(x = "Biomass", y = "Fishing Mortality") +
   theme_classic(base_size=14) +
   theme(
     panel.background = element_rect(fill = "white", colour = NA),
     plot.background = element_rect(fill = "white", colour = NA),
     axis.text.x = element_blank(),
     axis.text.y = element_blank()
   )

 (p25 / p40) | (p60/p75 ) 
 
 