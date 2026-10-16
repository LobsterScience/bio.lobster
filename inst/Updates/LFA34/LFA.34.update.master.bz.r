# LFA 33 Update Script

	require(devtools)
	require(bio.lobster)
	require(bio.utilities)
	require(sf)
	require(ggplot2)
	require(dplyr)
	p = list()
	p$current.assessment.year=2026
	la()
	p$yrs <- 2026 #ensuring empty variable

	outfile <- function() {paste0(
	  tools::file_path_sans_ext("LFA34_AC_2026"),
	  "_",
	  format(Sys.time(), "%Y%m%d_%H%M%S"),
	  ".pdf"
	)
	}
		
#If running script after the fishery year, run this line
#p$current.assessment.year=p$current.assessment.year-1


	    # define place for figures to go
	    figdir = file.path("C:\\Users\\cooka\\OneDrive - DFO-MPO\\Presentations\\AdvisoryPresentations\\2025-2026\\LFA34_Preseason26-27")
	    dir.create( figdir, recursive = TRUE, showWarnings = TRUE )
	    p$lfas = c("34") # specify lfas for data summary
	    
	    setwd(figdir)
	    
	    #If you only want to update logs for the last two years, run this:
	    #p$yr=p$current.assessment.year

	    # update data through ROracle
	    NewDataPull =F
	    #NewDataPull =T
  p$yrs = 2026
	    if(NewDataPull){
	      lobster.db('logs.redo',pH=p)
	      lobster.db('annual.landings.redo')
	      lobster.db('seasonal.landings.redo')
	      #lobster.db('vlog.redo')
	      logs=lobster.db('process.logs.redo')
	      per.rec= lobster.db("percent_reporting")
	    }

#Run a report of missing vs received logs and save a csv copy
	    
fl.name=paste("percent_logs_reported", Sys.Date(),"csv", sep=".")
per.rec= lobster.db("percent_reporting")
per.rec <- per.rec[order(per.rec$YEARMTH), ]
p1 <- per.rec[
    substr(per.rec$YEARMTH, 1, 4) == as.character(p$current.assessment.year-1) &
        substr(per.rec$YEARMTH, 5, 6) %in% c("11", "12"),
]
p2<- per.rec[grepl(p$current.assessment.year, per.rec$YEARMTH), ]
per.rec=rbind(p1, p2)
columns_to_keep <- c("YEARMTH", grep("L33|L34", colnames(per.rec), value = TRUE))
per.rec <- per.rec[, columns_to_keep]
per.rec <- per.rec[!apply(per.rec[, -which(names(per.rec) == "YEARMTH")], 1, function(x) all(is.na(x))), ]


L33_miss <- sum(per.rec$L33MISS)
L33_recd <- sum(per.rec$L33RECD)
L33_percent <- 100 * L33_miss / (L33_miss + L33_recd)

L34_miss <- sum(per.rec$L34MISS, na.rm = TRUE)
L34_recd <- sum(per.rec$L34RECD, na.rm = TRUE)
L34_percent <- 100 * L34_miss / (L34_miss + L34_recd)

summary_tbl <- data.frame(
    LobsterArea = c("L33", "L34"),
    Miss = c(L33_miss, L34_miss),
    Recd = c(L33_recd, L34_recd),
    PercentMiss = c(L33_percent, L34_percent)
)

summary_tbl


# Make per.rec into character data frame
per.rec_out <- per.rec %>%
    mutate(across(everything(), as.character))

# Add a blank row for separation
blank_row <- as.data.frame(matrix("", ncol = ncol(per.rec_out)))
colnames(blank_row) <- colnames(per.rec_out)

# Make summary into the same shape by adding missing columns
summary_out <- summary_tbl %>%
    mutate(across(everything(), as.character)) %>%
    # add missing cols as blanks so it can bind
    tibble::add_column(
        YEARMTH = "",
        L33MISS = "",
        L33RECD = "",
        L33PERCENT = "",
        L34MISS = "",
        L34RECD = "",
        L34PERCENT = "",
        .before = 1
    )

# Bind everything together
final_out <- bind_rows(per.rec_out, blank_row, summary_out)
write.csv(final_out, file=paste0(figdir,"/",fl.name),na="", row.names=F)

# Map ################

		#x11(width=5, height=5)
	#	LobsterMap('33')
	#	savePlot(file.path(figdir,'LFA33map.png'),type='png')

		#or

		png(filename=file.path(figdir, "MapLFA34.png"),width=5, height=5, units = "in", res = 800)
    ggLobsterMap('34', addLFALabels = T, LFA_label_size = 8, addGrids = T)
		dev.off()

# CPUE ###############


	#
		logs=lobster.db("process.logs")

		
		#Choose one to redo or not Add TempSkip=T to not model CPUE with Temps
		#CPUE.data<-CPUEModelData2(p,redo=T)
		#CPUE.data<-CPUEModelData2(p,redo=F)
		#cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(p$current.assessment.year),graphic='R')$annual.data
	
	
	
		h = lobster.db('annual.landings')
		i = lobster.db('seasonal.landings')
		g = lobster.db('process.logs')
		
		ref = data.frame(LFA=c(27:30,'31A','31B',32,33),lrp=c(.14,.12,.11,.28,.16,.16,.14,.14),usr=c(.27,.25,.22,.56,.41,.32,.29,.28))
		
		g = subset(g, SYEAR<=p$current.assessment.year)
		
		#bring in voluntary log data to populate <2005
		fn.root =  file.path( project.datadirectory('bio.lobster'), "data")
		fnODBC  =  file.path(fn.root, "ODBCDump")
		get.vlog=load(file.path( fnODBC, "processed.vlog.rdata"),.GlobalEnv)
		v = subset(vlog,SYEAR<2005, select=c("SYEAR","W_KG","N_TRP","LFA"))
		names(v)=c("SYEAR","WEIGHT_KG","NUM_OF_TRAPS","LFA")
		v$LFA[v$LFA%in%c("27N","27S")] = "27"
		v$LFA[v$LFA%in%c("33W","33E")] = "33"
		va = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=v,FUN=sum)
		#merge vlog and logs here
		
		gag = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=g,FUN=sum)
	    ga=rbind(va, gag)
		ga$cpue = ga$WEIGHT_KG/ga$NUM_OF_TRAPS
		l = unique(ga$LFA)
		o = list()
		for(j in 1:length(l)){
		    n = subset(ga,LFA==l[j])
		    running.median = with(rmed(n$SYEAR,n$cpue),data.frame(SYEAR=yr,running.median=x))
		    o[[j]]=merge(n,running.median,all=T)
		}
		o = dplyr::bind_rows(o)
		
		##Above section can be dropped into 27-32 assessment
		
	    crd=subset(o,LFA==p$lfas[1])	
		names(crd)=c("YEAR", "LFA", "NUM_OFTRAPS","WEIGHT_KG", "CPUE", "running.median")
		crd = crd[is.finite(crd$CPUE),]
		usr = 0.28
		lrp = 0.14


	

		png(filename=file.path(figdir, "CPUE_LFA34.png"),width=8, height=5.5, units = "in", res = 800)
		par(mar=c(4.0,5.5,2.0,3.0))
		xlim=c(1990,max(crd$YEAR))
		plot(crd$YEAR,crd$CPUE,xlab='Year',ylab='CPUE (kg/TH)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
		points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
		running.median = with(rmed(crd$YEAR,crd$CPUE),data.frame(YEAR=yr,running.median=x))
		lines(running.median$YEAR,running.median$running.median,col='red',lwd=2)
		#crd=merge(crd,running.median,all=T)
		dev.off()

    
    write.csv(crd, file=paste0(figdir,file='CatchRateRefs34.csv'), row.names=F )
    save(crd, file=paste0(figdir, "/cpueData.Rdata") )
   
    

    a = lobster.db('process.logs')
    a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
    
    aa = split(a,f=list(a$LFA,a$SYEAR))
    cpue.lst<-list()
    m=0
    #by time
    for(i in 1:length(aa)){
      tmp<-aa[[i]]
      tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
      names(tmp)<-c('time','catch','effort')
      tmp$date<-as.Date(tmp$time)
      first.day<-min(tmp$date)
      tmp$time<-julian(tmp$date,origin=first.day-1)
      tmp$time = ceiling(tmp$time/7) #convert to week of season
      if(nrow(tmp)>5){
        m=m+1
        g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
        g$lfa=unique(aa[[i]]$LFA)
        g$yr = unique(aa[[i]]$SYEAR)
        g = t(g)[,2]
        cpue.lst[[m]] <- g
      }
    }
    cc =as.data.frame(do.call(rbind,cpue.lst))
    cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
    cc = cc[order(cc$lfa,cc$yr),]
    cc$yr = as.numeric(cc$yr)
    cc$fyr = as.factor(cc$yr)
 #   i = which(cc$yr==2026)
#    j = which(cc$lfa %in% c(27,28,29,30,'31A','31B','32'))
#    cc = cc[-intersect(i,j),] #2026 are elogs in these LFAs and we have not checked this data yet leave out for now
    
      cc1 = cc
      
      cc$highlight <- ifelse(cc$yr == max(cc$yr), "Last", "Other")
      png(filename=file.path(figdir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
      ggplot(cc, aes(yr, CPUE)) +
        geom_smooth(se = FALSE) +
        geom_point(aes(colour = highlight)) +
        facet_wrap(~lfa, scales = "free_y") +
        scale_colour_manual(values = c(Other = "black",
                                       Last = "orange")) +
        theme(legend.position = "none")
      dev.off()
      
      #Unbiased cpue patterns by week of season
  
      #-----------------------------------------
   
        
       
        aa = split(a,f=list(a$LFA,a$SYEAR))
        aa = rm.from.list(aa)
        cpue.lst<-list()
        
        #by time
        for(i in 1:length(aa)){
          
          tmp <- aa[[i]]
          tmp <- tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
          names(tmp) <- c('time','catch','effort')
          
          tmp$date <- as.Date(tmp$time)
          first.day <- min(tmp$date)
          
          tmp$time <- julian(tmp$date, origin = first.day - 1)
          tmp <- tmp[order(tmp$time),]
          tmp$time <- ceiling(tmp$time/7)
          
          # Check whether any week has >5 observations
          n.by.week <- table(tmp$time)
          
          if(any(n.by.week > 5)){
            
            g <- as.data.frame(
              biasCorrCPUE(
                tmp,
                by.time = TRUE,
                min.sample.size = 5
              )
            )
            
            g$lfa <- unique(aa[[i]]$LFA)
            g$yr <- unique(aa[[i]]$SYEAR)
            
            cpue.lst[[i]] <- g
          }
        }
        
        cc =as.data.frame(do.call(rbind,cpue.lst))
        
        mean= aggregate(cc, CPUE~yr+lfa,mean )
        om = aggregate(CPUE~lfa,data=cc,FUN=function(x) mean(x))
        
        l=p$lfas
        png(filename=file.path(figdir, paste0("weekly_cpue_",l,".png")),width=8, height=5.5, units = "in", res = 800)
        print(
          ggplot(subset(cc,lfa==l),aes(x=t,y=CPUE)) +
            geom_point(size = 0.75) +
            geom_smooth(se=F, linewidth=0.75) +
            facet_wrap(~yr) +
            labs(title = paste0("LFA ",l)) +
            labs(y = "CPUE (kg/th)", x = "Week of Season") +
            theme(plot.title = element_text(hjust = 0.5), 
                  strip.text = element_text(size = 7, margin = margin(1,1,1,1))) +
            geom_hline(
              data = mean[mean$lfa==l,],
              aes(yintercept = CPUE),
              color = 'red',
              linewidth = 0.6)
            )
        dev.off()
   
  
        
        week_mean <- subset(cc,lfa==l) %>%
          group_by(t) %>%
          summarise(mu_week = mean(CPUE, na.rm = TRUE),
                    .groups = "drop")
        cc2 <- left_join(subset(cc,lfa==l), week_mean, by = "t") %>%
          mutate(dev = CPUE - mu_week)
  
        
        library(dplyr)
        
        insert_zero_crossings <- function(dat,x = "t",y = "dev",group = "yr") {
          sp <- split(dat, dat[[group]])
                    out <- lapply(sp, function(d) {
                      d <- d[order(d[[x]]), ]
                      res <- list()
                    for(i in seq_len(nrow(d)-1)) {
                          res[[length(res)+1]] <- d[i, ]
                          y1 <- d[[y]][i]
              y2 <- d[[y]][i+1]
                if(y1 * y2 < 0) {
                        x1 <- d[[x]][i]
                x2 <- d[[x]][i+1]
                xc <- x1 + (0 - y1) * (x2 - x1)/(y2 - y1)
                z <- d[i, ]
                z[[x]] <- xc
                z[[y]] <- 0
                res[[length(res)+1]] <- z
              }
            }
                    res[[length(res)+1]] <- d[nrow(d), ]
                    bind_rows(res)
          })
                  bind_rows(out)
        }
        
        
              
        cc2 <- cc2 |>
          dplyr::group_by(yr) |>
          dplyr::arrange(t) |>
          dplyr::mutate(
            xend = lead(t),
            yend = lead(dev),
            col = ifelse((dev + yend)/2 >= 0, "Above", "Below")
          ) |>
          dplyr::ungroup()
    
        cc3 <- insert_zero_crossings(cc2)
        
        cc3 <- cc3 %>%
          arrange(yr, t) %>%
          group_by(yr) %>%
          mutate(
            xend = lead(t),
            yend = lead(dev),
            col = ifelse((dev + yend)/2 >= 0,
                         "Above", "Below")
          ) %>%
          ungroup()
        
    png(filename=file.path(figdir, paste0("weekly_cpue_deviates",l,".png")),width=8, height=5.5, units = "in", res = 800)
        ggplot(cc3) +
          geom_hline(yintercept = 0, colour = "grey50") +
          geom_segment(
            aes(
              x = t,
              y = dev,
              xend = xend,
              yend = yend,
              colour = col
            ),
            linewidth = 0.5
          ) +
          scale_colour_manual(
            values = c(
              Above = "blue",
              Below = "red"
            )
          ) +
          facet_wrap(~yr) +
          theme_bw() +
          theme(
            panel.grid = element_blank(),
            strip.text = element_text(size = 7),
            legend.position = "none"
          ) +
          labs(
            x = "Week of Season",
            y = "Deviation from weekly mean CPUE (kg/th)"
          ) 
        
        dev.off()  
        
        ###
        v = subset(cc2, yr==2024)
        v$col = ifelse(v$unBCPUE>v$mu_week,'blue','red')
        
        png(filename=file.path(figdir, paste0("weekly_cpue_deviates_expl",l,".png")),width=8, height=5.5, units = "in", res = 800)
            ggplot(v,aes(x=t))+
          geom_point(aes(y=unBCPUE),colour='black')+
          geom_line(aes(y=unBCPUE),colour='black')+
          geom_point(aes(y=mu_week),colour='grey70')+
          geom_line(aes(y=mu_week),colour='grey70')+
          geom_segment(
            data = v,   aes(x = t,xend = t, y = mu_week,yend = unBCPUE, colour = col),
            arrow = arrow(length = unit(0.08, "inches")),linewidth = 0.6
          ) +
          scale_colour_identity()+
          theme_classic()+
              labs(
                x = "Week of Season",
                y = "Weekly mean CPUE (kg/th)"
              )
          dev.off()
        
        
      #######-----------------------------------------

# Landings and Effort ############
land = lobster.db('seasonal.landings')
    
    
#if running this section without having done the CPUE analysis during the same session, run the line below 
#load (file=paste0(figdir, "/cpueData.Rdata") )


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA33
		fishData = merge(crd,land[,c("YEAR","LANDINGS")])
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / crd$CPUE
		
		

		# plot Landings

		png(filename=file.path(figdir, "Landings_LFA34.png"),width=8, height=5, units = "in", res = 800)
    		par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
    		plot(fishData$YEAR,fishData$LANDINGS,xlab=NA,ylab='Landings (t)',type='h',main=NA,ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
    		lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
    dev.off()
    
        #plot effort
    png(filename=file.path(figdir, "Effort_LFA34.png"),width=8, height=5, units = "in", res = 800)
    plot(fishData$YEAR,fishData$EFFORT2/1000,ylab="Effort ('000s Trap Hauls)",xlab='', type='b', pch=16,ylim=c(0,max(fishData$EFFORT2/1000,na.rm=T)))
    points(max(fishData$YEAR),fishData$EFFORT2[length(fishData$EFFORT2)]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
    dev.off()
    

	  
# to compare weekly fishing effort year to year (include in AC presentation if desired)
      logs=lobster.db("process.logs")
      
     logs34=logs[logs$LFA=="34",]
	  logs34$unique_days=paste(logs34$VR_NUMBER, logs34$DATE_FISHED, sep=':')
	  
	  #-----------------------------------------------------------------------
	  
	

	  #To Double Check Number of Fishing Days in each week of season
	  days=aggregate(DATE_FISHED~WOS+SYEAR,data=logs34,FUN=function(x) length(unique(x)))
	  days.y0=days[days$SYEAR==max(days$SYEAR),]
	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
	  plot(x=days$WOS,y=days$DATE_FISHED, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(0,27))
	  lines(days.y0$WOS, days.y0$DATE_FISHED, col="red", lwd=2)
	  text(paste(days.y0$SYEAR[1]), x=25, y=1, col="red", cex=1.6)
	  lines(days.y1$WOS, days.y1$DATE_FISHED, col="blue")
	  text(paste(days.y1$SYEAR[1]), x=25, y=2, col="blue", cex=1.6)

	  # to plot weekly fishing effort year to year (include in AC presentation if desired)
	  png(filename=file.path(figdir, "Weekly_Comparison_effort.png"),width=8, height=5.5, units = "in", res = 1200)
    	  days=aggregate(unique_days~WOS+SYEAR, data=logs34, length)
    	  days.y0=days[days$SYEAR==max(days$SYEAR),]
    	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
    	  plot(x=days$WOS,y=days$unique_days, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(0,27))
    	  #axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
    	  #plot past 10 years in light gray)
    	  for (i in c(0:10)){
    	  day=days[days$SYEAR==(max(days$SYEAR)-i),]  
    	  lines(day$WOS, day$unique_days, col="gray83") 
    	   }
    	  lines(days.y0$WOS, days.y0$unique_days, col="red")
    	  lines(days.y1$WOS, days.y1$unique_days, col="blue")
    	  legend(x=21, y=7000, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray88", "blue", "red"), bty='n')
    	  dev.off()

	  # to plot weekly fishing CPUE year to year (include in AC presentation if desired)
	  png(filename=file.path(figdir, "Weekly_Comparison_cpue.png"),width=8, height=5.5, units = "in", res = 1200)
	  days=aggregate(CPUE~WOS+SYEAR, data=logs34, FUN='mean')
	  days.y0=days[days$SYEAR==max(days$SYEAR),]
	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
	  plot(x=days$WOS,y=days$CPUE, type='n', main= "Average CPUE by Week", xlab="Week of Season", ylab="CPUE (kg/trap)", xlim=c(0,27))
	  for (i in c(0:10)){
	    day=days[days$SYEAR==(max(days$SYEAR)-i),]  
	    lines(day$WOS, day$CPUE, col="gray82")
	  }
	  #axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
	  lines(days.y0$WOS, days.y0$CPUE, col="red")
	  lines(days.y1$WOS, days.y1$CPUE, col="blue")
	  legend(x=0.5, y=0.65,cex=0.8, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray82", "blue", "red"), bty='n')
	  dev.off()

	
	  ####grids

	  layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
	  r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
	  r = st_as_sf(r)
	  
	  a =  lobster.db('process.logs')
	  a = subset(a,SYEAR>1998 & SYEAR<=p$current.assessment.year & LFA==34)
	  b = lobster.db('seasonal.landings')
	  b = subset(b,!is.na(SYEAR))
	  b$SYEAR = 1976:p$current.assessment.year
	   b = data.frame(SYEAR=b$SYEAR,LFA34 = b$LFA34)
	  names(b)[1:2]=c('YR','SlipLand')
	  b$LFA=34
	  bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
	  bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
	  bAll = merge(b,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))
	  
	  sL= split(a,f=list(a$LFA, a$SYEAR))
	  sL = rm.from.list(sL)
	  cpue.lst<-list()
	  cpue.ann = list()
	  
	  for(i in 1:length(sL)){
	    tmp<-sL[[i]]
	    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
	    names(tmp)<-c('time','catch','effort')
	    tmp$date<-as.Date(tmp$time)
	    first.day<-min(tmp$date)
	    tmp$time<-julian(tmp$date,origin=first.day-1)
	    g<-biasCorrCPUE(tmp,by.time = F)
	    cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
	  }
	  
	  cc =as.data.frame(do.call(rbind,cpue.lst))
	  
	  cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))
	  
	  cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
	  cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
	  cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)
	  
	  
	  ###########################################
	  #part the effort to grids
	  
	  partEffort = list()
	  
	  for(i in 1:length(sL)){
	    tmp = sL[[i]]
	    tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
	    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
	    pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
	    pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
	    pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
	    pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
	    
	    partEffort[[i]] = pTH
	  }
	  
	  partEffort = do.call(rbind, partEffort)
	  
	  #pe = merge(partEffort,r,by.x=c('GRID_NUM','LFA'),by.y=c('GRID_NO','LFA'))
	  
	  saveRDS(partEffort,'TrapHaulsWithinGrid.rds')
	  
	  
	  #############################################
	  # PartitionLandings to Grids
	  
	  partLandings = list()
	  
	  for(i in 1:length(sL)){
	    tmp = sL[[i]]
	    tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
	    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
	    pTH = aggregate(WEIGHT_KG~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
	    pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
	    partLandings[[i]] = pTH
	  }
	  
	  partLandings = do.call(rbind, partLandings)
	  
	  saveRDS(partLandings,'LandingsWithinGrid.rds')
	  
	  ###################################################
	  ##Licenses By Grid and Week
	  
	  g = lobster.db('process.logs')
	  g = subset(g, SYEAR<=p$current.assessment.year)
	  
	  gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))
	  
	  saveRDS(gg,'SDLOGSWithinGrid.rds')
	  
	  #############merge
	  #Licenses By Grid and Week
	  
	  g = lobster.db('process.logs')
	  g = subset(g, SYEAR<=p$current.assessment.year)
	  
	  gKL = aggregate(LICENCE_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))
	  
	  saveRDS(gKL,'LicencesWithinCommunity.rds')
	  
	  #############merge
	  
	  
	  Tot = merge(merge(merge(partEffort,partLandings),gg),gKL)
	  
	  Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,BTTH,BL,SD_LOG_ID,LICENCE_ID))
	  names(Tot)= c('FishingYear','LFA','Grid','TrapHauls','Landings','Trips','NLics')
	  
	  #Remove grids fall under "rule of 5"
	  Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)
	  calcTot = Tot
	  Tot=subset(Tot, PrivacyScreen==1)
	   saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
	  
	  Tot = readRDS('PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
	  
	  
	  #making plots of Tot
	  
	  GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
	  coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
	  
	  
	  GrMap1 = GrMap
	  GrMap1$area = st_area(GrMap1)/1000000
	  GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
	  st_geometry(GrMap1)<- NULL
	  gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))
	  
	  GrMap2 =merge(GrMap,gg)
	  
	  gTot = merge(subset(GrMap2,LFA==34),Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)
	  calcgTot = merge(subset(GrMap2,LFA==34),calcTot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)
	  
	  r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
	  gTot$CPUE = gTot$Landings/gTot$TrapHauls
	  
	  #Can run this line to only take certain LFAs / years
	  g27p = subset(gTot, FishingYear%in%2016:p$current.assessment.year)
	  
	  gTot = gTot %>%
	    group_by(FishingYear) %>%
	    mutate(prop_landings = Landings / sum(Landings, na.rm = TRUE)) %>%
	    ungroup()
	  	  
	  ok1 = ggplot(subset(gTot, !is.na(FishingYear) & FishingYear %% 2 | FishingYear==2026), aes(fill = Landings)) +
	      geom_sf() +
	      scale_fill_distiller(trans = 'identity', palette = 'Spectral',labels=scales::label_number(scale=1e-5)) +
	 #   scale_fill_distiller(trans = 'identity', palette = 'Spectral') +
	      geom_sf(data = coa, fill = 'grey') +
	      geom_sf(data = GrMap, fill = NA) +
	      coord_sf(
	          xlim = c(st_bbox(g27p)$xmin, st_bbox(g27p)$xmax),
	          ylim = c(st_bbox(g27p)$ymin, st_bbox(g27p)$ymax),
	          expand = FALSE
	      ) +
	    facet_wrap(~FishingYear,ncol=5)+
	      theme_grey(base_size = 12) +
	      theme(
	          panel.grid = element_blank(),
	          axis.text = element_blank(),
	          axis.ticks = element_blank(),
	          axis.title = element_blank(),
	          panel.background = element_rect(fill = "grey96", colour = NA) ,
	          plot.margin = margin(0,0,0,0),legend.position = 'bottom',
	          panel.spacing = unit(0.03,'lines'),
	          plot.title = element_text(hjust = 0.5), 
	          strip.text = element_text(size = 7, margin = margin(1,1,1,1))
	          # lighter grey background
	      )+
	    guides(
	      fill = guide_colourbar(
	        title.position = "top",
	        barwidth = unit(3, "cm"),
	        barheight = unit(0.2, "cm")
	      )
	    )
	  
	  output_png <- file.path(figdir, "landings_grid_all.png")
	  png(filename = output_png, width = 1200, height = 900, res = 175)
	    print(ok1)
	  dev.off()

	  #Slice out individual years
	  gl = subset(g27p,FishingYear==p$current.assessment.year-10)
	  
	  gp = subset(g27p,FishingYear==p$current.assessment.year)
	  
	  gl$geometry<- NULL
	  
	  gg = merge(gp,gl[,c('LFA','GRID_NO','CPUE')],by=c('LFA','GRID_NO'))
	  
	  
	  ls=unique(gg$LFA)
	  print(paste0('Looking at the following LFA(s):', ls,' for the following years: ', gl$FishingYear[1], ' & ',gp$FishingYear[1] ))
	  require(colorspace)
	  percent_diff <- function(row) {
	    row$geometry<- NULL
	    
	    abs_diff <- (as.numeric(row[1]) - as.numeric(row[2]))
	    mean_val <- mean(as.numeric(row))
	    percent_diff <- (abs_diff / mean_val) * 100
	    return(percent_diff)
	  }
	  
	  gg$percentChange =  apply(gg[,c('CPUE.x','CPUE.y')],1,percent_diff)
	  
	  
	  library(ggplot2)
	  library(sf)
	  library(grid)
	  
	  # -----------------------------
	  # 1️⃣ Create label
	  # -----------------------------
	  lab <- paste0(gl$FishingYear[1], " -> ", gp$FishingYear[1])
	  
	  # -----------------------------
	  # 2️⃣ Build the original map
	  # -----------------------------
	  cpue.diff = {
	      ggplot(subset(gg, PrivacyScreen == 1), aes(fill = percentChange)) +
	          geom_sf() +
	          scale_fill_continuous_diverging(palette = "Purple-Green") +
	          labs(fill = "CPUE\n% Change") +
	          geom_sf(data = coa, fill = "grey") +
	          geom_sf(data = GrMap, fill = NA) +
	          coord_sf(
	              xlim = c(st_bbox(g27p)$xmin, st_bbox(g27p)$xmax),
	              ylim = c(st_bbox(g27p)$ymin, st_bbox(g27p)$ymax),
	              expand = FALSE
	          ) +
	          theme_grey(base_size = 12) +
	          theme(
	              axis.title = element_blank(),
	              axis.text = element_blank(),
	              axis.ticks = element_blank(),
	              panel.grid = element_blank(),
	              panel.background = element_rect(fill = "grey85", colour = NA),  # lighter grey background
	              plot.margin = grid::unit(c(8, 2, 8, 2), "mm")
	          ) +
	          annotate(
	              "text",
	              x = (st_bbox(g27p)$xmax) - 0.9,
	              y = (st_bbox(g27p)$ymin) + 0.2,
	              label = lab,
	              size = 8
	          )
	  }
	  
	
	  # -----------------------------
	  # 3️⃣ Export PDF and overlay label
	  # -----------------------------
	  output_png <- file.path(figdir, "cpue.diff.png")
	  
	  png(filename = output_png, width = 1200, height = 900, res = 175)
	  print(cpue.diff)
	  
	  # Overlay bottom-right label
	  grid.text(
	      label = lab,
	      x = unit(0.98, "npc"),  # near right edge
	      y = unit(0.02, "npc"),  # near bottom
	      hjust = 1,
	      vjust = 0,
	      gp = gpar(col="white", fontsize=16, fontface="bold")
	  )
	  
	  dev.off()
	  
#############center of gravity
	 
	  ####
	  require(mgcv)
	  require(bio.lobster)
	  require(spdep)
	  require(sf)
	  require(devtools)
	  require(dplyr)
	  la()
	  

	  
	  
	  g = lobster.db('process.logs')
	  aT = subset(g, SYEAR>1998 & LFA==34)
	  po = lobster.db('port_location')
	  
	  
	  gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
	  coa = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','CoastSF.rds')))
	  coa= st_make_valid(coa)
	  coa = subset(coa,PROVINCE=='Nova Scotia')
	  coa = subset(coa,st_area(coa)==max(st_area(coa))) # remove islands and cape breton
	  
	  gr$GRID_NO = as.numeric(gr$GRID_NO)
	  #remove the islands or multipart polygons and keep only the biggest ones
	  gr <- subset(gr,LFA==34) %>%
	    mutate(area = st_area(geometry)) %>%
	    group_by(GRID_NO, LFA) %>%
	    slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
	    ungroup() %>%
	    select(-area)
	  gall = gtot = gr
	  gtot$centroid = st_centroid(gtot)
	  cen_coords = st_coordinates(gtot$centroid)
	  gtot$X = cen_coords[,1] 
	  gtot$Y = cen_coords[,2] 
	  gtot$centroid <- NULL
	  gtot$geometry <- NULL
	  gtots = st_as_sf(gtot,coords=c('X','Y'),crs=4326)
	  gtot = subset(gtots,LFA %in% c(34))
	  gtot$X <- st_coordinates(gtot)[,1]
	  gtot$Y <- st_coordinates(gtot)[,2]
	  st_geometry(gtot) <- NULL
	  gtot = as_tibble(gtot)
	  po3 = subset(po, PORT_CODE %in% unique(aT$COMMUNITY_CODE) & LFA ==34)

	   correctPorts_aT = subset(aT, COMMUNITY_CODE %in% po3$PORT_CODE) 
	  cwg = merge(correctPorts_aT,gtot,by.x=c('LFA','GRID_NUM'),by.y=c('LFA','GRID_NO'))
	  cwg = merge(cwg,po3[,c('PORT_CODE','CENTLAT','CENTLON')],by.x='COMMUNITY_CODE',by.y='PORT_CODE')
	  require(geosphere)
	  
	  cwg$dist2shore = geosphere::distHaversine( cbind(cwg$CENTLON,cwg$CENTLAT), cbind(cwg$X,cwg$Y))/1000
	  
	  ##center of gravity for both catch and effort
	  cc = split(subset(cwg,!is.na(NUM_OF_TRAPS)),f=subset(cwg,!is.na(NUM_OF_TRAPS))$SYEAR)
	  o = list()
	  for(i in 1:length(cc)){
	    xcw = sum(cc[[i]]$X*cc[[i]]$WEIGHT_KG)/sum(cc[[i]]$WEIGHT_KG)
	    xce = sum(cc[[i]]$X*cc[[i]]$NUM_OF_TRAPS)/sum(cc[[i]]$NUM_OF_TRAPS)
	    ycw = sum(cc[[i]]$Y*cc[[i]]$WEIGHT_KG)/sum(cc[[i]]$WEIGHT_KG)
	    yce = sum(cc[[i]]$Y*cc[[i]]$NUM_OF_TRAPS)/sum(cc[[i]]$NUM_OF_TRAPS)
	    d2se = sum(cc[[i]]$dist2shore*cc[[i]]$NUM_OF_TRAPS)/sum(cc[[i]]$NUM_OF_TRAPS)
	    d2sc = sum(cc[[i]]$dist2shore*cc[[i]]$WEIGHT_KG)/sum(cc[[i]]$WEIGHT_KG)
	    
	      o[[i]] = c(unique(cc[[i]]$SYEAR),xce,xcw,yce,ycw,d2se,d2sc)      
	  }
	  
	  w = as.data.frame(do.call(rbind,o))
	  names(w) = c('SYEAR','Xef','Xca','Yef','Yca','DistEff','DistCat')
	  
	  g = ggLobsterMap('34',return.object = T)
	  gca = g+geom_point(data=w,aes(x=Xca,y=Yca,colour='red'))+geom_path(data=w,aes(x=Xca,y=Yca,colour='red'))
	  gef = g+geom_point(data=w,aes(x=Xef,y=Yef,colour='red'))+geom_path(data=w,aes(x=Xef,y=Yef,colour='red'))
	  
	  #total distance and bearing for catch and effort
	  require(geosphere)
	  
	  dist.effort = (distVincentySphere(w[-1,c('Xef','Yef')],w[-nrow(w),c('Xef','Yef')]))
	  bear.effort = (bearing(w[-nrow(w),c('Xef','Yef')],w[-1,c('Xef','Yef')]))
	  
	  bear.effort.weight = sum(dist.effort*bear.effort) / sum(dist.effort)
	  ov_dist.ef = distVincentySphere(w[1,c('Xef','Yef')],w[nrow(w),c('Xef','Yef')])
	  ov_bear.ef = bearing(w[1,c('Xef','Yef')],w[nrow(w),c('Xef','Yef')])
	  dest.ef <- destPoint(p=c(w$Xef[1],w$Yef[1]),  b=ov_bear.ef, d= ov_dist.ef)
	  def = data.frame(sx = w$Xef[1],sy = w$Yef[1], ex = dest.ef[1],ey = dest.ef[2])
	  
	  dist.cat = distVincentySphere(w[-1,c('Xca','Yca')],w[-nrow(w),c('Xca','Yca')])
	  bear.cat = bearing(w[-1,c('Xca','Yca')],w[-nrow(w),c('Xca','Yca')])
	  bear.cat.weight = sum(dist.cat*bear.cat) / sum(dist.cat)
	  
	  ov_dist.ca = distVincentySphere(w[1,c('Xca','Yca')],w[nrow(w),c('Xca','Yca')])
	  ov_bear.ca = bearing(w[1,c('Xca','Yca')],w[nrow(w),c('Xca','Yca')])
	  dest.ca <- destPoint(p=c(w$Xca[1],w$Yca[1]),  b=ov_bear.ca, d= ov_dist.ca)
	  dca = data.frame(sx = w$Xca[1],sy = w$Yca[1], ex = dest.ca[1],ey = dest.ca[2])
	  
	  library(ggplot2)
	  library(ggrepel)
	  
	  yrs <- c(min(w$SYEAR),
	           seq(2000, 2025, by = 5),
	           max(w$SYEAR))
	  
	  xlim <- range(c(w$Xca, w$Xef), na.rm = TRUE)
	  ylim <- range(c(w$Yca, w$Yef), na.rm = TRUE)
	  png(filename = 'cog_catch.png', width = 1200, height = 900, res = 175)
	    ggplot(w, aes(Xca, Yca)) +
	    geom_path(colour = "grey60", linewidth = 1) +
	    geom_point(aes(colour = SYEAR), size = 3) +
	    geom_text_repel(
	      data = subset(w, SYEAR %in% yrs),
	      aes(label = SYEAR),
	      size = 5
	    ) +
	    scale_colour_viridis_c() +
	      coord_equal(xlim = xlim, ylim = ylim)+
	    labs(x="Longitude",y='Latitude',title = 'Center of Catch')+
	    theme(legend.position  = 'none')
	    
	  dev.off()
	 
	  png(filename = 'cog_effort.png', width = 1200, height = 900, res = 175)
	  ggplot(w, aes(Xef, Yef)) +
	    geom_path(colour = "grey60", linewidth = 1) +
	    geom_point(aes(colour = SYEAR), size = 3) +
	    geom_text_repel(
	      data = subset(w, SYEAR %in% yrs),
	      aes(label = SYEAR),
	      size = 5
	    ) +
	    scale_colour_viridis_c() +
	    coord_equal(xlim = xlim, ylim = ylim)+
	    labs(x="Longitude",y='Latitude',title = 'Center of Effort')+
	    theme(legend.position  = 'none')
	  
	  dev.off()
	  
	 
	   wef= ggplot(w, aes(Xef, Yef)) +
	      geom_path(colour = "grey60", linewidth = 1) +
	      geom_point(aes(colour = SYEAR), size = 3) +
	      geom_text_repel(
	        data = subset(w, SYEAR %in% yrs),
	        aes(label = SYEAR),
	        size = 5
	      ) +
	      scale_colour_viridis_c() +
	      coord_equal() +
	      labs(x="Longitude",y='Latitude',title = 'Center of Effort')+
	    theme(base_size = 16,legend.position = 'none')
	 
	   wca= ggplot(w, aes(Xca, Yca)) +
	      geom_path(colour = "grey60", linewidth = 1) +
	      geom_point(aes(colour = SYEAR), size = 3) +
	      geom_text_repel(
	        data = subset(w, SYEAR %in% yrs),
	        aes(label = SYEAR),
	        size = 5
	      ) +
	      scale_colour_viridis_c() +
	      coord_equal() +
	      labs(x="Longitude",y='Latitude',title = 'Center of Catch')+
	      theme(base_size = 16,legend.position = 'none')
	   
	   
	   
	    
	   ################how many grids make the 95% of catch
	   library(dplyr)
	   
	   g95 <- calcgTot |>
	     st_drop_geometry() |>
	     group_by(FishingYear, GRID_NO) |>
	     summarise(Landings = sum(Landings, na.rm = TRUE),
	               .groups = "drop") |>
	     arrange(FishingYear, desc(Landings)) |>
	     group_by(FishingYear) |>
	     mutate(
	       pLand = Landings / sum(Landings),
	       cpLand = cumsum(pLand)
	     ) |>
	     summarise(
	       nGrid95 = which(cpLand >= 0.95)[1],
	       totalGrids = n(),
	       propGrids95 = nGrid95 / totalGrids,
	       .groups = "drop"
	     )
	   
	   g95$maxGr = max(g95$totalGrids)
	   g95$PropTotal = g95$nGrid95 / g95$maxGr
	   
	   
	   oo = ggplot(g95, aes(FishingYear, PropTotal)) +
	     geom_line() +
	     geom_point() +
	     scale_y_continuous(labels = scales::percent) +
	     labs(
	       x = "Year",
	       y = "% of Total grids accounting for 95% of landings"
	     ) +
	     theme_bw()
	   
	   
	   ggplot(g95, aes(FishingYear, totalGrids)) +
	     geom_line() +
	     geom_point() +
	     labs(
	       x = "Year",
	       y = "Number of Grids Reported"
	     ) +
	     theme_bw()
#################################################################	   
#####landing per vessel
	   
	   
	   
	   
	   g = lobster.db('landings_by_vessel')
	   g$year = g$SYEAR
	   g$LFA = ifelse(g$LFA=='38B',38,g$LFA)
	   glo = aggregate(MT~LFA+year,data=g,FUN=function(x) quantile(x,0.25)); names(glo)[3]='lCI'
	   gup = aggregate(MT~LFA+year,data=g,FUN=function(x) quantile(x,0.75)); names(gup)[3]='uCI'
	   gme = aggregate(MT~LFA+year,data=g,FUN=mean); names(gme)[3]='MEAN_T'
	   
	   g = merge(merge(glo,gup),gme)
	   g = subset(g,LFA==34)
	   ggplot(g,aes(x=year,y=MEAN_T))+
	     geom_point()
	   
	   
#####
###price
	   
	   a = lobster.db('process.logs')
	   a = subset(a, LFA %in% c(34) & SYEAR < 2026 & SYEAR>1998)
	   
	   xg = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+LICENCE_ID+LFA+GRID_NUM,data=a,FUN=sum)
	   sp = lobster.db('process_slips')
	   sp$woy = lubridate::week(sp$Date)
	   
	   sp = aggregate(PRICE~woy+LFA+SYEAR,data=sp,FUN=median)
	   ggplot(subset(sp, SYEAR>2001 & LFA %in% c(34)),aes(woy,PRICE, group=LFA,colour=LFA))+geom_point()+facet_wrap(~SYEAR)+theme_test()
	   a$woy = lubridate::week(a$DATE_FISHED)
	   
	   asp = merge(a,sp)
	   asp$valuePerTrip = asp$WEIGHT_KG*2.20462*(asp$PRICE)
	   asp$valuePerTrap = asp$WEIGHT_KG*2.20462*(asp$PRICE)/asp$NUM_OF_TRAPS
	   
	   oo = aggregate(valuePerTrip~LFA+SYEAR,data=asp,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))
	   oa = aggregate(valuePerTrap~LFA+SYEAR,data=asp,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))
	   
	   ggplot(oo,aes(x=SYEAR,y=valuePerTrip[,2],ymin=valuePerTrip[,1],ymax=valuePerTrip[,3]))+geom_point()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Value Per Trip')+theme_test(base_size = 14)
	   
	   
	   ggplot(oa,aes(x=SYEAR,y=valuePerTrap[,2],ymin=valuePerTrap[,1],ymax=valuePerTrap[,3]))+geom_point()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Value Per Trap')+theme_test(base_size = 14)
	   
	   
	   
	   
	   x1 = aggregate(WEIGHT_KG~SYEAR+LFA+WOS,data=subset(asp,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=sum)
	   x1$P=x1$WEIGHT_KG
	   
	   
	   xxx = split(x1,f=list(x1$LFA,x1$SYEAR))
	   junk = list()
	   
	   for(i in 1:length(xxx)){
	     o = xxx[[i]]
	     o$prp = cumsum(o$P) / sum(o$P)
	     junk[[i]] = o  
	   }
	   x1a = do.call(rbind,junk)
	   x1a$Fishing_Season =x1a$SYEAR
	   ggplot(subset(x1a),aes(x=WOS,y=prp,group=Fishing_Season,colour=Fishing_Season))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Week of Season')+ylab('Proportion of Total Landings')+theme_test(base_size = 14)
	   
	   
	   x1 = aggregate(valuePerTrip~SYEAR+LFA+WOS,data=subset(asp,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=sum)
	   x1$P=x1$valuePerTrip
	   
	   
	   xxx = split(x1,f=list(x1$LFA,x1$SYEAR))
	   junk = list()
	   
	   for(i in 1:length(xxx)){
	     o = xxx[[i]]
	     o$prp = cumsum(o$P) / sum(o$P)
	     junk[[i]] = o  
	   }
	   x1a = do.call(rbind,junk)
	   x1a$Fishing_Season =x1a$SYEAR
	   ggplot(x1a,aes(x=WOS,y=prp,group=Fishing_Season,colour=Fishing_Season))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Week of Season')+ylab('Proportion of Total Income')+theme_test(base_size = 14)
	   
#############################################
	   ##landings
	   
	   require(bio.lobster)
	   require(PBSmapping)
	   la()
	   a = lobster.db('annual.landings')
	   b = lobster.db('seasonal.landings')
	   d = lobster.db('historic.landings')
	   l34 = c("YARMOUTH","DIGBY")
	   l35 = c("KINGS","ANNAPOLIS", "COLCHESTER" , "CUMBERLAND")
	   l36 = c('ALBERT','SAINT JOHN','CHARLOTTE')
	   l38 = c('CHARLOTTE')
	   
	   d = subset(d, !is.na(LFA))
	   d = aggregate(LANDINGS_MT~YEAR+LFA,data=d,FUN=sum)
	   d = subset(d, YEAR<1947)
	   
	   names(d) = c('YR','LFA','LAND')
	   
	   b$YR = substr(b$SYEAR,6,9)
	   a = subset(a,YR<1976)
	   b = subset(b,YR>1975 & YR<=2026)
	   
	   i = c('LFA34')
	   
	     aa = a[,c('YR',i)]
	     bb = b[,c('YR',i)]
	     dd = subset(d,LFA==i)
	     dd$LFA <- NULL
	     names(dd)[2] <- i
	     aa = rbind(rbind(aa,bb),dd)
	     aa = aa[order(aa$YR),]
	   
 	     
	     plot(aa$YR,aa$LFA34,type='h',lwd=4,col='black',xlab='Year',ylab='Landings (t)')
	     lines(aa$YR,runmed(aa[,i],3),col='salmon',lwd=3)
	   