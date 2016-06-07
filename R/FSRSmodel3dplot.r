
FSRSmodel.3dplot<-function(M,response="SHORTS",yr=2015){

  RLibrary("ggplot2","glmmADMB","rgl")
 
  idata<-subset(M$frame,fYEAR==yr)
  
  if(response=="SHORTS"){
   
    x = with(idata,seq(min(TEMP),max(TEMP),l=100))
    y = with(idata,seq(min(LEGALS),max(LEGALS),1))

    pdata = data.frame(fYEAR=factor(yr, levels(M$frame$fYEAR)),expand.grid(TEMP = x, LEGALS = y))
    pdata$logTRAPS<-0

    Pred <- predict(M, newdata = pdata, se = TRUE)

    pdata$mu <- exp(Pred$fit)
    pdata$ub <- exp(Pred$fit + 1.96 * Pred$se.fit)
    pdata$lb <- exp(Pred$fit - 1.96 * Pred$se.fit)

    mu.2d <- matrix(pdata$mu, nrow = length(x), ncol = length(y))
    
    idata$LEGALS.jitter  <- jitter(idata$LEGALS)
    idata$SHORTS.jitter  <- jitter(idata$SHORTS)
    idata$SPT.jitter  <- idata$SHORTS.jitter/exp(idata$'offset(logTRAPS)')

    plot3d(x = idata$TEMP ,
           y = idata$LEGALS.jitter ,
           z = idata$SPT.jitter,
           type = "p",
           size = 3,
           lit = FALSE,
           xlab = "TEMP",
           ylab = "LEGALS",
           zlab = "SHORTS",
           main = M$LFA
           )

    #Add the surface for the fitted values
    surface3d(x, y, mu.2d, 
            alpha = 0.6, 
            front = "lines", 
            back = "lines", 
            color = "black"
            )
  }


  if(response=="LEGALS"){

    x = with(idata,seq(min(TEMP),max(TEMP),l=100))
    y = with(idata,seq(min(DOS),max(DOS),1))

    pdata = data.frame(fYEAR=factor(yr, levels(M$frame$fYEAR)),expand.grid(TEMP = x, DOS = y))
    pdata$logTRAPS<-0

    Pred <- predict(M, newdata = pdata, se = TRUE)

    pdata$mu <- exp(Pred$fit)
    pdata$ub <- exp(Pred$fit + 1.96 * Pred$se.fit)
    pdata$lb <- exp(Pred$fit - 1.96 * Pred$se.fit)

    mu.2d <- matrix(pdata$mu, nrow = length(x), ncol = length(y))

    idata$LEGALS.jitter  <- jitter(idata$LEGALS)
    idata$LPT.jitter  <- idata$LEGALS.jitter/exp(idata$'offset(logTRAPS)')
    idata$DOS.jitter  <- jitter(idata$DOS)

    plot3d(x = idata$TEMP ,
           y = idata$DOS.jitter ,
           z = idata$LPT.jitter,
           type = "p",
           size = 3,
           lit = FALSE,
           xlab = "TEMP",
           ylab = "DOS",
           zlab = "LEGALS",
           main = M$LFA
           )
  

  #Add the surface for the fitted values
  surface3d(x, y, mu.2d, 
            alpha = 0.6, 
            front = "lines", 
            back = "lines", 
            color = "black"
            )
  }

}

