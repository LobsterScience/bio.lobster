 
setwd("c:/Rsaves")
load("c:/Rsaves/survey.crab.with.mat.region.etc.to.2006.rdata")

	loadfunctions( c( "spacetime", "utility", "parallel", "lobster") )
	
xmatreg=xmatreg[!is.na(xmatreg$temp),]
xmatreg=xmatreg[!is.na(xmatreg$depth),]
#xmatreg$logz=log(xmatreg$z)

# use line below to remove 4X crab as they are in limited temp range and spatial / time series
xmatreg=xmatreg[xmatreg$region!="cfa4x",]


###-----------------------
### Index males & females

fem=which(xmatreg$sex==2,)
mal=which(xmatreg$sex==1,)
  	#nfem=which(xmatreg$sex==2 & xmatreg$region=="cfanorth")
	#sfem=which(xmatreg$sex==2 & xmatreg$region=="cfasouth")
	
require (doBy) #  for "dose.LD50" .. which estimates the size at 50% maturity
require(epicalc) # for "logistic.display"
require (effects) # for "effect" for adjusted means
require(car) # for "Anova"

# model statement- can add variables as desired
 # .model = as.formula( "mat ~ cw + region + depth + temp" )

# logistic regression results
  #.model.glm = glm( .model, data=xmatreg, subset=mal, family=binomial(link="logit"), na.action ="na.omit" )   

#use following if want to compare 2 possible models with differing interaction terms
 
  .model2 = as.formula( "mat ~ cw + region + depth + temp + cw*temp + cw*region + cw*depth + region*depth + region*temp + temp*depth" )
  .model.glm2 = glm( .model2, data=xmatreg, subset=mal, family=binomial(link="logit"), na.action ="na.omit" )   
  
# extract information
    #summary( .model.glm ) 
    variance=Anova ( .model.glm2 )
  
    
    #Anova (.model.glm2)
    anova(.model.glm2,.model.glm3)  #if comparing two possible models with varying interaction terms
    AIC(.model.glm2,.model.glm3)  #if comparing two possible models with varying interaction terms
    
    all.effects ( .model.glm2 ) # from effects
    size.at.maturity ( .model.glm2 )  # see below
    model.effects  = get.effects.from.model( .model.glm )
    
    #logistic.display( .model.glm2 ) # or whatever the final model may be 

    
    lm.model = .model.glm2
    model.statement = formula( lm.model )
    all.variables = all.vars(model.statement)
    dependent.var = all.variables[1] 
    all.terms = attr( terms( model.statement), "term.labels" )
        
    
                            
    effect.variable="depth*temp"
    res = effect( effect.variable, lm.model )
    plot( res)
     
     
     
     
     
   #The following can be used to plot other interaction terms in opposite order (temperature:depth in this case)
#    a=data.frame
#    a=data.frame(fit=res$fit)
#    a$depth=(res$x$depth)
#    a$temp=(res$x$temp)
#    a$m=exp(a$fit)/(1+exp(a$fit))
#     xyplot(a$m~a$temp | a$depth)
    
    res.summary = summary(res)
    res.effect = res.summary$effect
    results.adjusted = cbind( 
          as.vector(res.summary$effect), 
          as.vector(res.summary$lower), 
          as.vector(res.summary$upper)  ) 
    rownames(results.adjusted) = names(res.summary$effect)
    colnames(results.adjusted) =c("Adjusted mean effect", "Lower95% CI", "Upper95% CI")
results.adjusted
    
    # Following used for plotting GLM Separately by Region
    
   
  nmal=which(xmatreg$sex==1 & xmatreg$region=="cfanorth")
	smal=which(xmatreg$sex==1 & xmatreg$region=="cfasouth")
  
   
.model = as.formula( "mat ~ cw" )
  .model.glmn = glm( .model, data=xmatreg, subset=nmal, family=binomial(link="logit"), na.action ="na.omit" )   
  .model.glms = glm( .model, data=xmatreg, subset=smal, family=binomial(link="logit"), na.action ="na.omit" )   
  
      
	cw50m = dose.LD50 (.model.glms, lambda=c(1,NA))
	lowm=round(cw50m [2],2)
	upm=round(cw50m [3],2)

    
   plot(.model.glms$model$cw, .model.glms$fitted.values, main="Male Size at Maturity in S-ENS", abline(v=cw50m [1], h=0.5), xlab= "Carapace Width (mm)", ylab= "Probability")
   text ( x=125, y = 0.4, paste("Size at 50% maturity =", round(cw50m [1],2) ), )
   text ( x=125, y = 0.35, paste("(",lowm,",",upm,")"))

    
      # Following used for plotting Size at 50% Maturity by Region
    
       
.model = as.formula( "mat ~ cw + region" )
  .model.glm = glm( .model, data=xmatreg, subset=mal, family=binomial(link="logit"), na.action ="na.omit" )   
    
  
      
	
    
   plot(.model.glm$model$cw, .model.glm$fitted.values, main="Male Size at Maturity", abline(v=82.86, h=0.5), xlab= "Carapace Width (mm)", ylab= "Probability")
  

    
    # import into excel or word ...
  export.to.clipboard(variance )
 

    
    
    
 


