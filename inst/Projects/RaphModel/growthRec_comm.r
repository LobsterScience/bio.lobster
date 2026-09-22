###annual growth of recruits 71-82 and then commercial 82.5
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
la()

lobster.db('survey')
i = subset(surveyMeasurements, LFA=='L34' & SPECCD_ID==2550,select=c(SET_DATE,FISHSET_ID,SEX,FISH_LENGTH))
i$yr = lubridate::year(i$SET_DATE)
i$SET_DATE <- NULL

r = groundfish.db('gsdet')
inf = groundfish.db('gsinf')
inf = subset(inf,lubridate::month(sdate) %in% c(6,7,8) & strat %in% 480:492)
r = subset(r,spec==2550 & id %in% unique(inf$id))
r$yr = as.numeric(substr(r$id,4,7))
r = subset(r,select=c(id,sex,len,yr))

names(i) = names(r)
i$id = as.character(i$id)
ir=bind_rows(i,r)

ir$len = floor(ir$len)
ir$class = ifelse(ir$len %in% 71:82,'R',ifelse(ir$len >82,'C',NA))
ir = subset(ir,!is.na(class))



matFun = function(a=12.875,b=-0.151,cl){
  1/(1+exp(a+b*cl))
}


#growth matrix
load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')
fg = moltIncrModel(p=p,redo=F,sex=2,return.model = F)
mg = moltIncrModel(p=p,redo=F,sex=1,return.model = F)
ml=mg[[2]]
fl=fg[[2]]
mg = mg[[1]]
fg = fg[[1]]

grow_one <- function(len, sex, m=mg, f=fg, matfun=matFun, mlens=ml,flens=fl){
  if(is.na(sex)) sex=2
  
  if(sex==0) sex=1
  if(sex==2) i <- match(len, flens)
  if(sex==1) i <- match(len, mlens)
  if(sex==3) i <- match(len, flens)
  
  if(is.na(i)) {i = length(mlens); len=max(mlens)}
  
  ## male
  if(sex == 1){
    
    new_len <- sample(
      mlens,
      size = 1,
      prob = m[i,]
    )
    
    return(data.frame(
      len = new_len,
      sex = 1
    ))
  }
  
  ## female
  if(sex == 2){
    
    pmat <- matfun(cl=len)
    
    mature <- rbinom(1, 1, pmat)
    
    if(mature == 1){
      
      return(data.frame(
        len = len,
        sex = 3
      ))
    }
    
    new_len <- sample(
      flens,
      size = 1,
      prob = f[i,]
    )
    
    return(data.frame(
      len = new_len,
      sex = 2
    ))
  }
  
  ## egg-bearing
  if(sex == 3){
    
    new_len <- sample(
      flens,
      size = 1,
      prob = f[i,]
    )
    
    return(data.frame(
      len = new_len,
      sex = 3
    ))
  }
}
ir$nl = NA
ir$ns = NA
ir = subset(ir,len<146)
for(k in 1:nrow(ir)){
  o = grow_one(len=ir$len[k],sex = ir$sex[k])
  ir$nl[k]= o[[1]]
  ir$ns[k] = o[[2]]
  
}

ir$wt1 = lobLW(ir$len,sex=ir$sex)
ir$wt2 = lobLW(ir$nl,sex=ir$sex)

ir$wtinc = (ir$wt2-ir$wt1)/ir$wt1

v = aggregate(wtinc~yr+class,data=ir,FUN=mean)

require(ggplot2)
ggplot(v,aes(yr,wtinc,colour=class))+geom_line()
write.csv(v,file='WtGrowth_rec_comm.csv')