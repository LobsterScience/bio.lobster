
#make a population, either with or without fishing if m is a vector of length 2
#first is m, second is fishing mortality, tbreak introduces when fishing mortality would come in

require(ggplot2)
pop = function(n0,m,t,tbreak){
  nt=rep(NA,t+1)
  nt[1]=n0
  if(length(m)==1){
    v=2:(t+1)
    for(i in v){
      nt[i] = n0*(1-m)^(i-1)
    }
  }
  if(length(m)==2){
    #change in M
    v=2:(t+1)
    for(i in v){
      if(i<tbreak) M=m[1]
      if(i>=tbreak) M=m[2]
      nt[i] = n0*(1-M)^(i-1)
    }
  }
  return(nt)    
}

size = pop(100000000,m=.15,t=25)

len = seq(10,260,by=10)
ou = data.frame(len=len,size=size)
ggplot(data=ou,aes(x=len,y=size))+geom_bar(stat = 'identity')+xlab('Length')+ylab('Number of Lobster')

size2 = pop(100000000,m=c(.15,.3),t=25,tbreak = 8)
ou = cbind(ou,size2)
ggplot(data=ou,aes(x=len,y=size2))+geom_bar(stat = 'identity')+xlab('Length')+ylab('Number of Lobster')

#apply a selectivity sigmoidal function
sel = function(x,k,x0){
  #x0 is the midpoint or 50%; k is the rate of change
  1/(1+exp(-k*(x-x0)))
}

ou$sel = sel(x=1:26,k=1,x0=7)

ou$popSel = ou$size*ou$sel
ou$fishSel = ou$size2*ou$sel

#if no fishery removals that is what we would see
ggplot(ou,aes(x=len,y=popSel))+geom_bar(stat='identity')+xlab('Length')+ylab('Number of Lobster')

#since there is a fishery this is what we would see
ggplot(ou,aes(x=len,y=fishSel))+geom_bar(stat='identity')+xlab('Length')+ylab('Number of Lobster')
