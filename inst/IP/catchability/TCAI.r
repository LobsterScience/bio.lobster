rm(list=ls())
graphics.off()
setwd('/media/adam/My Book/Alumni/JTremblay/R_Files/TCAI Code/')
getwd()

rm(list=ls())

# Data MUST have the fields 'Catch', 'Temperature',  'Location',  'Season'
# Data MUST have the fields 'Catch', 'Temperature',  'Location',  'Season'
# Data MUST have the fields 'Catch', 'Temperature',  'Location',  'Season'
# Data MUST have the fields 'Catch', 'Temperature',  'Location',  'Season'

load('GoodData.RData')

if ( 'factor' %in% class(Data$Season) ) { Data$Season <- as.character(Data$Season) } else {
  if (mode(Data$Season) == 'numeric') {
      Temp <- as.character(Data$Season)
      if ( min(nchar(Temp)) == max(nchar(Temp)) ) { Data$Season <- as.character(Data$Season)
    } else {
      Max <- ceiling(log(max(Data$Season),10))
      Max. <- max(nchar(as.character(Data$Season)))
      Temp <- 10^(Max+1) + Data$Season
      Temp <- as.character(Temp)
      Data$Season <- substr(Temp,3+Max-Max.,2+Max)
    }
  }
}


Data$Season <- as.character(Data$Season)
Seasons <- sort(unique(Data$Season))
Seasons.n <- length(Seasons)
IndexBaseSeasons <- Seasons[1]
Data$Location <- as.character(Data$Location)
Locations <- sort(unique(Data[,'Location']))
Locations.n <- length(Locations)

ProbaAtTemp <- function(Temperatures=seq(-5,20,by=.1),T0=2,Slope=.005,Diff=F,k=100,Figure=F) {
  if (F) {
    Slope <- 1;
    Temperatures <- -5:20;
    T0 <- 2;
    Proba <- ProbaAtTemp(Temperatures,T0,Slope)
    graphics.off()
    plot(Temperatures,Proba,type='l');
  }
  # Slope <- Slope.x.1000/1000
  A <- 1/Slope;
  x <- Temperatures-T0-A/2
  Proba <-  sign(x) * (0.5 - 1/(1 - 0.25 * A * k + 0.5 * k * abs(x) + 0.25 * (16 + 8 * A * k +
    16 * k * abs(x) + A^2 * k^2 - 4 * A * k^2 * abs(x) + 4 * k^2 * abs(x)^2)^(1/2))) ;
  Proba <- Proba + 0.5
  if (Figure) plot(Temperatures,Proba,type='l')
  return(Proba)
}

ProbaAtTemp.Diff <- function(Temperatures=seq(-5,20,by=.1),T0=2,Slope=.005,Diff=F,k=100,Figure=F) {
  if (F) {
      ProbaAtTemp(Figure=T)
      Slope <- .01;
      Temperatures <- -5:20;
      T0 <- 2;
      k <- 100
      Proba <- ProbaAtTemp(Temperatures,T0,Slope)
      graphics.off()
      plot(Temperatures,Proba,type='l');
      Junk <- ProbaAtTemp.Diff(Temperatures,T0,Slope)
      Junk[1:5,]
      Delta <- .0001
      (ProbaAtTemp(Temperatures,T0,Slope)-ProbaAtTemp(Temperatures,T0+Delta,Slope))/Delta
      Delta <- .00001
      (ProbaAtTemp(Temperatures,T0,Slope)-ProbaAtTemp(Temperatures,T0,Slope+Delta))/Delta
  } 
  A <- 1/Slope;
  x <- Temperatures-T0-A/2
  Diff_rel_x=8 * k  /
    ( (16 + 8 * A*k+16 * k*abs(x)+A^2*k^2-4 * A*k^2*abs(x)+4 * k^2*abs(x)^2)^(1/2) *
      (4.-1 * A*k+2 * k*abs(x)+(16 + 8 * A*k+16 * k*abs(x)+A^2*k^2-4 * A*k^2*abs(x)+4 * k^2*abs(x)^2)^(1/2)) );

  Diff_Pre_rel_A=-(4 * ((16 + 8 * A*k+16 * k*abs(x)+A^2*k^2-4 * A*k^2*abs(x)+4 * k^2*abs(x)^2)^(1/2)-4.-1 * A*k+2 * k*abs(x))*k) /
      ( (4.-1 * A*k+2 * k*abs(x)+(16 + 8 * A*k+16 * k*abs(x)+A^2*k^2-4 * A*k^2*abs(x)+4*k^2*abs(x)^2)^(1/2))^2
       * (16 + 8 * A*k+16 * k*abs(x)+A^2*k^2-4 * A*k^2*abs(x)+4 * k^2*abs(x)^2)^(1/2) ) ;
  Diff_Pre_rel_A=sign(x) * Diff_Pre_rel_A;
  Diff_rel_T0=-Diff_rel_x;
  Diff_rel_A=-0.5*Diff_rel_x+Diff_Pre_rel_A;
  Diff_rel_Slope=-Diff_rel_A/(Slope^2);
  return(cbind(Diff_rel_T0,Diff_rel_Slope))
}

Catch.EV <- function(Parameters,Data) {
  # Data MUST have the fields 'Temperature',  'Location',  'Season'
  # Junk <- Catch.EV(Parameters,Data); Junk
  P <- ProbaAtTemp(Data[,'Temperature'],Parameters['T0'],Parameters['Slope'])
  N <- Parameters[Data$Location]
  I <- Parameters[Data$Season]
  C <- I*N*P
  return(C)
}

Catch.EV.Diff <- function(Parameters,Data) {
  # Data MUST have the fields 'Catch', 'Temperature',  'Location',  'Season'
  # Junk <- Catch.EV(Parameters,Data); Junk
  if (F) {
    Gradient <- Catch.EV.Diff(Parameters,Data)
    Delta <- .0001
    ParName <- 'T0'
    DiffTest <- (Catch.EV(Parameters+(names(Parameters)==ParName)*Delta,Data) - Catch.EV(Parameters,Data))/Delta
    cbind(Gradient[1:5,ParName],DiffTest[1:5])
      ParName <- 'Slope'
    DiffTest <- (Catch.EV(Parameters+(names(Parameters)==ParName)*Delta,Data) - Catch.EV(Parameters,Data))/Delta
    cbind(Gradient[1:5,ParName],DiffTest[1:5])
      ParName <- Data$Season[1]
    DiffTest <- (Catch.EV(Parameters+(names(Parameters)==ParName)*Delta,Data) - Catch.EV(Parameters,Data))/Delta
    cbind(Gradient[1:5,ParName],DiffTest[1:5])
        ParName <- Data$Location[1]
    DiffTest <- (Catch.EV(Parameters+(names(Parameters)==ParName)*Delta,Data) - Catch.EV(Parameters,Data))/Delta
    cbind(Gradient[1:5,ParName],DiffTest[1:5])
  }
  P <- ProbaAtTemp(Data[,'Temperature'],Parameters['T0'],Parameters['Slope'])
  N <- Parameters[Data$Location]
  I <- Parameters[Data$Season]
  C <- I*N*P
  P.Diff <- ProbaAtTemp.Diff(Data[,'Temperature'],Parameters['T0'],Parameters['Slope'])
  Diff.T0 <- I*N*P.Diff[,1]
  Diff.Slope <- I*N*P.Diff[,2]
  Gradient <- cbind(Diff.T0,Diff.Slope)
  Diff.I <- N*P
  for (ii in Seasons) {
    Gradient <- cbind(Gradient,Diff.I*(Data[,'Season']==ii))
  }
  Diff.N <- I*P
  for (ii in Locations) {
    Gradient <- cbind(Gradient,Diff.N*(Data[,'Location']==ii))
  }
  colnames(Gradient) <- c('T0','Slope',Seasons,Locations)
  return(Gradient)
}

NegLogLikelihood.Poisson <- function(Parameters,Data) {
  # Data MUST have the fields 'Temperature',  'Location',  'Season', 'Catch'
  # NegLogLikelihood.Poisson(Parameters,Data)
  C <- Catch.EV(Parameters,Data)
  # Keep <- which(is.na(Catch.EV));  Data[Keep,]
  p <- dpois(Data[,'Catch'],C,log=T)
  NLL <- -sum(p)
  return(NLL)
}

NegLogLikelihood.Poisson.Diff <- function(Parameters,Data) {
  # Data MUST have the fields 'Temperature',  'Location',  'Season', 'Catch'
  # NegLogLikelihood.Poisson(Parameters,Data)
  if (F) {
    NLL.Diff <- NegLogLikelihood.Poisson.Diff(Parameters,Data)
    Delta <- 0.000000001
    ParName <- 'T0'
    DiffTest <- (NegLogLikelihood.Poisson(Parameters+(names(Parameters)==ParName)*Delta,Data) - NegLogLikelihood.Poisson(Parameters,Data))/Delta
    cbind(NLL.Diff[ParName],DiffTest[1])
    ParName <- 'Slope'
    DiffTest <- (NegLogLikelihood.Poisson(Parameters+(names(Parameters)==ParName)*Delta,Data) - NegLogLikelihood.Poisson(Parameters,Data))/Delta
    cbind(NLL.Diff[ParName],DiffTest[1])
    ParName <- Data$Season[1]
    DiffTest <- (NegLogLikelihood.Poisson(Parameters+(names(Parameters)==ParName)*Delta,Data) - NegLogLikelihood.Poisson(Parameters,Data))/Delta
    cbind(NLL.Diff[ParName],DiffTest[1])
    ParName <- Data$Location[1]
    DiffTest <- (NegLogLikelihood.Poisson(Parameters+(names(Parameters)==ParName)*Delta,Data) - NegLogLikelihood.Poisson(Parameters,Data))/Delta
    cbind(NLL.Diff[ParName],DiffTest[1])
  }
  C <- Catch.EV(Parameters,Data)
  NLL.Diff <- (Data[,'Catch']/C - 1)*Catch.EV.Diff(Parameters,Data)
  NLL.Diff <- -apply(NLL.Diff,2,sum)
  names(NLL.Diff) <- names(Parameters)
  return(NLL.Diff)
}

ParToParOpt <- function(Parameters) {
  # ParToParOpt(Parameters)
  Parameters_SqRt <- Parameters[(names(Parameters)!=IndexBaseSeasons)]
  IsNumberAtLocationParameter <- which(names(Parameters_) %in% Locations)
  Parameters_SqRt[Locations] <- Parameters_SqRt[Locations]^.5
  return(Parameters_SqRt)
}

ParOptToPar <- function(Parameters_SqRt.New) {
  # ParOptToPar(Parameters_SqRt)
Parameters.New <- Parameters_SqRt.New
Parameters.New[Locations] <- Parameters.New[Locations]^2
IndexBase <- 1
names(IndexBase) <- IndexBaseSeasons
Parameters.New <- c(Parameters.New,IndexBase)
Parameters.New <- Parameters.New[c('T0','Slope',Seasons,Locations)]
T0 <- Parameters.New['T0']
T0 <- Parameters.New['Slope']
Index <- Parameters.New[Seasons]
NumberAtLocation <- Parameters.New[Locations]
return(list(Parameters=Parameters.New,T0=T0,Slope=Slope,Index=Index,NumberAtLocation=NumberAtLocation))
}

LLtoMaxLik <- function(Parameters_SqRt,IndexBase,Data,IsNumberAtLocationParameter) {
  # LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter)
  Parameters_ <- Parameters_SqRt
  Parameters_[Locations] <- Parameters_SqRt[Locations]^2
  R <- NegLogLikelihood.Poisson(c(Parameters_,IndexBase),Data=Data)
  #print(Parameters_SqRt)
  #print(names(Data))
  #print(R)
  #save(Parameters_SqRt,file='Temp.RData')
  return(R)
}

LLtoMaxLik.Diff <- function(Parameters_SqRt,IndexBase,Data,IsNumberAtLocationParameter) {
  if (F) {
   Delta <- 0.000001
    ParName <- 'T0'
    DiffTest <- (LLtoMaxLik(Parameters_SqRt+(names(Parameters_SqRt)==ParName)*Delta,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter) -
            LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter))/Delta
    cbind(D1[ParName],DiffTest[1])
      D1[ParName]/DiffTest[1]
      ParName <- 'Slope'
    DiffTest <- (LLtoMaxLik(Parameters_SqRt+(names(Parameters_SqRt)==ParName)*Delta,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter) -
            LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter))/Delta
    cbind(D1[ParName],DiffTest[1])
      ParName <- Data$Season[1]
    DiffTest <- (LLtoMaxLik(Parameters_SqRt+(names(Parameters_SqRt)==ParName)*Delta,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter) -
            LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter))/Delta
    cbind(D1[ParName],DiffTest[1])
        ParName <- Data$Location[1]
    DiffTest <- (LLtoMaxLik(Parameters_SqRt+(names(Parameters_SqRt)==ParName)*Delta,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter) -
            LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter))/Delta
    cbind(D1[ParName],DiffTest[1])
    D1[ParName]/DiffTest[1]
    Parameters[ParName]^.5
  LLtoMaxLik.Diff(Parameters_SqRt,IndexBase,Data,IsNumberAtLocationParameter)
  }
  # LLtoMaxLik(Parameters_SqRt,IndexBase,Data=Data,IsNumberAtLocationParameter=IsNumberAtLocationParameter)
  #  rm(D1)
  Parameters_ <- Parameters_SqRt
  Parameters_[Locations] <- Parameters_SqRt[Locations]^2
  ParTemp <- c(Parameters_,IndexBase)
  ParTemp <- ParTemp[c('T0','Slope',Seasons,Locations)]
  D1 <- NegLogLikelihood.Poisson.Diff(Parameters=ParTemp,Data=Data)
  D1[Locations] <- D1[Locations] * 2 * Parameters_[Locations]^.5
  D1 <- D1[names(D1) != names(IndexBase) ]
  return(D1)
}

## Compute parameters for initial values
Index <- tapply(Data$Catch,Data$Season,mean,na.rm=T)
Index <- Index/Index[1]
T0 <- 0; names(T0) <- 'T0'
Slope <- lm(Data$Catch ~ Data$Temperature)$coefficient[2]/75; names(Slope) <- 'Slope'
NumberAtLocation <- tapply(Data$Catch,Data$Location,mean,na.rm=T)*50
Parameters <- c( T0 , Slope, Index  , NumberAtLocation )

IndexBase <- Parameters[IndexBaseSeasons]


Parameters_SqRt <- Parameters[(names(Parameters)!=IndexBaseSeasons)]
# IsNumberAtLocationParameter <- which(names(Parameters_SqRt) %in% Locations)
Parameters_SqRt[Locations] <- Parameters_SqRt[Locations]^.5

Res.Optim.A <- optim(fn=LLtoMaxLik,gr=LLtoMaxLik.Diff,par=Parameters_SqRt,IndexBase=IndexBase,Data=Data,hessian=F,control = list(maxit=100000,REPORT=100,trace=0))
print('Computing Hessian')
Res.Optim.B <- optim(fn=LLtoMaxLik,gr=LLtoMaxLik.Diff,par=Res.Optim.A$par,IndexBase=IndexBase,Data=Data,hessian=T,control = list(maxit=100000,REPORT=100,trace=0))

Res.Optim <- Res.Optim.B
Estimates <- ParOptToPar(Res.Optim$par)
Estimates


  Hessian <- Res.Optim$hessian
  VarCov <- solve(Hessian)
  rownames(VarCov) <- names(Res.Optim$par)
  colnames(VarCov) <- names(Res.Optim$par)

  SEs <- c(sqrt(diag(VarCov)))
  SEs <- c(0,SEs)
  names(SEs)[1] <- IndexBaseSeasons

  SEs[Seasons]

  round(cbind(Estimates$Index,SEs[Seasons]),2)
 
 #using estimates from above
 #ProbaAtTemp(T0=0.002437103,Slope=0.003322298,Figure=T)

###