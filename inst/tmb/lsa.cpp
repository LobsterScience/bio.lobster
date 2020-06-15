#include <TMB.hpp>

template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

bool isNAINT(int x){
  return NA_INTEGER==x;
}

template <class Type>
Type itrans(Type x){ // scaled ilogit
  return Type(2)/(Type(1) + exp(-Type(2) * x)) - Type(1);
}

template<class Type>
vector<Type> ssbFUN(matrix<Type> logN, matrix<Type> logFF, matrix<Type> M, matrix<Type> SW, matrix<Type> MO, matrix<Type> PF, matrix<Type> PM){
  int nrow = logN.rows();
  int ncol = logN.cols();
  vector<Type> ret(nrow);
  ret.setZero();
  for(int y=0; y<nrow; ++y){
    for(int a=0; a<ncol; ++a){
      ret(y) += SW(y,a)*MO(y,a)*exp(logN(y,a))*exp(-PF(y,a)*exp(logFF(y,a))-PM(y,a)*M(y,a));
    }
  }
  return ret;
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_IVECTOR(year)
  DATA_IVECTOR(survey)
  DATA_IVECTOR(len)
  DATA_VECTOR(obs)
  DATA_ARRAY(stockMeanWeight)
  DATA_ARRAY(M)              
  DATA_ARRAY(propMature)     
  DATA_SCALAR(surveyTime)     

  PARAMETER_VECTOR(logN1Y);
  PARAMETER_VECTOR(logN1L);
  PARAMETER_VECTOR(logFY);
  PARAMETER_VECTOR(logFL);
  PARAMETER(logVarLogCatch);
  PARAMETER_VECTOR(logQ);
  PARAMETER(logVarLogSurvey);  

  int minLen=len.minCoeff();
  int maxLen=len.maxCoeff();
  int nl=maxLen-minLen+1;
  int minYear=year.minCoeff();
  int maxYear=year.maxCoeff();
  int ny=maxYear-minYear+1;

  Type ans=0;
  
  // setup F
  matrix<Type> F(ny,nl);
  for(int l=0; l<nl; ++l){
    for(int y=0; y<ny; ++y){
      F(y,l)=exp(logFY(y))*exp(logFL(l));
    }
  }
  
  // setup logN
  matrix<Type> logN(ny,nl);
  for(int l=0; l<nl; ++l){
    logN(0,l)=logN1Y(l);
  } 
  for(int y=1; y<ny; ++y){
    logN(y,0)=logN1L(y-1);
    for(int l=1; l<nl; ++l){
      logN(y,l)=logN(y-1,l)-F(y-1,l)-M(y-1,l);
      

      //if(l==(nl-1)){ // plus group 
      //  logN(y,l)=log(exp(logN(y,l))+exp(logN(y,l-1)-F(y,l-1)-M(y,l-1)));
      //}
    }
    predN = logN(y) * GroMat
  } 

  // Match to observations
  vector<Type> logObs=log(obs);
  Type pred, sd;
  int l, y;
  for(int i=0; i<logObs.size(); ++i){
    l = len(i)-minLen;
    y = year(i)-minYear;

    if(fleet(i)==1){
      pred = log(F(y,l))-log(F(y,l)+M(y,l))+log(Type(1.0)-exp(-F(y,l)-M(y,l)))+logN(y,l);
      sd = exp(Type(0.5)*logVarLogCatch);
    }else{
      pred = logQ(l)-(F(y,l)+M(y,l))*surveyTime+logN(y,l);
      sd = exp(Type(0.5)*logVarLogSurvey);
    }    
    ans += -dnorm(logObs(i),pred,sd,true);
  }

  vector<Type> ssb(ny);
  ssb.setZero();
  for(int y=0; y<ny; ++y){
    for(int l=0; l<nl; ++l){
      ssb(y)+=exp(logN(y,l))*stockMeanWeight(y,l)*propMature(y,l);
    }
  }

  ADREPORT(ssb);
  return ans;
}
