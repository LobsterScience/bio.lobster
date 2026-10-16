#include <TMB.hpp>

template<class Type>
  Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(StartCL);
  DATA_VECTOR(EndCL);
  DATA_VECTOR(Temperature);
  DATA_VECTOR(Days);
  
  DATA_IVECTOR(Individual);
  
  PARAMETER(log_kref);
  
  PARAMETER(log_E);
  PARAMETER(log_Eh);
  
  PARAMETER(log_Th);
  
  PARAMETER(log_Linf);
  
  PARAMETER(log_sigma_obs);
  PARAMETER(log_sigma_ind);
  
  PARAMETER_VECTOR(u);
  
  Type kref = exp(log_kref);
  
  Type E  = exp(log_E);
  Type Eh = exp(log_Eh);
  
  Type Th = exp(log_Th);
  
  Type Linf = exp(log_Linf);
  
  Type sigma_obs = exp(log_sigma_obs);
  Type sigma_ind = exp(log_sigma_ind);
  
  Type nll = 0.0;
  
  const Type kb = Type(8.617e-5);
  
  const Type Tref = Type(293.15); // 20 C
  
  int n = EndCL.size();
  
  vector<Type> pred(n);
  vector<Type> kT(n);
  
  for(int j=0;j<u.size();j++)
    nll -= dnorm(u(j), Type(0), sigma_ind, true);
  
  for(int i=0;i<n;i++)
  {
    Type TK = Temperature(i) + Type(273.15);
    
    Type num =
      exp(
        (E / kb) *
          (Type(1.0)/Tref - Type(1.0)/TK)
      );
    
    Type denom =
      Type(1.0) +
      exp(
        (Eh / kb) *
          (Type(1.0)/Th - Type(1.0)/TK)
      );
    
    kT(i) =
      kref *
      num /
      denom;
    
    kT(i) *= exp(u(Individual(i)));
    
    pred(i) =
      Linf -
      (Linf - StartCL(i)) *
      exp(
        -kT(i) * Days(i)
      );
    
    nll -= dnorm(
      EndCL(i),
      pred(i),
      sigma_obs,
      true
    );
  }
  
  REPORT(pred);
  REPORT(kT);
  
  ADREPORT(kref);
  ADREPORT(E);
  ADREPORT(Eh);
  ADREPORT(Th);
  
  ADREPORT(kT);
  ADREPORT(Linf);
  
  ADREPORT(sigma_obs);
  ADREPORT(sigma_ind);
  
  return nll;
  }
