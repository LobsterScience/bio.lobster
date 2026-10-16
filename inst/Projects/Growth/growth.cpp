#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(StartCL);
  DATA_VECTOR(EndCL);
  DATA_VECTOR(Days);
  
  DATA_MATRIX(X);
  
  DATA_FACTOR(Individual);
  
  PARAMETER(beta0);
  PARAMETER_VECTOR(beta);
  
  PARAMETER(log_sigma_beta);
  PARAMETER(log_sigma_obs);
  PARAMETER(log_sigma_ind);
  
  PARAMETER_VECTOR(u);
  
  Type sigma_beta = exp(log_sigma_beta);
  Type sigma_obs  = exp(log_sigma_obs);
  Type sigma_ind  = exp(log_sigma_ind);
  
  Type nll = 0.0;
  
  // spline coefficients
  for(int i=0; i<beta.size(); i++)
    nll -= dnorm(beta(i), Type(0), sigma_beta, true);
  
  // individual RE
  for(int i=0; i<u.size(); i++)
    nll -= dnorm(u(i), Type(0), sigma_ind, true);
  
  int n = EndCL.size();
  
  vector<Type> pred(n);
  
  for(int i=0; i<n; i++){
    
    Type eta = beta0;
    
    for(int j=0; j<beta.size(); j++)
      eta += X(i,j) * beta(j);
    
    eta += u(Individual(i));
    
    Type growth_rate = exp(eta);
    
     pred(i) =StartCL(i) + Days(i) * growth_rate;
    
    nll -= dnorm(
      EndCL(i),
      pred(i),
      sigma_obs,
      true
    );
  }
  
  ADREPORT(sigma_beta);
  ADREPORT(sigma_obs);
  ADREPORT(sigma_ind);
  REPORT(pred);
  return nll;
}
