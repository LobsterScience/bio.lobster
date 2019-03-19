#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() (){
  using namespace density;
  using namespace R_inla;

  DATA_VECTOR(Y); //counts
  DATA_IVECTOR(v_i);
  DATA_STRUCT(spde, spde_t);

  PARAMETER(beta0);
  PARAMETER(ln_kappa);
  PARAMETER(ln_tau);

  PARAMETER_VECTOR(omega);

  Type kappa = exp(ln_kappa);
  Type tau = exp(ln_tau);
  int N = Y.size();

  SparseMatrix<Type> Q = Q_spde(spde, kappa);
  Type nll = SCALE(GMRF(Q), 1/tau)(omega);

  //Poisson likelihood
  vector<Type>ln_mean(N);
  for(int i=0; i<N; i++){
    ln_mean(i) = beta0 + omega(v_i(i));
    nll -= dpois(Y(i), exp(ln_mean(i)), true);
  }
  return nll;
}

