// test nagative multinomial model

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
	
	// data:
	DATA_VECTOR(n_A);
	DATA_VECTOR(n_B);
    
	// parameters:
	PARAMETER(log_alpha);
	PARAMETER(log_beta);
	PARAMETER_VECTOR(log_rho);
	Type alpha = exp(log_alpha);
	Type beta = exp(log_beta);
	vector<Type> rho = exp(log_rho);
	Type mu = alpha/(alpha+beta);
	Type phi = sqrt(alpha*beta/((alpha+beta)*(alpha+beta)*(alpha+beta+1)));
	vector<Type> p = rho/(1+rho);


	// obj fn 
	const int n_i = rho.size();
	Type nll = Type(0.0); // initialize negative log likelihood

	for(int i=0;i<n_i;i++){
		nll -= dbinom(n_B(i), n_A(i) + n_B(i), p(i), TRUE);
	}

	for(int i=0;i<n_i;i++){
		nll -= dbeta(p(i), alpha, beta, TRUE); 
	}


	// report
	ADREPORT(rho);
	ADREPORT(mu);
	ADREPORT(phi);
	REPORT(p);
	REPORT(mu);
	REPORT(phi);
	REPORT(alpha);
	REPORT(beta);
	


	return nll;
}






