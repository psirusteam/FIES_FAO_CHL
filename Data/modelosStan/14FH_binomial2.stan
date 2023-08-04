data {
  int<lower=1> N1;                      // sample size
  int<lower=1> N2;                      // sample size
  int<lower=1> p;                       // p predictors
  int<lower=0> y_effect[N1];        // response 
  matrix[N1,p] X;
  matrix[N2,p] Xs;
  int<lower=1> n_effec[N1];              // dispersion parameter
 }

parameters {
  vector[p] beta;       // coefficients for predictors
  real<lower=0> sigma2_u; 
  vector[N1] u;

}

transformed parameters {
   vector[N1] LP;
   vector[N1] theta;
   real<lower=0> sigma_u;
  
   sigma_u = sqrt(sigma2_u); 
   LP =  X * beta + u;
   theta = inv_logit(LP);
}
model {
  to_vector(beta) ~ normal(0, 10000);
   u ~ normal(0, sigma_u);
    sigma2_u ~ inv_gamma(0.0001, 0.0001);
  for(ii in 1:N1){
  y_effect[ii] ~ binomial(n_effec[ii], theta[ii]);  // likelihood
}
  }
generated quantities {
  real ypred[N1 + N2];                    // vector de longitud n
  vector[N1 + N2] thetaLP;                   // vector de longitud n
  vector[N1 + N2] LP_pred;                    // vector de longitud n
  
  LP_pred[1:N1] =  X * beta + u;
  LP_pred[(N1 + 1):(N1 + N2)] =  Xs * beta;
  thetaLP = inv_logit(LP_pred);    

}
