//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> N;
  vector[N] x_launch;
  vector[N] x_batting_avg;
  vector[N] x_barrel;
  vector[N] x_exit_velo;
  vector[N] y;
}

parameters {
  real beta_launch;
  real beta_avg;
  real beta_barrel;
  real beta_exit_velo;
  real intercept;
  real<lower=0> sigma;
}

model {
  // Informative priors
  beta_launch ~ normal(0.02, 0.0005);       // More informative
  beta_avg ~ normal(1.5, 0.5);              
  beta_barrel ~ normal(0.01, 0.005);        
  beta_exit_velo ~ normal(0.005, 0.005);    
  intercept ~ normal(0.5, 1);
  sigma ~ exponential(1);

  // Likelihood
  y ~ normal(intercept + beta_launch * x_launch +
             beta_avg * x_batting_avg +
             beta_barrel * x_barrel +
             beta_exit_velo * x_exit_velo, sigma);
}

generated quantities {
  vector[N] y_rep;

  for (n in 1:N) {
    y_rep[n] = normal_rng(
      intercept +
      beta_launch * x_launch[n] +
      beta_avg * x_batting_avg[n] +
      beta_barrel * x_barrel[n] +
      beta_exit_velo * x_exit_velo[n],
      sigma
    );
  }
}
