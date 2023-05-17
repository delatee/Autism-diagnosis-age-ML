data{ 
  int y_sample;
  int n_sample;
  
  real spec_mu;
  real spec_sd;
  real sens_mu;
  real sens_sd;
  real p_mu;
  real p_kappa;
}
parameters{
  real<lower=0, upper=1> p;
  real<lower=0, upper=1> spec;
  real<lower=0, upper=1> sens;
}
transformed parameters{
  real p_sample = p*sens + (1-p)*(1-spec);
}
model{
  p ~ beta_proportion(p_mu, p_kappa);
  spec ~ normal(spec_mu, spec_sd);
  sens ~ normal(sens_mu, sens_sd);
  y_sample ~ binomial(n_sample, p_sample);
}
generated quantities{
  int yppc = binomial_rng(n_sample, p_sample);
}