data {
  real ybar_t; real<lower=0> s_t; int<lower=1> n_t;
  real ybar_c; real<lower=0> s_c; int<lower=1> n_c;
  real ybar_h; real<lower=0> s_h; int<lower=1> n_h;
  real<lower=0> tau_shape;
  real<lower=0> tau_rate;
}
parameters {
  real mu_t;
  real mu_c;
  real mu_h;
  real<lower=0> tau;
}
transformed parameters {
  real diff = mu_t - mu_c;
}
model {
  // Commensurate Prior Link
  tau ~ gamma(tau_shape, tau_rate);
  mu_h ~ normal(ybar_h, s_h / sqrt(n_h)); // Historical estimate
  mu_c ~ normal(mu_h, 1.0 / sqrt(tau));   // The link

  // Current Data Likelihoods
  mu_t ~ normal(0, 100); // Weak prior on treatment
  ybar_t ~ normal(mu_t, s_t / sqrt(n_t));
  ybar_c ~ normal(mu_c, s_c / sqrt(n_c));
}
