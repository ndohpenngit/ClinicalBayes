data {
  int<lower=0> y_t; int<lower=1> n_t;
  int<lower=0> y_c; int<lower=1> n_c;
  int<lower=0> y_h; int<lower=1> n_h;
  real<lower=0> tau_shape;
  real<lower=0> tau_rate;
}
parameters {
  real logit_pc;
  real logit_pc_hist;
  real<lower=0> tau;
  real logit_pt;
}
transformed parameters {
  real pc = inv_logit(logit_pc);
  real pt = inv_logit(logit_pt);
  real ph = inv_logit(logit_pc_hist);
}
model {
  y_c ~ binomial(n_c, pc);
  y_t ~ binomial(n_t, pt);
  y_h ~ binomial(n_h, ph);

  logit_pc ~ normal(0, 2.5);
  logit_pc_hist ~ normal(0, 2.5);
  logit_pt ~ normal(0, 2.5);

  target += normal_lpdf(logit_pc | logit_pc_hist, pow(tau, -0.5));
  tau ~ gamma(tau_shape, tau_rate);
}
generated quantities {
  real diff = inv_logit(logit_pt) - inv_logit(logit_pc);
}
