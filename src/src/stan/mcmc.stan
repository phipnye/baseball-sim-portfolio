data {
  int<lower=1> N_obs;            // Number of rows (matchups)
  int<lower=1> K;                // Number of categories (events)
  array[N_obs, K] int<lower=0> counts_obs; // Observed event counts
}

parameters {
  real<lower=0> shape;           // Shape parameter for Gamma
  real<lower=0> rate;            // Rate parameter for Gamma
  simplex[K] q;                  // Probability distribution over K categories
}

model {
  // Priors
  shape ~ gamma(2, 0.5);
  rate  ~ gamma(2, 2);
  q     ~ dirichlet(rep_vector(1, K));

  for (n in 1:N_obs) {
    // 1) Compute row sum s_n
    int s_n = 0;
    for (k in 1:K) {
      s_n += counts_obs[n, k];
    }

    // 2) Gamma-Poisson Sampling
    real log_gamma_s = gamma_lpdf(s_n | shape, rate);
    target += log_gamma_s;

    // 3) Given s_n, sample event counts from a Multinomial
    counts_obs[n] ~ multinomial(q);
  }
}
