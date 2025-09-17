functions {
  int zero_truncated_neg_binomial_rng(real r, real p) {
    int s_n = 0, attempts = 0;
    
    while (s_n == 0 && attempts < 250) {
      s_n = neg_binomial_rng(r, p);
      attempts += 1;
    }
    
    s_n = s_n == 0 ? 1 : s_n; // default to 1 if no non-zeros are drawn
    return s_n;
  }
}

data {
  int<lower=1> N_obs;      // Number of rows (matchups)
  int<lower=1> K;          // Number of categories (events)
  int<lower=1> N_batch;    // Number of posterior samples in this batch
  matrix[N_batch, K] q;    // Probability distributions for batch of posterior samples
  vector[N_batch] nb_r;    // Negative binomial size parameters for the batch
  vector[N_batch] nb_p;    // Negative binomial probability parameters for the batch
}

generated quantities {
  array[N_batch, N_obs, K] int alpha;  // Dirichlet prior alphas (counts only)

  for (s in 1:N_batch) {
    // Normalize q once per batch
    vector[K] q_vec = to_vector(q[s]);
    vector[K] q_normalized = q_vec / sum(q_vec);

    // Generate row sums and sample directly
    for (n in 1:N_obs) {
      int s_n = zero_truncated_neg_binomial_rng(nb_r[s], nb_p[s]);
      alpha[s, n] = multinomial_rng(q_normalized, s_n);
    }
  }
}
