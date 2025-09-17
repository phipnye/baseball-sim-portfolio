// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include <random>

using namespace Rcpp;
using namespace RcppParallel;

// ----------------------------------------------------------------------------
// Helper: Normalize Each Row of a NumericMatrix In-Place
// ----------------------------------------------------------------------------
void normalize_q_matrix(NumericMatrix &q_matrix) {
  int N = q_matrix.nrow();
  int K = q_matrix.ncol();

  for (int i = 0; i < N; ++i) {
    double row_sum = 0.0;
    for (int j = 0; j < K; ++j) {
      row_sum += q_matrix(i, j);
    }

    if (row_sum > 0) {  // Avoid division by zero
      for (int j = 0; j < K; ++j) {
        q_matrix(i, j) /= row_sum;
      }
    }
  }
}

// ----------------------------------------------------------------------------
// Thread-safe multinomial sampling using std::discrete_distribution
// ----------------------------------------------------------------------------
void sample_multinomial(const int total_count, const std::vector<double> &probabilities,
                        std::vector<int> &counts, std::mt19937 &rng) {
  std::discrete_distribution<int> multinom_dist(probabilities.begin(), probabilities.end());
  std::memset(counts.data(), 0, counts.size() * sizeof(int));  // Zero counts

  for (int i = 0; i < total_count; ++i) {
    int sampled_index = multinom_dist(rng);
    counts[sampled_index]++;
  }
}

// ----------------------------------------------------------------------------
// Parallel Worker for Alpha Prior Generation
// ----------------------------------------------------------------------------
struct AlphaPriorWorker : public Worker {
  const RMatrix<double> q_norm;  // Normalized probability matrix
  const RVector<double> shape;   // Negative binomial shape (r)
  const RVector<double> rate;    // Negative binomial probability (p)
  int N_obs, K;
  RVector<int> result;  // Output vector

  AlphaPriorWorker(const NumericMatrix &q_norm, const NumericVector &shape, const NumericVector &rate,
                   const int N_obs, IntegerVector &result)
      : q_norm(q_norm), shape(shape), rate(rate), N_obs(N_obs), K(q_norm.ncol()), result(result) {}

  void operator()(std::size_t start, std::size_t end) {
    std::random_device rd;
    std::mt19937 rng(rd());  // Thread-local RNG

    std::vector<int> counts(K);      // Thread-local storage for multinomial counts
    std::vector<double> q_probs(K);  // Thread-local storage for probabilities

    for (std::size_t s = start; s < end; ++s) {
      // Fetch and copy the row probabilities
      for (int j = 0; j < K; ++j) {
        q_probs[j] = q_norm(s, j);
      }

      std::gamma_distribution<double> gamma_dist(shape[s], 1.0 / rate[s]);

      for (int n = 0; n < N_obs; ++n) {
        int val = 0, cnt = 0;

        // Sample lambda from Gamma(α, β)
        double lambda = gamma_dist(rng);

        // Sample from Poisson(λ)
        std::poisson_distribution<int> pois_dist(lambda);

        // Ensure at least 1 occurrence
        while (val == 0 && cnt++ < 250) {
          val = pois_dist(rng);
        }

        val = (val == 0) ? 1 : val;

        // Use Custom Multinomial Sampler
        sample_multinomial(val, q_probs, counts, rng);

        // Store results in the flat vector
        int offset = (s * N_obs + n) * K;

        std::memcpy(&result[offset], counts.data(), K * sizeof(int));
      }
    }
  }
};

// ----------------------------------------------------------------------------
// Main Function: Generate Alpha Priors
// ----------------------------------------------------------------------------
// [[Rcpp::export]]
IntegerVector generate_alpha_priors(const NumericMatrix &q_matrix, const NumericVector &shape,
                                    const NumericVector &rate, const int N_obs) {
  int N_batch = shape.size();
  int K = q_matrix.ncol();

  // Preallocate result as IntegerVector
  IntegerVector result(N_batch * N_obs * K);

  // Clone and normalize q_matrix
  NumericMatrix q_norm = clone(q_matrix);
  normalize_q_matrix(q_norm);

  // Run parallel execution
  AlphaPriorWorker worker(q_norm, shape, rate, N_obs, result);
  parallelFor(0, N_batch, worker);

  return result;  // Output remains as an IntegerVector
}
