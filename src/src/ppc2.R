# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(cmdstanr)
library(parallel)
library(stringr)
library(data.table)
library(MCMCprecision)
set.seed(398L)
Rcpp::sourceCpp(file.path("src", "cpp", "alpha_gen.cpp"))

# Import data -------------------------------------------------------------------------------------------

ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
cutoff_date <- readRDS(file.path("misc", "Rds", "cutoff_date.Rds"))
stopifnot(all(event_colnames %in% names(ALL.PLAYS)))

# Helper functions --------------------------------------------------------------------------------------

simulate_posterior_predictive <- function(FIT.DT, possible_events, n_sim, batch_size = 10L, seed = 398L) {
  # Randomly sample 250 posterior draws
  K <- length(possible_events)
  n_samples <- 250L
  FIT.DT <- FIT.DT[sample.int(nrow(FIT.DT), n_samples, FALSE)]
  batches <- seq.int(1L, n_samples, by = batch_size)
  
  EVENTS.DTs <- lapply(batches, function(start_idx) {
    end_idx <- min(start_idx + batch_size - 1L, n_samples)
    idx_range <- start_idx:end_idx
    shape_batch <- FIT.DT[idx_range, shape]
    rate_batch <- FIT.DT[idx_range, rate]
    q_batch <- as.matrix(FIT.DT[idx_range, .SD, .SDcols = patterns("^q\\[\\d+\\]$")])
    
    stopifnot(all(abs(rowSums(q_batch) - 1) < 1e-3))
    
    # Call Rcpp function to generate alpha priors
    alpha_flat <- generate_alpha_priors(q_batch, shape_batch, rate_batch, n_sim)
    
    # Reshape flat vector into a 3D array [K, n_sim, batch_size]
    alpha_array <- aperm(array(alpha_flat, dim = c(K, n_sim, length(idx_range))), c(2L, 3L, 1L))
    
    # Sample events using Dirichlet draws
    probs_mat <- aperm(apply(alpha_array, c(2L, 1L), function(a) rdirichlet(1L, a)), c(2L, 1L, 3L))
    stopifnot(all(dplyr::near(apply(probs_mat, c(1L, 3L), sum), 1)))
    
    # Sample events
    event_samples <- apply(probs_mat, c(1L, 3L), function(p) sample(possible_events, size = 1L, prob = p))
    
    # Convert to data.table
    EVENTS.DT <- lapply(seq_along(idx_range), function(b) {
      EVENTS <- as.data.table(as.list(table(event_samples[b, ])))
      miss_cols <- setdiff(possible_events, names(EVENTS))
      if (length(miss_cols)) set(EVENTS, j = miss_cols, value = 0L)
      set(EVENTS, j = "posterior_sample", value = idx_range[b])
      setcolorder(EVENTS, c("posterior_sample", possible_events))
      return(EVENTS)
    })
    
    EVENTS.DT <- rbindlist(EVENTS.DT, use.names = TRUE)
    return(EVENTS.DT)
  })
  
  EVENTS.DT <- rbindlist(EVENTS.DTs, use.names = TRUE)
  setorder(EVENTS.DT, posterior_sample)
  return(EVENTS.DT)
}

# Compare event frequencies between simulated and observed events
evaluate_bayesian_p_values <- function(OBS, PRED, possible_events) {
  # 1) Count total number of observations for each event in our simulated and observed data
  # Tabulate observed counts
  OBS <- OBS[, lapply(.SD, sum), .SDcols = possible_events]
  
  # Tabulate simulated counts for each posterior sample of our hyperpriors
  PRED <- PRED[, lapply(.SD, sum), .SDcols = possible_events, by = .(posterior_sample)]
  
  # 2) For each event, compute fraction of times predicted > observed
  p_vals <- vapply(possible_events, function(event) {
    obs <- OBS[[event]]
    pred <- PRED[[event]]
    eq <- obs == pred
    if (any(eq)) pred[eq] <- pred[eq] + rep_len(c(-1, 1), sum(eq))
    p_val <- mean(pred > obs)
    return(p_val)
  }, numeric(1L))
  
  # 3) Return as a data.table
  P.VALS <- tibble::enframe(p_vals, name = "event", value = "bayesian_p_val")
  setDT(P.VALS)
  return(P.VALS)
}

# Main loop --------------------------------------------------------------------------------------------

for (batter_hand in c("L", "R")) {
  for (pitcher_hand in c("L", "R")) {
    MATCHUPS <- ALL.PLAYS[bat_hand == batter_hand & pit_hand == pitcher_hand]
    stopifnot(nrow(MATCHUPS) > 0L)
    
    for (athome in c(TRUE, FALSE)) {
      for (base_cond in c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3")) {
        combo <- sprintf(
          "%s_%s_bat%s_pit%s", fifelse(athome, "home", "away"), base_cond, batter_hand, pitcher_hand
        )
        message(combo)
        
        fp <- file.path("misc", "ppc", combo, "ppc2.csv")
        
        possible_events <- names(
          Filter(isFALSE, vapply(event_colnames, function(x) all(ALL.PLAYS[get(base_cond), ..x] == 0L), logical(1L)))
        )
        
        OBS <- MATCHUPS[
          date < cutoff_date & at_home == athome & get(base_cond), .SD, .SDcols = possible_events
        ]
        
        fit_fp <- file.path("misc", "stan_fits", combo, "fit.qs")
        fit <- qs::qread(fit_fp)
        FIT.DT <- fit$draws(format = "df")
        setDT(FIT.DT)
        PRED <- simulate_posterior_predictive(FIT.DT, possible_events, n_sim = nrow(OBS))
        P.VALS <- evaluate_bayesian_p_values(OBS, PRED, possible_events)
        rm(PRED); gc()
        
        set(P.VALS, j = "at_home", value = athome)
        set(P.VALS, j = "base_cond", value = base_cond)
        set(P.VALS, j = "bat_hand", value = batter_hand)
        set(P.VALS, j = "pit_hand", value = pitcher_hand)
        
        if (!dir.exists(dirname(fp))) dir.create(dirname(fp), recursive = TRUE)
        fwrite(P.VALS, fp)
      }
    }
  }
}

# Read in results ---------------------------------------------------------------------------------------

P.VALS <- lapply(c("L", "R"), function(batter_hand) {
  P.VALS <- lapply(c("L", "R"), function(pitcher_hand) {
    P.VALS <- lapply(c(TRUE, FALSE), function(athome) {
      P.VALS <- lapply(c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3"), function(base_cond) {
        combo <- sprintf(
          "%s_%s_bat%s_pit%s", fifelse(athome, "home", "away"), base_cond, batter_hand, pitcher_hand
        )
        fp <- file.path("misc", "ppc", combo, "ppc2.csv")
        DT <- fread(fp)
        return(DT)
      })
    })
  })
})
concat <- function(l) unlist(l, recursive = FALSE, use.names = FALSE)
P.VALS <- rbindlist(concat(concat(concat(P.VALS))), use.names = TRUE)
