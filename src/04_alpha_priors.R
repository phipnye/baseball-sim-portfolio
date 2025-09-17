# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(parallel)
library(collapse)
library(data.table)

# Load Rcpp code
Rcpp::sourceCpp(file.path("src", "cpp", "alpha_gen.cpp"))

# Import data
ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
cutoff_date <- readRDS(file.path("misc", "Rds", "cutoff_date.Rds"))
stopifnot(all(event_colnames %in% names(ALL.PLAYS)))

# Draw alpha priors -------------------------------------------------------------------------------------

# Make all batter hands the same for a given matchup
ALL.PLAYS[, bat_hand := fmode(bat_hand, ties = "last"), by = .(batter, pitcher)]
stopifnot(all(ALL.PLAYS[, .(unq = uniqueN(bat_hand)), by = .(batter, pitcher)][, unq] == 1L))

for (batter_hand in c("L", "R")) {
  for (pitcher_hand in c("L", "R")) {
    # Generate all possible player-batter combinations for the given handedness
    HAND.MATCHUPS <- CJ(
      batter = ALL.PLAYS[date >= cutoff_date & bat_hand == batter_hand, unique(batter)],
      pitcher = ALL.PLAYS[date >= cutoff_date & pit_hand == pitcher_hand, unique(pitcher)]
    )
    
    for (athome in c(TRUE, FALSE)) {
      for (base_cond in c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3")) {
        # Import fitted model
        combo <- sprintf(
          "%s_%s_bat%s_pit%s", fifelse(athome, "home", "away"), base_cond, batter_hand, pitcher_hand
        )
        message(combo)
        
        # Create a folder for storing priors
        combo_dir <- file.path("processed", "priors", combo)
        
        if (!dir.exists(combo_dir)) dir.create(combo_dir, recursive = TRUE)
        # if (length(list.files(combo_dir)) == 2000L) next
        
        fit_fp <- file.path("misc", "stan_fits", combo, "fit.qs")
        fit <- qs::qread(fit_fp)
        
        # Get a data.table for every MCMC posterior sample
        FIT.DT <- fit$draws(format = "df")
        setDT(FIT.DT)
        
        # Append additional identifier columns
        set(HAND.MATCHUPS, j = "at_home", value = athome)
        set(HAND.MATCHUPS, j = "base_cond", value = base_cond)
        set(HAND.MATCHUPS, j = "bat_hand", value = batter_hand)
        set(HAND.MATCHUPS, j = "pit_hand", value = pitcher_hand)
        
        # Get events that could occur for the given conditions
        possible_events <- names(
          Filter(isFALSE, vapply(event_colnames, function(x) all(ALL.PLAYS[get(base_cond), ..x] == 0L), logical(1L)))
        )
        
        # Confirm no missing data
        stopifnot(!anyNA(HAND.MATCHUPS))
        
        # Prepare data dimensions
        N_obs <- nrow(HAND.MATCHUPS)
        K <- length(possible_events)
        
        # Process posterior samples in batches of 20
        n_samples <- nrow(FIT.DT)
        batch_size <- 20L
        batches <- seq.int(1L, n_samples, by = batch_size)
        
        for (start_idx in batches) {
          end_idx <- min(start_idx + batch_size - 1L, n_samples)
          idx_range <- start_idx:end_idx
          print(sprintf("Processing batch: %d-%d/%d", start_idx, end_idx, n_samples))
          
          # Extract batch samples
          shape_batch <- FIT.DT[idx_range, shape]
          rate_batch <- FIT.DT[idx_range, rate]
          q_batch <- as.matrix(FIT.DT[idx_range, .SD, .SDcols = patterns("^q\\[\\d+\\]$")])
          
          # Verify q sums to 1 approximately
          stopifnot(all(abs(rowSums(q_batch) - 1) < 1e-3))
          
          # Call Rcpp function to generate alpha priors
          alpha_flat <- generate_alpha_priors(q_batch, shape_batch, rate_batch, N_obs)
          
          # Reshape flat vector into a 3D array [K, N_obs, batch_size]
          alpha_array <- aperm(array(alpha_flat, dim = c(K, N_obs, length(idx_range))), c(2L, 3L, 1L))
          
          # Convert to data.table and save results
          for (b in seq_along(idx_range)) {
            ALPHA.SAMPLE <- as.data.table(alpha_array[, b, ])
            setnames(ALPHA.SAMPLE, sprintf("alpha.%s", possible_events))
            PRIORS <- cbind(HAND.MATCHUPS, ALPHA.SAMPLE)
            stopifnot(!anyNA(PRIORS))
            qs::qsave(PRIORS, file.path(combo_dir, sprintf("priors_%d.qs", idx_range[b])))
          }
        }
      }
    }
  }
}
