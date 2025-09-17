# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(stringr)
library(data.table)
library(cmdstanr)

# Import data
ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
cutoff_date <- readRDS(file.path("misc", "Rds", "cutoff_date.Rds"))
stopifnot(all(event_colnames %in% names(ALL.PLAYS)))

# Stan model
mod <- cmdstan_model(file.path("src", "stan", "mcmc.stan"))

# Helper functions --------------------------------------------------------------------------------------

# Save fitted model object
save_fit <- function(fit, fname) {
  # Load CmdStan output files into the fitted model object.
  fit$draws() # Load posterior draws into the object.
  try(fit$sampler_diagnostics(), silent = TRUE) # Load sampler diagnostics.
  try(fit$init(), silent = TRUE) # Load user-defined initial values.
  try(fit$profiles(), silent = TRUE) # Load profiling samples.
  
  # Save the object to a file.
  qs::qsave(x = fit, file = fname)
  return(invisible(NULL))
}

# MCMC hyperpriors --------------------------------------------------------------------------------------

for (batter_hand in c("L", "R")) {
  for (pitcher_hand in c("L", "R")) {
    # Subset to a specific grouping and generate counts of events
    MATCHUPS <- ALL.PLAYS[bat_hand == batter_hand & pit_hand == pitcher_hand]
    stopifnot(nrow(MATCHUPS) > 0L)
    
    for (athome in c(TRUE, FALSE)) {
      for (base_cond in c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3")) {
        # Get events that could occur for the given conditions
        possible_events <- names(
          Filter(isFALSE, vapply(event_colnames, function(x) all(ALL.PLAYS[get(base_cond), ..x] == 0L), logical(1L)))
        )
        
        # Create a table: one row per matchup (batter-pitcher combination) and one column per event.
        # Aggregate data by matchup (e.g., by batter and pitcher IDs)
        # Generate counts for observed matchups (prior to cutoff date)
        OBS.MATCHUPS <- MATCHUPS[
          date < cutoff_date & at_home == athome & get(base_cond),
          lapply(.SD, sum),
          by = .(batter, pitcher),
          .SDcols = possible_events
        ]
        
        if (nrow(OBS.MATCHUPS) == 0L) {
          msg <- sprintf(
            "Missing data for: at_home = %s - base_cond = %s - bat_hand = %s - pit_hand = %s",
            athome, base_cond, batter_hand, pitcher_hand
          )
          print(msg)
          next
        }
        
        # Confirm no missing data
        stopifnot(!anyNA(OBS.MATCHUPS))
        
        # Create a matrix of counts: each row is a matchup, columns correspond to events.
        counts_obs <- as.matrix(OBS.MATCHUPS[, ..possible_events])
        stopifnot(is.integer(counts_obs) && all(counts_obs >= 0L) && all(rowSums(counts_obs) > 0L))
        
        # Set parameters
        N_obs <- nrow(counts_obs)
        K <- ncol(counts_obs)
        n_sample <- 500L
        n_markov_chains <- 4L
        S <- n_sample * n_markov_chains
        
        # Create the data list for Stan:
        data_stan <- list(
          N_obs = N_obs,
          K = K,
          counts_obs = counts_obs
        )
        
        # Setup directories to save results
        print(combo <- sprintf(
          "%s_%s_bat%s_pit%s", fifelse(athome, "home", "away"), base_cond, batter_hand, pitcher_hand
        ))
        
        if (!dir.exists(fit_dir <- file.path("misc", "stan_fits", combo))) {
          dir.create(fit_dir)
        }
        
        # if (file.exists(file.path(fit_dir, "fit.qs"))) {
        #   next
        # }
        
        if (!dir.exists(out_dir <- file.path("misc", "cmdstan", combo))) {
          dir.create(out_dir)
        }
        
        # Remove old files
        if (length(old_out_files <- list.files(out_dir, full.names = TRUE))) {
          invisible(file.remove(old_out_files))
        }
        
        # Run MCMC with the full hierarchical model
        fit <- mod$sample(
          data = data_stan,
          seed = 395L,
          chains = n_markov_chains,
          parallel_chains = parallel::detectCores() - 1L,
          refresh = 100L,
          output_dir = out_dir,
          iter_warmup = 1000L,
          iter_sampling = n_sample
        )
        
        # Save the model
        save_fit(fit, file.path(fit_dir, "fit.qs"))
      }
    }
  }
}
