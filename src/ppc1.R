# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(actuar)
library(parallel)
library(stringr)
library(data.table)
set.seed(321L)

# Import data
ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
cutoff_date <- readRDS(file.path("misc", "Rds", "cutoff_date.Rds"))
stopifnot(all(event_colnames %in% names(ALL.PLAYS)))

# Helper functions --------------------------------------------------------------------------------------

# Generate posterior predictive samples
simulate_posterior_predictive <- function(fit, possible_events, n_sim) {
  # 1) Extract posterior draws
  NB.R.DT <- fit$draws("nb_r", format = "df")
  NB.P.DT <- fit$draws("nb_p", format = "df")
  Q.DT <- fit$draws("q", format = "df")
  setDT(NB.R.DT)
  setDT(NB.P.DT)
  setDT(Q.DT)
  
  # Select a subset of samples
  smpl <- sample.int(n = nrow(NB.P.DT), size = 250L)
  NB.R.DT <- NB.R.DT[smpl]
  NB.P.DT <- NB.P.DT[smpl]
  Q.DT <- Q.DT[smpl]
  
  EVENTS.DTs <- mclapply(seq_len(nrow(NB.R.DT)), function(i) {
    # Extract NB parameters for iteration i
    nb_r <- unlist(NB.R.DT[i, .SD, .SDcols = patterns("^nb_r")])
    nb_p <- unlist(NB.P.DT[i, .SD, .SDcols = patterns("^nb_p")])
    
    # Extract q vector (length K)
    q_val <- unlist(Q.DT[i, .SD, .SDcols = patterns("^q")])
    
    alpha_mat <- lapply(seq_len(n_sim), function(it_) {
      # Draw total count T from NB, truncated at T >= 1
      T_j <- rztnbinom(1, size = nb_r, prob = nb_p)
      
      # Now draw alpha from Multinomial(T_j, q_val)
      alpha_vec <- as.vector(rmultinom(1, size = T_j, prob = q_val))
      return(alpha_vec)
    })
    
    alpha_mat <- do.call(rbind, alpha_mat)
    
    # Convert alpha_mat to a data.table
    EVENTS <- as.data.table(alpha_mat)
    setnames(EVENTS, possible_events)
    
    # Tag posterior_sample and matchup_id
    set(EVENTS, j = "posterior_sample", value = i)
    set(EVENTS, j = "matchup_id", value = seq_len(n_sim))
    setcolorder(EVENTS, c("posterior_sample", "matchup_id", possible_events))
    
    return(EVENTS)
  }, mc.cores = detectCores() - 1L)
  
  # Combine all iterations
  EVENTS.DT <- rbindlist(EVENTS.DTs, use.names = TRUE)
  setorder(EVENTS.DT, posterior_sample, matchup_id)
  return(EVENTS.DT)
}

# Compare event frequencies between simulated and observed events
evaluate_bayesian_p_values <- function(OBS, PRED, possible_events) {
  # 1) Merge OBS and PRED by 'matchup_id' so we can compare row-by-row
  #    Suffixes ensure we keep distinct columns for the same event name
  M <- merge.data.table(
    OBS,
    PRED,
    by = "matchup_id", 
    suffixes = c("_obs", "_pred"),
    all = TRUE,
    allow.cartesian = FALSE
  )
  
  # 2) For each event, compute fraction of times predicted > observed
  p_vals <- vapply(possible_events, function(event) {
    # Construct predicted vs. observed column names
    obs <- M[[paste0(event, "_obs")]]
    pred <- M[[paste0(event, "_pred")]]
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

# Calculate Bayesian p-values ---------------------------------------------------------------------------

for (batter_hand in c("L", "R")) {
  for (pitcher_hand in c("L", "R")) {
    # Subset to a specific grouping and generate counts of events
    MATCHUPS <- ALL.PLAYS[bat_hand == batter_hand & pit_hand == pitcher_hand]
    stopifnot(nrow(MATCHUPS) > 0L)
    
    for (athome in c(TRUE, FALSE)) {
      for (base_cond in c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3")) {
        combo <- sprintf(
          "%s_%s_bat%s_pit%s", fifelse(athome, "home", "away"), base_cond, batter_hand, pitcher_hand
        )
        print(combo)
        
        # File to save down p-values
        fp <- file.path("misc", "ppc", combo, "ppc1.csv")
        if (file.exists(fp)) next
        
        # Get events that could occur for the given conditions
        possible_events <- names(
          Filter(isFALSE, vapply(event_colnames, function(x) all(ALL.PLAYS[get(base_cond), ..x] == 0L), logical(1L)))
        )
        
        # Generate table of event counts for each matchup
        OBS <- MATCHUPS[
          date < cutoff_date & at_home == athome & get(base_cond),
          lapply(.SD, sum),
          .SDcols = possible_events,
          by = .(batter, pitcher)
        ]
        
        # Add a matchup ID
        OBS[, matchup_id := .I]
        
        # Import fitted model
        if (!file.exists(fit_fp <- file.path("misc", "stan_fits", combo, "fit.qs"))) next
        fit <- qs::qread(fit_fp)        
        
        # For every posterior sample, draw n_sim event occurrences for a given lambda
        PRED <- simulate_posterior_predictive(fit, possible_events, n_sim = nrow(OBS))
        
        # Compute Bayesian p-values
        P.VALS <- evaluate_bayesian_p_values(OBS, PRED, possible_events)
        rm(PRED); gc()
        
        # Add identifiers
        set(P.VALS, j = "at_home", value = athome)
        set(P.VALS, j = "base_cond", value = base_cond)
        set(P.VALS, j = "bat_hand", value = batter_hand)
        set(P.VALS, j = "pit_hand", value = pitcher_hand)
        
        # Save the p-values out
        if (!dir.exists(dirname(fp))) dir.create(dirname(fp))
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
        fp <- file.path("misc", "ppc", combo, "ppc1.csv")
        DT <- fread(fp)
        return(DT)
      })
    })
  })
})
concat <- function(l) unlist(l, recursive = FALSE, use.names = FALSE)
P.VALS <- rbindlist(concat(concat(concat(P.VALS))), use.names = TRUE)
