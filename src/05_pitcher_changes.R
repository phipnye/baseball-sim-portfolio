# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(data.table)
set.seed(42L)

# Import all plays dataset
ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))

# Generate number of pitches distribution ---------------------------------------------------------------

# Event columns for reshaping the data
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))

# All possible number of pitches
n_pitch_counts <- seq_len(max(ALL.PLAYS[, n_pitches], na.rm = TRUE))

# For each event and batter hand - pitcher hand - home/away combo, generate a number of pitches distribution
PITCH.DISTs <- lapply(n_pitch_counts, function(n) {
  # Subset to data with a positive pitch count
  DT <- melt.data.table(
    ALL.PLAYS[n_pitches == n],
    id.vars = c("at_home", "batter", "bat_hand", "pitcher", "pit_hand", "game_id", "inning"),
    measure.vars = event_colnames,
    variable.name = "event",
    value.name = "event_occurred"
  )
  
  DT <- DT[(event_occurred), .N, by = .(at_home, bat_hand, pit_hand, event)]
  set(DT, j = "n_pitches", value = n)
  
  return(DT)
})

# Partial pitch distribution
PITCH.DIST.PARTIAL <- rbindlist(PITCH.DISTs, use.names = TRUE)

# Full pitch distribution
PITCH.DIST <- CJ(
  at_home = c(FALSE, TRUE),
  bat_hand = c("L", "R"),
  pit_hand = c("L", "R"),
  event = event_colnames,
  n_pitches = n_pitch_counts
)

# Merge the counts
PITCH.DIST[PITCH.DIST.PARTIAL, on = .(at_home, bat_hand, pit_hand, event, n_pitches), N := i.N]

### Fill in missing counts
# If any count is non-missing for a given combination, then the others are 0s
PITCH.DIST[, N := fifelse(is.na(N) & any(!is.na(N)), 0L, N), by = .(at_home, bat_hand, pit_hand, event)]

# Rare events may only have a pitch distribution for certain combinations, share these values across
# combinations if still missing
PITCH.DIST[, N := as.double(N)]
PITCH.DIST[, N := fifelse(is.na(N), mean(N, na.rm = TRUE), N), by = .(event, n_pitches)]

# There are 5 events for which we have no information about pitch distribution
stopifnot(PITCH.DIST[is.nan(N), uniqueN(event)] == 5L)

# For these events, fill in with the average across all events
PITCH.DIST[, N := fifelse(is.nan(N), mean(N, na.rm = TRUE), N), by = .(at_home, bat_hand, pit_hand)]

# Confirm no more missing values at this point
stopifnot(!anyNA(PITCH.DIST))

# Reshape the data out wide
PITCH.DIST <- dcast.data.table(
  PITCH.DIST,
  at_home + bat_hand + pit_hand + event ~ n_pitches,
  value.var = "N"
)

# Calculate distributions as smoothed probabilities
prob_cols <- as.character(n_pitch_counts)
epsilon <- 0.5
PITCH.DIST[, (prob_cols) := lapply(.SD, function(x) (x + epsilon) / rowSums(.SD + epsilon)), .SDcols = prob_cols]

# Confirm probability distributions sum to one
stopifnot(all(dplyr::near(PITCH.DIST[, rowSums(.SD), .SDcols = prob_cols], 1)))

# Save out pitch distribution
fwrite(PITCH.DIST, file.path("processed", "pitch_dist.csv"))

# Generate data for modeling pitcher changes ------------------------------------------------------------

# Confirm there are only three plays missing pitches
stopifnot(nrow(ALL.PLAYS[is.na(n_pitches)]) == 3L)

# Generate model data (setup as the probability of switching pitchers after the play)
MODEL.DT <- ALL.PLAYS[!is.na(n_pitches)]
MODEL.DT[, `:=`(
  # Game Context Variables
  pitcher_change = (pitcher != shift(pitcher, type = "lead", fill = NA_character_)),
  new_inning = (inning != shift(inning, type = "lead", fill = NA_integer_)),
  player_on_1st = (p1 | p1p2 | p1p3 | p1p2p3),
  player_on_2nd = (p2 | p1p2 | p2p3 | p1p2p3),
  player_on_3rd = (p3 | p1p3 | p2p3 | p1p2p3),
  
  # Pitcher-Specific Variables
  starting_pitcher = (pitcher == pitcher[1L]),
  lefty_pitcher = (pit_hand == "L"),
  
  # Batter-Specific Variables
  lefty_batter_upcoming = (shift(bat_hand, type = "lead", fill = NA_character_) == "L"),
  
  # Game score
  score_away = if (at_home) NA_integer_ else cumsum(play_runs),
  score_home = if (!at_home) NA_integer_ else cumsum(play_runs)
), by = .(game_id, at_home)]

# Fill in scores
MODEL.DT[inning == 1L & !at_home, score_home := 0L]
MODEL.DT[, `:=`(
  score_away = nafill(score_away, type = "locf"),
  score_home = nafill(score_home, type = "locf")
)]

# Shifted columns will have NAs
shift_cols <- c("pitcher_change", "new_inning", "lefty_batter_upcoming")
MODEL.DT[, (shift_cols) := lapply(.SD, function(x) fifelse(.N == seq_len(.N), FALSE, x)), 
         .SDcols = shift_cols, by = .(game_id, at_home)]

# Variables that need to be calculated within a pitcher grouping
MODEL.DT[, `:=`(
  total_pitches = cumsum(n_pitches),
  runs_allowed = cumsum(play_runs),
  era = (cumsum(play_runs) / inning) * 9,
  whip = cumsum(single + double + triple + hr + doubleplay.single + walk + caught2.walk + caught3.walk + caught4.walk + passedball.walk + walk.wildpitch) / inning,
  strikeout_rate = cumsum(caught2.doubleplay.strikeout + caught2.strikeout + caught3.doubleplay.strikeout + caught3.strikeout + caught4.doubleplay.strikeout + caught4.strikeout + doubleplay.strikeout + erroradv.strikeout + passedball.strikeout + strikeout + strikeout.wildpitch) / seq_len(.N),
  walk_rate = cumsum(caught2.walk + caught3.walk + caught4.walk + passedball.walk + walk + walk.wildpitch) / seq_len(.N)
), by = .(game_id, at_home, pitcher)]

MODEL.DT <- MODEL.DT[, .(
  # Game Context Variables
  pitcher_change,
  inning,
  at_home,
  new_inning,
  total_pitches,
  runs_allowed,
  n_outs = outs_pre,
  player_on_1st,
  player_on_2nd,
  player_on_3rd,
  score_away,
  score_home,
  
  # Pitcher-Specific Variables
  starting_pitcher,
  relief_pitcher = !starting_pitcher & inning < 9L,
  closing_pitcher = NA, # placeholder
  lefty_pitcher,
  era,
  whip,
  strikeout_rate,
  walk_rate,
  
  # Batter-Specific Variables
  lefty_batter_upcoming,
  
  # Temporary variables
  game_id,
  pitcher
)]

# Impute closers
MODEL.DT[, closing_pitcher := !any(starting_pitcher | relief_pitcher), by = .(game_id, pitcher)]

# Only one closer per game (per team)
MODEL.DT[(closing_pitcher), pid := seq_len(.N), by = .(game_id, at_home)]
MODEL.DT[(closing_pitcher), pid := pid[1L], by = .(game_id, pitcher)]
MODEL.DT[(closing_pitcher), closing_pitcher := pid == 1L]
stopifnot(MODEL.DT[(closing_pitcher), .(n_closers = uniqueN(pitcher)), by = .(game_id)][, n_closers] %in% c(1L, 2L))
MODEL.DT[!starting_pitcher & !relief_pitcher & !closing_pitcher, relief_pitcher := TRUE]

# Confirm pitcher is one of three defined types
stopifnot(MODEL.DT[, starting_pitcher + relief_pitcher + closing_pitcher] == 1L)

# Drop temporary variables
set(MODEL.DT, j = c("game_id", "pitcher", "pid"), value = NULL)

# Confirm no missing data
stopifnot(!anyNA(MODEL.DT))

# Save out for pytorch model
fwrite(MODEL.DT, file.path("processed", "pitcher_change.csv"))

# # Bayesian Model with Two-Way Interactions ---------------------------------------------------------------
# 
# # Clear memory
# rm(list = setdiff(names(.GlobalEnv), "MODEL.DT")); gc()
# 
# # Define all predictors
# response <- "pitcher_change"
# predictors <- setdiff(names(MODEL.DT), response)  # Get all predictor variable names
# 
# # Fit Bayesian logistic regression with interactions
# fit <- brm(
#   formula = as.formula(paste(response, " ~ ", paste0(predictors, collapse = " + "))),
#   data = MODEL.DT,
#   family = bernoulli(link = "logit"),
#   prior = c(
#     set_prior("normal(0, 2)", class = "b"),
#     set_prior("normal(0, 5)", class = "Intercept")
#   ),
#   chains = 4L,
#   iter = 2000L,
#   cores = detectCores() - 1L,
#   control = list(adapt_delta = 0.9)
# )
# 
# # Model Evaluation --------------------------------------------------------------------------------------
# 
# # Add WAIC and LOO criteria
# fit <- add_criterion(fit, c("waic", "loo"))
# 
# # Summary and diagnostics
# print(summary(fit))
# print(loo(fit))
# print(waic(fit))
# 
# # Save the model
# saveRDS(fit, file.path("misc", "Rds", "bayesian_pitcher_change_model.Rds"))
# 
# # Check results -----------------------------------------------------------------------------------------
# 
# # Plot credible intervals for coefficients
# plot(fit)
# 
# # Posterior predictive checks
# pp_check(fit)
# 
# # Predict Pitcher Changes:
# preds <- posterior_predict(fit, newdata = MODEL.DT, ndraws = 1L)
# 
# # Visualize Important Predictors:
# conditional_effects(fit)
