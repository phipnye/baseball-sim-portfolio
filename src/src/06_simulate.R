# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(patchwork)
library(collapse)
library(scales)
library(ggplot2)
library(stringr)
library(parallel)
library(data.table)

# Load Simulation Dependencies
Sys.setenv(LD_LIBRARY_PATH = paste(Sys.getenv("LD_LIBRARY_PATH"),
  file.path(Sys.getenv("HOME"), "libtorch/lib"),
  sep = ":"
))
Sys.setenv(Torch_DIR = file.path(Sys.getenv("HOME"), "libtorch"))
Rcpp::sourceCpp(file.path("src", "cpp", "baseball_sim.cpp"))

# Import processed data
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
ALL.PLAYS <- fread(file.path("processed", "all_plays.csv"))
PIT.DIST <- fread(file.path("processed", "pitch_dist.csv"))
RUN.ADV <- fread(file.path("processed", "runner_adv_probs.csv"))
alpha_cols <- sprintf("alpha.%s", event_colnames)
cutoff_date <- readRDS(file.path("misc", "Rds", "cutoff_date.Rds"))

# Define games with named lists and hardcoded lineups ----------------------------------------------------

game_date <- as.IDate("2025-03-28")
games <- list(
  # Game 1
  list(
    "Tigers" = data.table(
      player_id = c(
        "mckiz001",
        "greer003",
        "torrg001",
        "carpk001",
        "keitc001",
        "torks001",
        "sweet001",
        "rogej004",
        "margm001",
        "skubt001",
        "hanib001",
        "vestw001",
        "hurtb001",
        "brieb001"
      ),
      bat_pit = c(
        rep_len("batter", 9L),
        "starter",
        "reliever",
        "reliever",
        "reliever",
        "closer"
      ),
      at_home = FALSE
    ),
    "Dodgers" = data.table(
      player_id = c(
        "ohtas001",
        "bettm001",
        "freef001",
        "hernt002",
        "muncm001",
        "smitw003",
        "confm001",
        "edmat001",
        "pagea001",
        "snelb001",
        "vesia001",
        "banda001",
        "yatek001",
        "scott003"
      ),
      bat_pit = c(
        rep_len("batter", 9L),
        "starter",
        "reliever",
        "reliever",
        "reliever",
        "closer"
      ),
      at_home = TRUE
    )
  ),

  # Game 2
  list(
    "Pirates" = data.table(
      player_id = c(
        "phamt001",
        "reynb001",
        "cruzo001",
        "bartj003",
        "mccua001",
        "gonzn001",
        "rodre006",
        "hayek001",
        "kinei001",
        "kellm003",
        "lawrj002",
        "mayzt001",
        "holdc001",
        "bednd001"
      ),
      bat_pit = c(
        rep_len("batter", 9L),
        "starter",
        "reliever",
        "reliever",
        "reliever",
        "closer"
      ),
      at_home = FALSE
    ),
    "Marlins" = data.table(
      player_id = c(
        "edwax001",
        "stowk001",
        "bridj001",
        "mervm001",
        "lopeo001",
        "conig001",
        "hilld002",
        "paulg001",
        "fortn001",
        "gillc003",
        "venea001",
        "benda001",
        "henre001",
        "faucc001"
      ),
      bat_pit = c(
        rep_len("batter", 9L),
        "starter",
        "reliever",
        "reliever",
        "reliever",
        "closer"
      ),
      at_home = TRUE
    )
  )#,

  # Game 3
  # list(
  #   "Brewers" = data.table(
  #     player_id = c(
  #       "chouj001",
  #       "yelic001",
  #       "contw002",
  #       "mitcg001",
  #       "hoskr001",
  #       "turab002",
  #       "ortij005",
  #       "frels001",
  #       "dunno001",
  #       "peraf001",
  #       "hudsb001",
  #       "pegue001",
  #       "koenj001",
  #       "megit002"
  #     ),
  #     bat_pit = c(
  #       rep_len("batter", 9L),
  #       "starter",
  #       "reliever",
  #       "reliever",
  #       "reliever",
  #       "closer"
  #     ),
  #     at_home = FALSE
  #   ),
  #   "Yankees" = data.table(
  #     player_id = c(
  #       "wella002",
  #       "judga001",
  #       "bellc002",
  #       "goldp001",
  #       "chisj001",
  #       "volpa001",
  #       "domij003",
  #       "riceb001",
  #       "cabro002",
  #       "rodoc001",
  #       "hillt002",
  #       "leitm002",
  #       "weavl001",
  #       "willd004"
  #     ),
  #     bat_pit = c(
  #       rep_len("batter", 9L),
  #       "starter",
  #       "reliever",
  #       "reliever",
  #       "reliever",
  #       "closer"
  #     ),
  #     at_home = TRUE
  #   )
  # )
)

# Simulation Function -----------------------------------------------------------------------------------

# Obtain posterior matchup estimates
prep_game_data <- function(AWAY.TEAM, HOME.TEAM, posterior_draw, away_home = c("away", "home")) {
  # Extract pitchers
  PITCHER.DTs <- lapply(list(AWAY.TEAM, HOME.TEAM), function(DT) {
    DT <- DT[bat_pit != "batter"]
    DT[, pitcher_order := .I - 1L]
    return(DT)
  })
  names(PITCHER.DTs) <- away_home

  # Extract batters
  BATTER.DTs <- lapply(list(AWAY.TEAM, HOME.TEAM), function(DT) {
    DT <- DT[bat_pit == "batter"]
    DT[, batter_order := .I - 1L]
    return(DT)
  })
  names(BATTER.DTs) <- away_home

  # Batting lineup should be 9 rows
  stopifnot(all(vapply(BATTER.DTs, nrow, integer(1L)) == 9L))

  # Generate matchups for filtering
  FILTER.DT <- ALL.PLAYS[, .(bat_hand = fmode(bat_hand, ties = "last"), keep = TRUE), by = .(batter, pitcher)]

  # Obtain event priors
  TEAM.DTs <- lapply(away_home, function(team) {
    other_team <- setdiff(away_home, team)
    dirs <- str_subset(list.dirs(file.path("processed", "priors"), full.names = FALSE), paste0("^", team))
    PRIORs <- lapply(dirs, function(dir) {
      DT <- qs::qread(file.path("processed", "priors", dir, sprintf("priors_%d.qs", posterior_draw)))
      DT <- DT[batter %in% BATTER.DTs[[c(team, "player_id")]]]
      DT <- DT[pitcher %in% PITCHER.DTs[[c(other_team, "player_id")]]]
      DT <- merge.data.table(DT, FILTER.DT, by = c("batter", "pitcher", "bat_hand"), all.x = TRUE, sort = FALSE)
      return(DT)
    })
    TEAM.DT <- rbindlist(PRIORs, use.names = TRUE, fill = TRUE)
    TEAM.DT[, keep := if (.N == 1L) TRUE else keep, by = .(batter, pitcher, base_cond)]
    TEAM.DT[, keep := {
      if (isTRUE(any(keep))) {
        val <- keep
        inds <- c()

        for (pit in unique(pitcher)) {
          if (allNA(keep[pitcher == pit])) inds <- c(inds, whichv(pitcher, pit))
        }

        curr_batter <- batter
        curr_pit_hand <- pit_hand
        BATTER.PLAYS <- ALL.PLAYS[batter == curr_batter & pit_hand == curr_pit_hand]
        hand <- BATTER.PLAYS[, fmode(bat_hand, ties = "last")]
        val[inds] <- bat_hand[inds] == hand
      } else {
        curr_batter <- batter
        curr_pit_hand <- pit_hand
        BATTER.PLAYS <- ALL.PLAYS[batter == curr_batter & pit_hand == curr_pit_hand]
        hand <- BATTER.PLAYS[, fmode(bat_hand, ties = "last")]
        inds <- whichNA(keep)
        val <- keep
        val[inds] <- bat_hand[inds] == hand
      }
      val
    }, by = .(batter, pit_hand)]
    TEAM.DT <- TEAM.DT[(keep)]
    stopifnot(nrow(TEAM.DT) == (nrow(BATTER.DTs[[team]]) * nrow(PITCHER.DTs[[other_team]]) * 8L))
    set(TEAM.DT, j = "keep", value = NULL)
    alpha_cols <- sprintf("alpha.%s", event_colnames)
    set(TEAM.DT, j = setdiff(alpha_cols, names(TEAM.DT)), value = NA_real_)
    TEAM.DT[BATTER.DTs[[team]], on = c(batter = "player_id"), batter_order := i.batter_order]
    TEAM.DT[PITCHER.DTs[[other_team]], on = c(pitcher = "player_id"), c("pitcher_type", "pitcher_order") := .(i.bat_pit, i.pitcher_order)]
    setorder(TEAM.DT, batter_order, pitcher_order)
    setcolorder(
      TEAM.DT,
      c("batter", "pitcher", "batter_order", "pitcher_order", "bat_hand", "pit_hand", "at_home", "base_cond", "pitcher_type")
    )
    return(TEAM.DT)
  })
  names(TEAM.DTs) <- away_home
  return(TEAM.DTs)
}

# Update priors to posteriors using Bayesian updating
update_priors <- function(PRIORs) {
  POSTERIORs <- lapply(PRIORs, function(PRIOR) {
    # Iterate over all base conditions
    POSTERIORs <- lapply(c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3"), function(bc) {
      # Subset PRIOR data based on the base condition
      POSTERIOR.DT <- PRIOR[base_cond == bc]

      # Ensure alpha columns are numeric
      POSTERIOR.DT[, (alpha_cols) := lapply(.SD, as.numeric), .SDcols = alpha_cols]

      # Get relevant plays before the posterior date
      BASE.PLAYS <- ALL.PLAYS[get(bc) & date < game_date & date >= cutoff_date]

      # Compute total appearances for batters and pitchers in this base condition
      POSTERIOR.DT[
        BASE.PLAYS[, .N, by = .(batter, pit_hand)],
        on = .(batter, pit_hand), n_bat := i.N
      ]
      POSTERIOR.DT[
        BASE.PLAYS[, .N, by = .(pitcher, bat_hand)],
        on = .(pitcher, bat_hand), n_pit := i.N
      ]
      setnafill(POSTERIOR.DT, type = "const", fill = 0L, cols = c("n_bat", "n_pit"))
      POSTERIOR.DT[, n_ttl := n_bat + n_pit] # Total appearances

      # Iterate over all event types and update the corresponding alpha values
      for (event in event_colnames) {
        alpha_col <- sprintf("alpha.%s", event) # Column name for alpha values

        # Subset plays where the event occurred
        EVENT.PLAYS <- BASE.PLAYS[get(event)]

        # Compute event occurrence counts for batters and pitchers
        POSTERIOR.DT[
          EVENT.PLAYS[, .N, by = .(batter, pit_hand)],
          on = .(batter, pit_hand), bat_sum := i.N
        ]
        POSTERIOR.DT[
          EVENT.PLAYS[, .N, by = .(pitcher, bat_hand)],
          on = .(pitcher, bat_hand), pit_sum := i.N
        ]

        # Replace missing values with zero
        setnafill(POSTERIOR.DT, type = "const", fill = 0L, cols = c("bat_sum", "pit_sum"))

        # Bayesian update: Adjust alpha values based on weighted event frequencies
        POSTERIOR.DT[n_ttl != 0, (alpha_col) := get(alpha_col) + (n_bat / n_ttl) * bat_sum + (n_pit / n_ttl) * pit_sum]

        # Remove temporary columns
        set(POSTERIOR.DT, j = c("bat_sum", "pit_sum"), value = NULL)
      }

      # Remove temporary count columns
      set(POSTERIOR.DT, j = c("n_bat", "n_pit", "n_ttl"), value = NULL)
      return(POSTERIOR.DT)
    })

    # Combine all base condition posteriors into a single data.table
    POSTERIORS <- rbindlist(POSTERIORs, use.names = TRUE, fill = FALSE)
    setorder(POSTERIORS, batter_order, pitcher_order, base_cond)
    stopifnot(rowSums(is.na(POSTERIORS[, ..alpha_cols])) != length(alpha_cols))
    return(POSTERIORS)
  })

  return(POSTERIORs)
}

simulate_game <- function(game) {
  teams <- names(game)
  away_team <- teams[vapply(game, function(DT) all(DT[, !at_home]), logical(1L))]
  home_team <- teams[vapply(game, function(DT) all(DT[, at_home]), logical(1L))]
  AWAY.TEAM <- game[[away_team]]
  HOME.TEAM <- game[[home_team]]

  sim_game <- function(posterior_draw) {
    results <- tryCatch(
      {
        cat(sprintf("%d/2000\n", posterior_draw), file = log_file, append = TRUE)

        # Obtain team event priors
        PRIORs <- prep_game_data(AWAY.TEAM, HOME.TEAM, posterior_draw)

        # Update priors to posteriors
        POSTERIORs <- update_priors(PRIORs)

        # Simulate games
        SIMS <- sim_game_cpp(
          POSTERIORs,
          RUN.ADV,
          PIT.DIST,
          file.path("misc", "torch", "pitcher_change_model.pt"),
          alpha_cols,
          n_sim = 100L
        )

        setDT(SIMS)
        SIMS[, seed := .I]
        set(SIMS, j = "posterior_draw", value = posterior_draw)
        return(SIMS)
      },
      error = function(e) e$message
    )

    return(results)
  }

  # Simulate --------------------------------------------------------------------------------------------

  log_fp <- file.path("log", "simulation.log")
  if (file.exists(log_fp)) invisible(file.remove(log_fp))
  log_file <- file(log_fp, open = "w")
  sims <- mclapply(seq_len(2000L), sim_game, mc.cores = detectCores(), mc.preschedule = FALSE)
  close(log_file)

  # Consolidate results ---------------------------------------------------------------------------------

  errors <- Filter(is.character, sims)
  SIMs <- Filter(is.data.table, sims)
  stopifnot(length(errors) + length(SIMs) == 2000L)

  if (length(errors)) {
    unq_errors <- sort(unique(unlist(errors)))
    msg <- sprintf("Simulation errors for %s vs. %s:\n%s", away_team, home_team, paste0(unq_errors, collapse = "\n"))
    warning(msg, immediate. = TRUE)
  }

  SIMS <- rbindlist(SIMs, use.names = TRUE)

  # Save results
  fwrite(
    SIMS,
    file.path(
      "results",
      tolower(str_remove_all(sprintf("%s_%s_%s.csv", away_team, home_team, format(game_date, "%Y%m%d")), "\\s"))
    )
  )

  # Compute summary statistics ---------------------------------------------------------------------------

  AVG.STATS <- SIMS[, .(
    away_win_pct = mean(away > home),
    home_win_pct = mean(away < home),
    avg_away_score = mean(away),
    avg_home_score = mean(home)
  )]

  # Compute 95% credible intervals
  CI <- rbindlist(
    lapply(SIMs, function(DT) {
      DT[, .(
        away_win_pct = mean(away > home),
        home_win_pct = mean(away < home),
        avg_away_score = mean(away),
        avg_home_score = mean(home)
      )]
    }),
    use.names = TRUE
  )

  CI <- CI[, lapply(.SD, quantile, c(0.025, 0.975))]
  CI <- transpose(CI, keep.names = "stat")
  setnames(CI, c("stat", "qnt_2.5", "qnt_97.5"))

  # Merge means with credible intervals
  VIZ.DT <- merge.data.table(
    melt.data.table(
      AVG.STATS,
      measure.vars = c("home_win_pct", "away_win_pct", "avg_home_score", "avg_away_score"),
      variable.name = "stat", value.name = "mean_val"
    ),
    CI,
    by = "stat",
    sort = FALSE,
    allow.cartesian = FALSE
  )

  set(VIZ.DT, j = "game", value = sprintf("%s vs %s", away_team, home_team))
  return(VIZ.DT)
}

# Run Simulations for Multiple Games --------------------------------------------------------------------

start_time <- Sys.time()
ALL.RESULTS <- rbindlist(lapply(games, simulate_game), use.names = TRUE)
end_time <- Sys.time()
print(end_time - start_time)

# Visualize Results -------------------------------------------------------------------------------------

# Define intuitive labels for your statistics
label_map <- c(
  home_win_pct = "Home Team Win %",
  away_win_pct = "Away Team Win %",
  avg_home_score = "Home Team Runs",
  avg_away_score = "Away Team Runs"
)

# Create clear variable type labels
ALL.RESULTS[, stat_label := label_map[stat]]
ALL.RESULTS[, category := fifelse(str_detect(stat, "win_pct"), "Win Probability", "Average Runs")]

# Order factors for clearer visualization
ALL.RESULTS[, stat_label := factor(stat_label, levels = c("Away Team Runs", "Home Team Runs", "Away Team Win %", "Home Team Win %"))]

# Create separate data subsets
WIN.PCT.DT <- ALL.RESULTS[category == "Win Probability"]
RUNS.SCORED.DT <- ALL.RESULTS[category == "Average Runs"]

# Plot for Win Probabilities
p1 <- ggplot(WIN.PCT.DT, aes(x = stat_label, y = mean_val, color = game)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = qnt_2.5, ymax = qnt_97.5),
    width = 0.25, linewidth = 0.8, alpha = 0.7,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~game, scales = "free_y", ncol = 1) +
  labs(x = NULL, y = "Win Probability") +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )

# Plot for Runs Scored
p2 <- ggplot(RUNS.SCORED.DT, aes(x = stat_label, y = mean_val, color = game)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = qnt_2.5, ymax = qnt_97.5),
    width = 0.25, linewidth = 0.8, alpha = 0.7,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~game, scales = "free_y", ncol = 1) +
  labs(x = NULL, y = "Runs Scored") +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )

# Now combine using patchwork
final_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Baseball Simulation Results",
    subtitle = "Mean Estimates with 95% Credible Intervals"
  ) &
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16)
  )

# Display it
final_plot
