# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(stringr)
library(data.table)
DT <- fread(file.path("raw", "2022", "2022plays.csv"))

# Subset to desired columns and omit all-star games
DT <- DT[gametype != "allstar", .(
  gid, inning, vis_home, batter, pitcher, lp, bathand, nump, pithand, single, double, triple, hr, sh, sf,
  hbp, walk, iw, k, xi, othout, noout, gdp, othdp, tp, wp, pb, bk, di, sb2, sb3, sbh, cs2, cs3, csh,
  pko1, pko2, pko3, e1, e2, e3, e4, e5, e6, e7, e8, e9, outs_pre, outs_post, br1_pre, br2_pre, br3_pre,
  br1_post, br2_post, br3_post, run_b, run1, run2, run3, runs, date, gametype
)]

# Data checks 1 -----------------------------------------------------------------------------------------

# Confirm binaries
for (cname in c(
  "vis_home", "single", "double", "triple", "hr", "sh", "sf", "hbp", "walk", "iw", "k", "xi", "othout",
  "noout", "gdp", "othdp", "tp", "wp", "pb", "bk", "di", "sb2", "sb3", "sbh", "cs2", "cs3", "csh",
  "pko1", "pko2", "pko3"
)) {
  if (!all(DT[[cname]] %in% c(0L, 1L))) {
    stop(sprintf("Column %s is not binary.", cname))
  }
  DT[, (cname) := lapply(.SD, as.logical), .SDcols = cname]
}
rm(cname)

# Confirm runners batted in were those on the corresponding plate before the play
stopifnot(DT[run1 != "", all(br1_pre == run1)])
stopifnot(DT[run2 != "", all(br2_pre == run2)])
stopifnot(DT[run3 != "", all(br3_pre == run3)])

# Process data ------------------------------------------------------------------------------------------

# Make errors binary
DT[, sprintf("e%d", seq_len(9L)) := lapply(.SD, as.logical), .SDcols = sprintf("e%d", seq_len(9L))]

DT <- DT[, .(
  # ID variables
  inning,
  at_home = vis_home,
  date = as.IDate(as.character(date), format = "%Y%m%d"),
  game_type = gametype,
  batter,
  bat_hand = bathand,
  pitcher,
  pit_hand = pithand,
  
  # Events
  balk = bk,
  caught_stealing_2 = cs2,
  caught_stealing_3 = cs3,
  caught_stealing_4 = csh,
  defensive_indifference = di,
  double,
  double_play = (gdp | othdp),
  error_advance = ((e1 | e2 | e3 | e4 | e5 | e6 | e7 | e8 | e9) & (batter == br1_post | batter == br2_post | batter == br3_post)),
  hr,
  other_no_out = noout,
  other_out = othout,
  passed_ball = pb,
  pickoff_base_1 = pko1,
  pickoff_base_2 = pko2,
  pickoff_base_3 = pko3,
  sacrifice_bunt = sh,
  sacrifice_fly = sf,
  single,
  stolen_base_2 = sb2,
  stolen_base_3 = sb3,
  stolen_base_4 = sbh,
  strikeout = k,
  triple,
  triple_play = tp,
  walk = (hbp | walk | iw | xi),
  wild_pitch = wp,
  
  # Base runner advancements (cannot infer runner advancement with 3 outs after the play)
  advance_0_to_1 = fifelse(outs_post != 3L, batter == br1_post, NA),
  advance_0_to_2 = fifelse(outs_post != 3L, batter == br2_post, NA),
  advance_0_to_3 = fifelse(outs_post != 3L, batter == br3_post, NA),
  advance_0_to_4 = (batter == run_b),
  advance_1_to_2 = fifelse(outs_post != 3L, br1_pre == br2_post & br1_pre != "", fifelse(br1_pre == "", FALSE, NA)),
  advance_1_to_3 = fifelse(outs_post != 3L, br1_pre == br3_post & br1_pre != "", fifelse(br1_pre == "", FALSE, NA)),
  advance_1_to_4 = (run1 != ""),
  advance_2_to_3 = fifelse(outs_post != 3L, br2_pre == br3_post & br2_pre != "", fifelse(br2_pre == "", FALSE, NA)),
  advance_2_to_4 = (run2 != ""),
  advance_3_to_4 = (run3 != ""),
  play_runs = runs,
  
  # Base runner outs
  out_0 = fcase(
    # For 3 outs post play, we cannot determine who's out from initial data unless everybody is out on the play
    ((outs_post != 3) | ((br1_pre != "") + (br2_pre != "") + (br3_pre != "") + 1L) == (outs_post - outs_pre)),
    (batter != shift(batter, type = "lead") | seq_len(.N) == .N) & batter != br1_post & batter != br2_post & batter != br3_post & batter != run_b,
    default = NA
  ),
  out_1 = fcase(
    br1_pre == "", FALSE,
    ((outs_post != 3) | ((br1_pre != "") + (br2_pre != "") + (br3_pre != "") + 1L) == (outs_post - outs_pre)),
    br1_pre != br1_post & br1_pre != br2_post & br1_pre != br3_post & br1_pre != run1,
    default = NA
  ),
  out_2 = fcase(
    br2_pre == "", FALSE,
    ((outs_post != 3) | ((br1_pre != "") + (br2_pre != "") + (br3_pre != "") + 1L) == (outs_post - outs_pre)),
    br2_pre != br2_post & br2_pre != br3_post & br2_pre != run2,
    default = NA
  ),
  out_3 = fcase(
    br3_pre == "", FALSE,
    ((outs_post != 3) | ((br1_pre != "") + (br2_pre != "") + (br3_pre != "") + 1L) == (outs_post - outs_pre)),
    br3_pre != br3_post & br3_pre != run3,
    default = NA
  ),
  
  # Conditionals
  player_on_1st = (br1_pre != ""),
  player_on_2nd = (br2_pre != ""),
  player_on_3rd = (br3_pre != ""),
  player_on_base = (br1_pre != "" | br2_pre != "" | br3_pre != ""),
  
  # Additional information
  n_pitches = nump,
  lineup_position = lp,
  
  # Temporary variables
  outs_pre,
  outs_post
), by = .(game_id = gid)]

# Impute outs -------------------------------------------------------------------------------------------

# Manually review outs for the last play of an inning
OUTS <- fread(file.path("raw", "2022", "matched_last_inning_plays_w_desc.csv"))
stopifnot(!anyNA(OUTS))

# Merge the outs onto our data
DT[OUTS, on = .(game_id, inning, at_home, outs_post),
   sprintf("out_%d_reviewed", 0L:3L) := .(i.out_0, i.out_1, i.out_2, i.out_3)
]

# Confirm there are no disagreements
stopifnot(DT[!is.na(out_0) & !is.na(out_0_reviewed), all(out_0 == out_0_reviewed)])
stopifnot(DT[!is.na(out_1) & !is.na(out_1_reviewed), all(out_1 == out_1_reviewed)])
stopifnot(DT[!is.na(out_2) & !is.na(out_2_reviewed), all(out_2 == out_2_reviewed)])
stopifnot(DT[!is.na(out_3) & !is.na(out_3_reviewed), all(out_3 == out_3_reviewed)])

# Fill in out values
DT[!is.na(out_0_reviewed), out_0 := out_0_reviewed]
DT[!is.na(out_1_reviewed), out_1 := out_1_reviewed]
DT[!is.na(out_2_reviewed), out_2 := out_2_reviewed]
DT[!is.na(out_3_reviewed), out_3 := out_3_reviewed]

# There are several game-ending plays in which we incorrectly mark the batter was out
stopifnot(nrow(DT[inning >= 9L & at_home & (out_0 + out_1 + out_2 + out_3 + outs_pre != outs_post)]) == 7L)
DT[inning >= 9L & at_home & (out_0 + out_1 + out_2 + out_3 + outs_pre != outs_post), out_0 := FALSE]

# Confirm no more missing base outs
stopifnot(!anyNA(DT[, .(out_0, out_1, out_2, out_3)]))
stopifnot(all(DT[, outs_pre + out_0 + out_1 + out_2 + out_3 == outs_post]))

# Confirm there was a player on base for every out
stopifnot(DT[(out_1), all(player_on_1st)])
stopifnot(DT[(out_2), all(player_on_2nd)])
stopifnot(DT[(out_3), all(player_on_3rd)])

# Confirm max one event per player
stopifnot(DT[, all(rowSums(.SD, na.rm = TRUE) <= 1L), .SDcols = c("advance_0_to_1", "advance_0_to_2", "advance_0_to_3", "advance_0_to_4", "out_0")])
stopifnot(DT[, all(rowSums(.SD, na.rm = TRUE) <= 1L), .SDcols = c("advance_1_to_2", "advance_1_to_3", "advance_1_to_4", "out_1")])
stopifnot(DT[, all(rowSums(.SD, na.rm = TRUE) <= 1L), .SDcols = c("advance_2_to_3", "advance_2_to_4", "out_2")])
stopifnot(DT[, all(rowSums(.SD, na.rm = TRUE) <= 1L), .SDcols = c("advance_3_to_4", "out_3")])

# Confirm the runs scored on the play equate to the number of base runners advanced home
stopifnot(all(DT[, rowSums(.SD) == play_runs, .SDcols = sprintf("advance_%d_to_4", 0L:3L)]))

# Drop the manual entry columns
DT[, sprintf("out_%d_reviewed", 0L:3L) := NULL]

# Impute advancements based on whether player(s) were out on the play
DT[(out_0), sprintf("advance_0_to_%d", 1L:4L) := FALSE]
DT[(out_1), sprintf("advance_1_to_%d", 2L:4L) := FALSE]
DT[(out_2), sprintf("advance_2_to_%d", 3L:4L) := FALSE]
DT[(out_3), advance_3_to_4 := FALSE]

# Impute others -----------------------------------------------------------------------------------------

# Impute "other" outs where no other events occur
DT[
  !(
    balk | caught_stealing_2 | caught_stealing_3 | caught_stealing_4 | defensive_indifference | double |
      double_play | error_advance | hr | other_no_out | other_out | passed_ball | pickoff_base_1 |
      pickoff_base_2 | pickoff_base_3 | sacrifice_bunt | sacrifice_fly | single | strikeout | triple |
      triple_play | walk | wild_pitch
  ),
  other_out := fifelse(out_0 | out_1 | out_2 | out_3, TRUE, other_out)
]

# Impute "other" no outs where no other events occur
DT[
  !(
    balk | caught_stealing_2 | caught_stealing_3 | caught_stealing_4 | defensive_indifference | double |
      double_play | error_advance | hr | other_no_out | other_out | passed_ball | pickoff_base_1 |
      pickoff_base_2 | pickoff_base_3 | sacrifice_bunt | sacrifice_fly | single | strikeout | triple |
      triple_play | walk | wild_pitch
  ),
  other_no_out := fifelse(!(out_0 | out_1 | out_2 | out_3), TRUE, other_no_out)
]

# Reconcile multi-event plays ---------------------------------------------------------------------------

# Prioritize sacrifices over other outs
DT[(sacrifice_bunt | sacrifice_fly) & other_out, other_out := FALSE]

# Prioritize double and triple plays over other outs
DT[(other_out & (double_play | triple_play)), other_out := FALSE]

# Prioritize singles, doubles, and triples over error advancements
DT[error_advance & (single | double | triple), error_advance := FALSE]

# Assume errors don't occur frequently on walks (prioritize walks) - these tend to be instances like catcher's
# interference
stopifnot(nrow(DT[walk & error_advance]) == 75L)
DT[walk & error_advance, error_advance := FALSE]

# Prioritize errors over "other" no out events
DT[error_advance & other_no_out, other_no_out := FALSE]

# Prioritize wild pitches over error advancements
DT[error_advance & wild_pitch, error_advance := FALSE]

# Prioritize passed balls over error advancements
DT[error_advance & passed_ball, error_advance := FALSE]

# Prioritize wild pitches over defensive indifferences
DT[wild_pitch & defensive_indifference, defensive_indifference := FALSE]

# Prioritize passed balls over defensive indifferences
DT[passed_ball & defensive_indifference, defensive_indifference := FALSE]

# Manually update specific records
source(file.path("src", "2022", "manual_updates.R"))

# Reconcile multi-event plays ---------------------------------------------------------------------------

### Add variables for multi-event plays

# Caught stealing on strikeouts
DT[, `:=`(
  caught_stealing_2_strikeout = caught_stealing_2 & !caught_stealing_3 & !caught_stealing_4 & !double_play & strikeout,
  caught_stealing_3_strikeout = !caught_stealing_2 & caught_stealing_3 & !caught_stealing_4 & !double_play & strikeout,
  caught_stealing_4_strikeout = !caught_stealing_2 & !caught_stealing_3 & caught_stealing_4 & !double_play & strikeout,
  caught_stealing_2_double_play_strikeout = caught_stealing_2 & !caught_stealing_3 & !caught_stealing_4 & double_play & strikeout,
  caught_stealing_3_double_play_strikeout = !caught_stealing_2 & caught_stealing_3 & !caught_stealing_4 & double_play & strikeout,
  caught_stealing_4_double_play_strikeout = !caught_stealing_2 & !caught_stealing_3 & caught_stealing_4 & double_play & strikeout
)]
DT[(caught_stealing_2_strikeout), c("caught_stealing_2", "strikeout") := FALSE] 
DT[(caught_stealing_3_strikeout), c("caught_stealing_3", "strikeout") := FALSE]
DT[(caught_stealing_4_strikeout), c("caught_stealing_4", "strikeout") := FALSE]
DT[(caught_stealing_2_double_play_strikeout), c("caught_stealing_2", "double_play", "strikeout") := FALSE] 
DT[(caught_stealing_3_double_play_strikeout), c("caught_stealing_3", "double_play", "strikeout") := FALSE]
DT[(caught_stealing_4_double_play_strikeout), c("caught_stealing_4", "double_play", "strikeout") := FALSE]

# Caught stealing on walks
DT[, `:=`(
  caught_stealing_2_walk = caught_stealing_2 & !caught_stealing_3 & !caught_stealing_4 & walk,
  caught_stealing_3_walk = !caught_stealing_2 & caught_stealing_3 & !caught_stealing_4 & walk,
  caught_stealing_4_walk = !caught_stealing_2 & !caught_stealing_3 & caught_stealing_4 & walk
)]
DT[(caught_stealing_2_walk), c("caught_stealing_2", "walk") := FALSE] 
DT[(caught_stealing_3_walk), c("caught_stealing_3", "walk") := FALSE]
DT[(caught_stealing_4_walk), c("caught_stealing_4", "walk") := FALSE]

# Double play multi-events
DT[, `:=`(
  double_play_sacrifice_bunt = double_play & sacrifice_bunt,
  double_play_sacrifice_fly = double_play & sacrifice_fly,
  double_play_single = double_play & single,
  double_play_strikeout = double_play & strikeout
)]
DT[(double_play_sacrifice_bunt), c("double_play", "sacrifice_bunt") := FALSE]
DT[(double_play_sacrifice_fly), c("double_play", "sacrifice_fly") := FALSE]
DT[(double_play_single), c("double_play", "single") := FALSE]
DT[(double_play_strikeout), c("double_play", "strikeout") := FALSE]

# Error advances
DT[, `:=`(
  error_advance_other_out = error_advance & other_out,
  error_advance_sacrifice_bunt = error_advance & sacrifice_bunt,
  error_advance_sacrifice_fly = error_advance & sacrifice_fly,
  error_advance_strikeout = error_advance & strikeout
)]
DT[(error_advance_other_out), c("error_advance", "other_out") := FALSE]
DT[(error_advance_sacrifice_bunt), c("error_advance", "sacrifice_bunt") := FALSE]
DT[(error_advance_sacrifice_fly), c("error_advance", "sacrifice_fly") := FALSE]
DT[(error_advance_strikeout), c("error_advance", "strikeout") := FALSE]

# Passed balls and wild pitches on strikeouts and walks
DT[, `:=`(
  passed_ball_strikeout = passed_ball & strikeout,
  passed_ball_walk = passed_ball & walk,
  strikeout_wild_pitch = strikeout & wild_pitch,
  walk_wild_pitch = walk & wild_pitch
)]
DT[(passed_ball_strikeout), c("passed_ball", "strikeout") := FALSE]
DT[(passed_ball_walk), c("passed_ball", "walk") := FALSE]
DT[(strikeout_wild_pitch), c("strikeout", "wild_pitch") := FALSE]
DT[(walk_wild_pitch), c("walk", "wild_pitch") := FALSE]

# Data checks 2 -----------------------------------------------------------------------------------------

### Confirm one event per play
event_cols <- readRDS(file.path("misc", "Rds", "event_cols.Rds"))

if (DT[, !all(rowSums(.SD) == 1L), .SDcols = event_cols]) {
  hold <- DT[, .SD[rowSums(.SD) != 1L], .SDcols = event_cols]
  hold[, check := do.call(paste, c(as.list(names(Filter(isTRUE, .SD))), sep = " + ")), by = seq_len(nrow(hold))]
  setcolorder(hold, "check")
  stop(sprintf("Multiple events detected:\n%s", paste0(sort(unique(hold[, check])), collapse = "\n")))
}

# Generate players on base conditionals
DT[, `:=`(
  pno = (!player_on_1st & !player_on_2nd & !player_on_3rd),
  p1 = (player_on_1st & !player_on_2nd & !player_on_3rd),
  p2 = (!player_on_1st & player_on_2nd & !player_on_3rd),
  p3 = (!player_on_1st & !player_on_2nd & player_on_3rd),
  p1p2 = (player_on_1st & player_on_2nd & !player_on_3rd),
  p1p3 = (player_on_1st & !player_on_2nd & player_on_3rd),
  p2p3 = (!player_on_1st & player_on_2nd & player_on_3rd),
  p1p2p3 = (player_on_1st & player_on_2nd & player_on_3rd),
  player_on_1st = NULL, # drop old versions
  player_on_2nd = NULL,
  player_on_3rd = NULL
)]

# Confirm our conditionals are mutually exlusive
stopifnot(all(DT[, rowSums(.SD) == 1L, .SDcols = c("pno", "p1", "p2", "p3", "p1p2", "p1p3", "p2p3", "p1p2p3")]))

# All sacrifices have a player on base and advance a player
stopifnot(DT[sacrifice_bunt | sacrifice_fly, all(player_on_base)])
stopifnot(DT[sacrifice_bunt | sacrifice_fly, all(advance_1_to_2 | advance_1_to_3 | advance_1_to_4 | advance_2_to_3 | advance_2_to_4 | advance_3_to_4)])

# Confirm only one player out on "other" outs
stopifnot(DT[(other_out), all(outs_post == outs_pre + 1L)])

# Confirm nobody gets out on "other" no outs
stopifnot(DT[(other_no_out), all(outs_pre == outs_post)])

# Confirm balks only occur with player(s) on base
stopifnot(DT[(balk), all(player_on_base)])

# Confirm defensive indifferences only occur with player(s) on base
stopifnot(DT[(defensive_indifference), all(player_on_base)])

# Confirm balks always advance all runners on base
stopifnot(DT[(balk & p1), all(advance_1_to_2)])
stopifnot(DT[(balk & p2), all(advance_2_to_3)])
stopifnot(DT[(balk & p3), all(advance_3_to_4)])

# If defensive indifference occurs, confirm nobody gets out
stopifnot(DT[(defensive_indifference & p1), all(outs_pre == outs_post)])

# Confirm double plays on strikeouts are just from getting caught stealing
stopifnot(DT[strikeout & double_play, all(caught_stealing_2 | caught_stealing_3 | caught_stealing_4)])

# Confirm error advances with other out always have a player on base
stopifnot(DT[(error_advance_other_out), all(player_on_base)])

# Confirm no defensive indifferences when no players are on
stopifnot(DT[(pno), all(!defensive_indifference)])

# Save out ----------------------------------------------------------------------------------------------

setcolorder(
  DT,
  c("game_id", "inning", "at_home", "date", "game_type", "batter", "bat_hand", "pitcher", "pit_hand", "n_pitches", event_cols)
)

fwrite(DT, file.path("processed", "2022_plays.csv"))
