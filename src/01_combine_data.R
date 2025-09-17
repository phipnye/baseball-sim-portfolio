# Setup -------------------------------------------------------------------------------------------------

setwd("~/Documents/R projects/baseball/")
library(data.table)
set.seed(93L)

# Import data
ALL.PLAYS <- lapply(2021L:2024L, function(year) {
  f <- file.path("processed", sprintf("%d_plays.csv", year))
  fread(f)
})
ALL.PLAYS <- rbindlist(ALL.PLAYS, use.names = TRUE)

# There are 128 instances where nothing happens on a play (e.g., dropped foul ball)
adv_cols <- c(
  "out_0", "advance_0_to_1", "advance_0_to_2", "advance_0_to_3", "advance_0_to_4",
  "out_1", "advance_1_to_2", "advance_1_to_3", "advance_1_to_4",
  "out_2", "advance_2_to_3", "advance_2_to_4",
  "out_3", "advance_3_to_4"
)
no_event_rows <- ALL.PLAYS[, rowSums(.SD, na.rm = TRUE) == 0L, .SDcols = adv_cols]
stopifnot(sum(no_event_rows) == 128L)

# Drop these from our data since they are so rare and have minimal game outcome implications
ALL.PLAYS <- ALL.PLAYS[!(no_event_rows)]
rm(adv_cols, no_event_rows)

# Determine switch hitter hands -------------------------------------------------------------------------

# Import switch hitter matchups
SWITCH.HITTERS <- fread(file.path("raw", "switch_hitter_hands.csv"))

# Impute "Unknowns" and "Left/Right" matchups
stopifnot(nrow(SWITCH.HITTERS[bat_hand == "Unknown"]) == 23L)
stopifnot(nrow(SWITCH.HITTERS[bat_hand == "Left/Right"]) == 381L)
SWITCH.HITTERS[, 
  bat_hand := fifelse(bat_hand %in% c("Unknown", "Left/Right"), collapse::fmode(bat_hand, ties = "last"), bat_hand),
  by = .(batter)
]

# Confirm all batter hands are left or right at this point
stopifnot(all(SWITCH.HITTERS[, bat_hand] %in% c("Left", "Right")))

# Merge these hands onto our original data set
ALL.PLAYS[SWITCH.HITTERS, on = c(batter = "batter_id", pitcher = "pitcher_id"), bat_hand_reviewed := i.bat_hand]

# Re-assign bat hand values for switch hitters based on review
ALL.PLAYS[bat_hand == "B", bat_hand := substr(bat_hand_reviewed, 1L, 1L)]
stopifnot(all(ALL.PLAYS[, bat_hand] %in% c("L", "R")))

# Rename columns ----------------------------------------------------------------------------------------

# Rename event columns
event_cols <- readRDS(file.path("misc", "Rds", "event_cols.Rds"))
event_colnames <- readRDS(file.path("misc", "Rds", "event_colnames.Rds"))
setnames(ALL.PLAYS, event_cols, event_colnames)

# Impute missing runner advancements --------------------------------------------------------------------

# Impute the batter did not advance on unlikely runner advancement plays
adv0_cols <- sprintf("advance_0_to_%d", seq_len(4L))
ALL.PLAYS[
  balk | caught2 | caught2.doubleplay.strikeout | caught3 | caught3.doubleplay.strikeout | caught4 |
    caught4.doubleplay.strikeout | doubleplay.sacrificebunt | doubleplay.sacrificefly | pickoff1 | pickoff2 |
    pickoff3 | sacrificebunt | sacrificefly | strikeout,
  (adv0_cols) := lapply(.SD, function(x) fifelse(is.na(x), FALSE, x)),
  .SDcols = (adv0_cols)
]

# Save out ----------------------------------------------------------------------------------------------

fwrite(ALL.PLAYS, file.path("processed", "all_plays.csv"))
