# Partition pickoffs and strikeouts on the same play ----------------------------------------------------

# There is one instance of a strikeout and pickoff on the same play
stopifnot(nrow(DT[strikeout & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]) == 1L)

# There is also one instance of a walk and a pickoff on the same play
stopifnot(nrow(DT[walk & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]) == 1L)

# Add a row id to be able to add sort back in order
nchar.id <- nchar(as.character(nrow(DT))) + 1L
DT[, row_id := sprintf(paste0("%0", nchar.id, "d"), .I)]
rm(nchar.id)

# Subset to the rows at issue and remove them from the original data.table
TEMP.DT <- DT[(strikeout | walk) & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]
DT <- DT[!((strikeout | walk) & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3))]

# Duplicate the rows of the records that need to be partitioned out
TEMP.DT <- rbindlist(list(TEMP.DT, TEMP.DT), use.names = TRUE)

# Append suffixes to the row id
TEMP.DT[, row_id := sprintf("%s_%d", row_id, seq_len(.N)), by = .(row_id)]

# Re-sort the data
setorder(TEMP.DT, row_id)

# Pickoffs are odd rows
TEMP.DT[rep_len(c(TRUE, FALSE), nrow(TEMP.DT)), `:=`(
  walk = FALSE,
  strikeout = FALSE,
  double_play = FALSE,
  outs_post = outs_post - 1L
)]

# Strikeouts are even rows
TEMP.DT[rep_len(c(FALSE, TRUE), nrow(TEMP.DT)), `:=`(
  pickoff_base_1 = FALSE,
  pickoff_base_2 = FALSE,
  pickoff_base_3 = FALSE,
  out_1 = FALSE,
  out_2 = FALSE,
  out_3 = FALSE,
  double_play = FALSE,
  outs_pre = outs_pre + 1L,
  player_on_1st = fifelse(pickoff_base_1, FALSE, player_on_1st),
  player_on_2nd = fifelse(pickoff_base_1, FALSE, player_on_2nd),
  player_on_3rd = fifelse(pickoff_base_1, FALSE, player_on_3rd)
)]

# Re-add the rows back to the original data.table
DT <- rbindlist(list(DT, TEMP.DT), use.names = TRUE)
rm(TEMP.DT)

# Re-sort and remove the row id variable
setorder(DT, row_id)
DT[, row_id := NULL]

# Manually update runner advancements -------------------------------------------------------------------

# There was one doubleplay-strikeout for which we need runner advancement information since it's such a rare
# play
stopifnot(nrow(DT[game_id == "TOR202208290" & double_play & strikeout]) == 1L)
DT[game_id == "TOR202208290" & double_play & strikeout, advance_2_to_3 := FALSE]

# There was one caught_stealing_3_double_play_strikeout for game SEA202208050
stopifnot(nrow(DT[game_id == "SEA202208050" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "SEA202208050" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for CHN202208080
stopifnot(nrow(DT[game_id == "CHN202208080" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "CHN202208080" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for CHN202208100
stopifnot(nrow(DT[game_id == "CHN202208100" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "CHN202208100" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# There was one caught_stealing_4_double_play_strikeout for game PHI202208201
stopifnot(nrow(DT[game_id == "PHI202208201" & caught_stealing_4 & double_play & strikeout]) == 1L)
DT[game_id == "PHI202208201" & caught_stealing_4 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for game CHN202208250
stopifnot(nrow(DT[game_id == "CHN202208250" & caught_stealing_4 & double_play & strikeout]) == 1L)
DT[game_id == "CHN202208250" & caught_stealing_4 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# The were pickoffs at second with bases loaded for which we need runner advancement information
stopifnot(nrow(DT[pickoff_base_2 & player_on_1st & player_on_2nd & player_on_3rd]) == 3L)
DT[pickoff_base_2 & player_on_1st & player_on_2nd & player_on_3rd, `:=`(
  advance_0_to_1 = FALSE,
  advance_0_to_2 = FALSE,
  advance_0_to_3 = FALSE,
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]
