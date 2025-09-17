# Partition pickoffs and strikeouts on the same play ----------------------------------------------------

# There is one instance of a strikeout and pickoff on the same play
stopifnot(nrow(DT[strikeout & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]) == 4L)

# Add a row id to be able to add sort back in order
nchar.id <- nchar(as.character(nrow(DT))) + 1L
DT[, row_id := sprintf(paste0("%0", nchar.id, "d"), .I)]
rm(nchar.id)

# Subset to the rows at issue and remove them from the original data.table
TEMP.DT <- DT[strikeout & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]
DT <- DT[!(strikeout & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3))]

# Duplicate the rows of the records that need to be partitioned out
TEMP.DT <- rbindlist(list(TEMP.DT, TEMP.DT), use.names = TRUE)

# Append suffixes to the row id
TEMP.DT[, row_id := sprintf("%s_%d", row_id, seq_len(.N)), by = .(row_id)]

# Re-sort the data
setorder(TEMP.DT, row_id)

# Pickoffs are odd rows
TEMP.DT[rep_len(c(TRUE, FALSE), nrow(TEMP.DT)), `:=`(
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

# There is one play where Ohtani was walked and the runner on 2nd advanced to third on fielder's indifference
# Since this event is so rare, manually update this to be a walk
stopifnot(nrow(DT[defensive_indifference & walk]) == 1L)
DT[defensive_indifference & walk, c("defensive_indifference", "advance_2_to_3") := FALSE]

# Manually update runner advancements -------------------------------------------------------------------

# There was one caught_stealing_3_double_play_strikeout for game CHN202104160
stopifnot(nrow(DT[game_id == "CHN202104160" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "CHN202104160" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for game CHN202104250
stopifnot(nrow(DT[game_id == "CHN202104250" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "CHN202104250" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for game KCA202109150
stopifnot(nrow(DT[game_id == "KCA202109150" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "KCA202109150" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for game DET202109270
stopifnot(nrow(DT[game_id == "DET202109270" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "DET202109270" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]
