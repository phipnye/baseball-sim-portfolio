# Partition pickoffs and strikeouts on the same play ----------------------------------------------------

# There are two instances of a strikeout and pickoff on the same play
stopifnot(nrow(DT[strikeout & (pickoff_base_1 | pickoff_base_2 | pickoff_base_3)]) == 2L)

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

# Manually update runner advancements -------------------------------------------------------------------

# There was one caught_stealing_3_double_play_strikeout for game PIT202305080
stopifnot(nrow(DT[game_id == "PIT202305080" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "PIT202305080" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for CHA202305310
stopifnot(nrow(DT[game_id == "CHA202305310" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "CHA202305310" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for SFN202308160
stopifnot(nrow(DT[game_id == "SFN202308160" & caught_stealing_3 & double_play & strikeout]) == 1L)
DT[game_id == "SFN202308160" & caught_stealing_3 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# There was one caught_stealing_4_double_play_strikeout for game ANA202305240
stopifnot(nrow(DT[game_id == "ANA202305240" & caught_stealing_4 & double_play & strikeout]) == 1L)
DT[game_id == "ANA202305240" & caught_stealing_4 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]

# And one for MIL202307240
stopifnot(nrow(DT[game_id == "MIL202307240" & caught_stealing_4 & double_play & strikeout]) == 1L)
DT[game_id == "MIL202307240" & caught_stealing_4 & double_play & strikeout, `:=`(
  advance_1_to_2 = FALSE,
  advance_1_to_3 = FALSE
)]
