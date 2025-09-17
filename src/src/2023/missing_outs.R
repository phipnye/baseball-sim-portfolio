# Import data sets
MISS.OUTS <- DT[is.na(out_1) | is.na(out_2) | is.na(out_3)]
LAST.PLAYS <- fread(file.path("raw", "2023", "last_inning_plays.csv"))

# Merge data sets to see which records we need to review manually
MERGED.DT <- merge.data.table(
  MISS.OUTS,
  LAST.PLAYS,
  by = c("game_id", "inning", "at_home", "outs_post"),
  all.x = TRUE,
  sort = FALSE,
  allow.cartesian = FALSE
)

# Subset to desired columns
MERGED.DT <- MERGED.DT[, .(game_id, inning, at_home, outs_post, play_description, player_on_1st, player_on_2nd, player_on_3rd, site)]

# Confirm no missing descriptions
stopifnot(!anyNA(MERGED.DT[, play_description]))

# Make sure the descriptions end with periods
MERGED.DT[str_detect(play_description, "\\.$", TRUE), play_description := paste0(play_description, ".")]

### Fill in those that don't need to be manually reviewed

# Handle simple outs (strikeouts, popouts, flyouts, groundouts, lineouts, foul outs)
out_patterns <- c(
  struck_out = "struck out (looking|swinging)\\.$",
  popout = "popped out to (first|second|third|catcher|pitcher|shortstop|left|center|right)\\.$",
  flyout = "flied out to (first|second|third|catcher|pitcher|shortstop|left|center|right)\\.$",
  groundout = "grounded out to (first|second|third|catcher|pitcher|shortstop|left|center|right)\\.$",
  lineout = "lined out to (first|second|third|catcher|pitcher|shortstop|left|center|right)\\.$",
  foulout = "fouled out to (first|second|third|catcher|pitcher|shortstop|left|center|right)\\.$"
)

for (out_pattern in out_patterns) {
  MERGED.DT[str_detect(play_description, out_pattern), sprintf("out_%d", 0L:3L) := .(TRUE, FALSE, FALSE, FALSE)]
}

# Handle fielder's choice (or pitcher changes) with one base runner - the base runner has to be out
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 1L) &
    str_detect(play_description, "(fielder's choice|pitches to)"),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    player_on_1st,
    player_on_2nd,
    player_on_3rd
  )
]

# Handle double plays with one base runner - both batter and runner have to be out
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 1L) &
    str_detect(play_description, "double play"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    player_on_1st,
    player_on_2nd,
    player_on_3rd
  )
]

# With runners on first and second, the player at first is forced to go to second
MERGED.DT[
  is.na(out_0) & player_on_1st & player_on_2nd & str_detect(play_description, "out at second") &
    str_detect(play_description, "(double play|scored)", TRUE),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    TRUE,
    FALSE,
    FALSE
  )
]

# With runners on first and second, the player at second is forced to go to third
MERGED.DT[
  is.na(out_0) & player_on_1st & player_on_2nd & str_detect(play_description, "out at third") &
    str_detect(play_description, "(double play|scored)", TRUE),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    FALSE,
    TRUE,
    FALSE
  )
]

# Lined into plays followed with doubled off second directly infer who's out
MERGED.DT[
  is.na(out_0) & str_detect(play_description, "lined into double play") &
    str_detect(play_description, "doubled off second"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    FALSE,
    TRUE,
    FALSE
  )
]

# Lined into plays followed with doubled off third directly infer who's out
MERGED.DT[
  is.na(out_0) & str_detect(play_description, "lined into double play") &
    str_detect(play_description, "doubled off third"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    FALSE,
    FALSE,
    TRUE
  )
]

# Impute that lead runner scores while other runner is out - if the batter's name only appears at the
# beginning of the string but "out at" appears elsewhere, we know the base runner is out
MERGED.DT[, batter := str_extract(play_description, "^([A-Z]\\.\\s\\S+|\\S+)")]
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 1L) & 
    str_count(play_description, batter) == 1L &
    str_detect(play_description, "out at"),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    player_on_1st,
    player_on_2nd,
    player_on_3rd
  )
]

# With only one player on, if that player scores, then the out must come from the batter
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 1L) &
    str_count(play_description, batter) == 2L &
    str_detect(play_description, paste0(sprintf("(?<!%s)", batter), " scored")) &
    str_detect(play_description, "out"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    FALSE,
    FALSE,
    FALSE
  )
]

# With two players on, the lead runner scores while the other player is ruled out on the play. The batter
# could not have been ruled out since the name of the batter only appears at the beginning of the string
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 2L) &
    str_count(play_description, batter) == 1L & str_detect(play_description, "scored") &
    str_detect(play_description, "out at") &
    str_detect(play_description, paste0(batter, " (singled|doubled|tripled|fielder's choice)")),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    player_on_1st,
    player_on_2nd & !player_on_1st,
    FALSE
  )
]

# Excluding triple or double plays - if only one player is out and it says that it's the batter, then we can
# infer the batter was the only player who got out
MERGED.DT[
  is.na(out_0) & (player_on_1st + player_on_2nd + player_on_3rd == 1L) &
    str_detect(play_description, paste0(batter, " out")) &
    str_detect(play_description, "(double|triple) play", TRUE),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    FALSE,
    FALSE,
    FALSE
  )
]

# Grounds into double play with a player on first and "outs at second" infer the batter and runner on first
# are out on the play
MERGED.DT[
  is.na(out_0) & player_on_1st & str_detect(play_description, "grounded into double play") &
    str_detect(play_description, "out at second"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    TRUE,
    FALSE,
    FALSE
  )
]

# Grounds into double play with a player on first and second and "outs at third" infer the batter and runner
# on first are out on the play
MERGED.DT[
  is.na(out_0) & player_on_1st & player_on_2nd & str_detect(play_description, "grounded into double play") &
    str_detect(play_description, "out at third"),
  sprintf("out_%d", 0L:3L) := .(
    TRUE,
    TRUE,
    FALSE,
    FALSE
  )
]

# Grounding into fielder's choice with an "out at second" and a player at first implies runner at first is out
MERGED.DT[
  is.na(out_0) & player_on_1st &
    str_detect(play_description, "grounded into fielder's choice to (first|second|third|shortstop), .+ out at second\\.$"),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    TRUE,
    FALSE,
    FALSE
  )
]

# Grounding into fielder's choice with an "out at third" and a player at first and second implies runner at
# second is out
MERGED.DT[
  is.na(out_0) & player_on_1st & player_on_2nd &
    str_detect(play_description, "grounded into fielder's choice to (first|second|third|shortstop), .+ out at third\\.$"),
  sprintf("out_%d", 0L:3L) := .(
    FALSE,
    FALSE,
    TRUE,
    FALSE
  )
]

# Drop the batter variable
set(MERGED.DT, j = "batter", value = NULL)

# Export the data
fwrite(MERGED.DT, file.path("raw", "2023", "matched_last_inning_plays.csv"))
