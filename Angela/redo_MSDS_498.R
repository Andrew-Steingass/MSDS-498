# -----------------------------
# 0) Libraries
# -----------------------------
library(dplyr)
library(stringr)
library(lubridate)
library(stringdist)
library(readr)
library(tidyr)
library(ggplot2)
library(showtext)
library(sysfonts)
library(pROC)
library(survival)
library(tibble)
library(e1071)
library(scales)
library(broom)
library(rpart)
library(rpart.plot)

# -----------------------------
# 1) Load data
# -----------------------------
setwd("C:\\Users\\Owner\\OneDrive\\Desktop\\MSDS 498\\Datasets")

Master   <- read.csv("Master.csv", stringsAsFactors = FALSE)
injuries <- read.csv("injuries.csv", stringsAsFactors = FALSE)
Fielding <- read.csv("Fielding.csv", stringsAsFactors = FALSE)
TurfTeams <- read.csv("TeamsWithTurf.csv", stringsAsFactors = FALSE)
Pitching <- read.csv("Pitching.csv", stringsAsFactors = FALSE)
PitchingPost <- read.csv("PitchingPost.csv", stringsAsFactors = FALSE)
Batting <- read.csv("Batting.csv", stringsAsFactors = FALSE)

# -----------------------------
# 2) Clean injuries: create Name, drop columns, clean punctuation
# -----------------------------
injuries <- injuries %>%
  mutate(
    Name = Relinquished,
    Name = str_replace_all(Name, "[^[:alnum:][:space:]]+", ""),
    Name = str_squish(Name)
  ) %>%
  select(-Acquired, -Relinquished)

# -----------------------------
# 3) Build Master Name + restrict to "youngMaster"
# -----------------------------
Master <- Master %>%
  mutate(
    Name = str_squish(paste(nameFirst, nameLast)),
    finalGame = as.Date(finalGame)
  )

youngMaster <- Master %>%
  filter(finalGame >= as.Date("1999-01-01"))

# -----------------------------
# 4) Handle duplicate Names in youngMaster (remove ambiguous)
# -----------------------------
dup_names <- youngMaster %>%
  count(Name) %>%
  filter(n > 1) %>%
  pull(Name)

final_injuries <- injuries %>%
  filter(!Name %in% dup_names) %>%
  mutate(Name = str_squish(Name))

# ============================================================
# 5) TWO lookup tables + TWO exact-match passes (FIXED)
# ============================================================

# Lookup #1: nameFirst + nameLast
lookup_first_last <- Master %>%
  mutate(Name_FL = str_squish(paste(nameFirst, nameLast))) %>%
  select(Name_FL, playerID) %>%
  filter(!is.na(playerID), Name_FL != "", !Name_FL %in% dup_names) %>%
  distinct(Name_FL, .keep_all = TRUE) %>%
  rename(playerID_FL = playerID)

# Lookup #2: nameGiven + nameLast -> first+last word
lookup_given_last_squish <- Master %>%
  mutate(
    Name_GL = str_squish(paste(nameGiven, nameLast)),
    Name_GL = str_squish(paste(word(Name_GL, 1), word(Name_GL, -1)))
  ) %>%
  select(Name_GL, playerID) %>%
  filter(!is.na(playerID), Name_GL != "") %>%
  distinct(Name_GL, .keep_all = TRUE) %>%
  rename(playerID_GL = playerID)

# Ensure final_injuries has a playerID column to fill
final_injuries <- final_injuries %>%
  mutate(playerID = as.character(NA))

# Pass 1: exact match using lookup #1
final_injuries <- final_injuries %>%
  left_join(lookup_first_last, by = c("Name" = "Name_FL")) %>%
  mutate(playerID = if_else(is.na(playerID), as.character(playerID_FL), playerID)) %>%
  select(-playerID_FL)

# Pass 2: exact match using lookup #2 (for remaining NAs)
final_injuries <- final_injuries %>%
  left_join(lookup_given_last_squish, by = c("Name" = "Name_GL")) %>%
  mutate(playerID = if_else(is.na(playerID), as.character(playerID_GL), playerID)) %>%
  select(-playerID_GL)

cat("Missing playerID after 2-pass exact:", sum(is.na(final_injuries$playerID)), "\n")

# Create name_lookup for fuzzy (NOW it exists)
name_lookup <- youngMaster %>%
  select(Name, playerID) %>%
  distinct(Name, .keep_all = TRUE)

# -----------------------------
# 6) Fuzzy match remaining NAs using Jaro-Winkler
# -----------------------------
to_match <- final_injuries %>% filter(is.na(playerID))

if (nrow(to_match) > 0) {
  dist_matrix <- stringdistmatrix(
    to_match$Name,
    name_lookup$Name,
    method = "jw"
  )
  
  best_match <- apply(dist_matrix, 1, function(row) {
    best_i <- which.min(row)
    best_d <- row[best_i]
    if (best_d < 0.22) best_i else NA_integer_
  })
  
  matched_ids <- name_lookup$playerID[best_match]
  final_injuries$playerID[is.na(final_injuries$playerID)] <- matched_ids
}

cat("Missing playerID after fuzzy match:", sum(is.na(final_injuries$playerID)), "\n")

# -----------------------------
# 7) Cleanup remaining NAs: drop jr/sr + trailing initial, then try exact again
# -----------------------------
final_injuries <- final_injuries %>%
  mutate(
    Name = if_else(
      is.na(playerID),
      Name %>%
        str_squish() %>%
        str_remove(regex(",?\\s*(jr|sr)\\.?\\s*$", ignore_case = TRUE)) %>%
        str_remove(regex(",?\\s*(ii|iii|iv|v|vi|vii|viii|ix|x)\\s*$", ignore_case = TRUE)) %>%
        str_remove(regex("\\s+[A-Z]\\.?\\s*$")) %>%
        str_squish(),
      Name
    )
  )

# Condense to first+last word for remaining NAs
final_injuries <- final_injuries %>%
  mutate(
    Name = if_else(
      is.na(playerID),
      str_squish(paste(word(Name, 1), word(Name, -1))),
      Name
    )
  )

# Re-try exact match using BOTH lookups again (safe fill)
final_injuries <- final_injuries %>%
  left_join(lookup_first_last, by = c("Name" = "Name_FL")) %>%
  mutate(playerID = if_else(is.na(playerID), as.character(playerID_FL), playerID)) %>%
  select(-playerID_FL)

final_injuries <- final_injuries %>%
  left_join(lookup_given_last_squish, by = c("Name" = "Name_GL")) %>%
  mutate(playerID = if_else(is.na(playerID), as.character(playerID_GL), playerID)) %>%
  select(-playerID_GL)

cat("Missing playerID after suffix/initial cleanup + exact:", sum(is.na(final_injuries$playerID)), "\n")

# -----------------------------
# 8) Birthdate table from youngMaster, then join by playerID
# -----------------------------
young_birth <- youngMaster %>%
  mutate(birthDate = make_date(birthYear, birthMonth, birthDay)) %>%
  select(playerID, birthDate) %>%
  filter(!is.na(playerID)) %>%
  group_by(playerID) %>%
  summarise(birthDate = first(na.omit(birthDate)), .groups = "drop") %>%
  mutate(playerID = as.character(playerID))

final_injuries <- final_injuries %>%
  mutate(
    playerID = as.character(playerID),
    Date = as.Date(Date)
  ) %>%
  left_join(young_birth, by = "playerID")

final_injuries <- final_injuries %>%
  mutate(injury_age = time_length(interval(birthDate, Date), "month"))

#Pitching table prep

Pitching_clean <- Pitching %>%
  rename(
    player_id = playerID,
    season_year = yearID,
    season_stint = stint,
    team_id = teamID,
    league_id = lgID,
    
    wins = W,
    losses = L,
    games_pitched = G,
    games_started = GS,
    complete_games = CG,
    shutouts = SHO,
    saves = SV,
    games_finished = GF,
    
    outs_recorded = IPouts,
    hits_allowed = H,
    earned_runs_allowed = ER,
    home_runs_allowed = HR,
    walks_allowed = BB,
    strikeouts = SO,
    opponent_batting_average = BAOpp,
    earned_run_average = ERA,
    intentional_walks = IBB,
    wild_pitches = WP,
    batters_hit_by_pitch = HBP,
    balks = BK,
    batters_faced = BFP,
    total_runs_allowed = R,
    sacrifice_hits_allowed = SH,
    sacrifice_flies_allowed = SF,
    grounded_into_double_play = GIDP
  ) %>%
  mutate(
    innings_pitched = outs_recorded / 3
  )

Pitching_workload <- Pitching_clean %>%
  group_by(player_id, season_year) %>%
  summarise(
    total_innings_pitched = sum(innings_pitched, na.rm = TRUE),
    total_games_started = sum(games_started, na.rm = TRUE),
    total_games_pitched = sum(games_pitched, na.rm = TRUE),
    total_batters_faced = sum(batters_faced, na.rm = TRUE),
    .groups = "drop"
  )

#add previous year workload to pitchers ONLY

final_injuries <- final_injuries %>%
  mutate(
    playerID   = as.character(playerID),
    Date       = as.Date(Date),
    injuryYear = year(Date),
    priorYear  = injuryYear - 1
  )

pitching_workload_prior <- Pitching_clean %>%
  mutate(
    player_id = as.character(player_id),
    season_year = as.integer(season_year)
  ) %>%
  group_by(player_id, season_year) %>%
  summarise(
    prior_innings_pitched = sum(innings_pitched, na.rm = TRUE),
    prior_games_pitched   = sum(games_pitched, na.rm = TRUE),
    prior_games_started   = sum(games_started, na.rm = TRUE),
    prior_batters_faced   = sum(batters_faced, na.rm = TRUE),
    .groups = "drop"
  )

#join with final injuries

final_injuries <- final_injuries %>%
  left_join(
    pitching_workload_prior,
    by = c("playerID" = "player_id", "priorYear" = "season_year")
  )

#fill in non pitchers with zeros

final_injuries <- final_injuries %>%
  mutate(
    across(starts_with("prior_"), ~replace_na(., 0))
  )

#add post season inning pitched for pitchers - as a proxy for postseason workload

pitching_post_prior <- PitchingPost %>%
  mutate(
    playerID = as.character(playerID),
    yearID = as.integer(yearID),
    postseason_innings_pitched = IPouts / 3
  ) %>%
  group_by(playerID, yearID) %>%
  summarise(
    prior_postseason_innings_pitched =
      sum(postseason_innings_pitched, na.rm = TRUE),
    .groups = "drop"
  )

# Join to injuries (1999 injury -> 1998 postseason workload)
final_injuries <- final_injuries %>%
  left_join(
    pitching_post_prior,
    by = c("playerID" = "playerID", "priorYear" = "yearID")
  ) %>%
  mutate(
    prior_postseason_innings_pitched =
      replace_na(prior_postseason_innings_pitched, 0)
  )


#add fielding stats to injury table for matching playerIDs
# 1) Ensure injuryYear/priorYear exist
final_injuries <- final_injuries %>%
  mutate(
    playerID = as.character(playerID),
    Date = as.Date(Date),
    injuryYear = year(Date),
    priorYear = injuryYear - 1
  )

# 2) Prior-year defensive innings from Fielding
fielding_def_innings_prior <- Fielding %>%
  mutate(
    playerID = as.character(playerID),
    yearID = as.integer(yearID),
    defensive_innings = InnOuts / 3
  ) %>%
  group_by(playerID, yearID) %>%
  summarise(
    prior_defensive_innings = sum(defensive_innings, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Join to final_injuries: injury in 1999 gets 1998 defensive innings
final_injuries <- final_injuries %>%
  left_join(
    fielding_def_innings_prior,
    by = c("playerID" = "playerID", "priorYear" = "yearID")
  ) %>%
  mutate(
    prior_defensive_innings = replace_na(prior_defensive_innings, 0)
  )

#add batting stats to injury table for prior year

Batting_clean <- Batting %>%
  rename(
    player_id = playerID,
    season_year = yearID,
    season_stint = stint,
    team_id = teamID,
    league_id = lgID,
    
    games_played = G,
    at_bats = AB,
    runs_scored = R,
    hits = H,
    doubles = X2B,
    triples = X3B,
    home_runs = HR,
    runs_batted_in = RBI,
    stolen_bases = SB,
    caught_stealing = CS,
    walks = BB,
    strikeouts = SO,
    intentional_walks = IBB,
    hit_by_pitch = HBP,
    sacrifice_hits = SH,
    sacrifice_flies = SF,
    grounded_into_double_play = GIDP
  ) %>%
  mutate(
    plate_appearances =
      at_bats + walks + hit_by_pitch + sacrifice_flies + sacrifice_hits,
    stolen_base_attempts = stolen_bases + caught_stealing
  )

batting_workload_prior <- Batting_clean %>%
  mutate(
    player_id = as.character(player_id),
    season_year = as.integer(season_year)
  ) %>%
  group_by(player_id, season_year) %>%
  summarise(
    prior_plate_appearances = sum(plate_appearances, na.rm = TRUE),
    prior_games_batting = sum(games_played, na.rm = TRUE),
    prior_stolen_base_attempts = sum(stolen_base_attempts, na.rm = TRUE),
    prior_hit_by_pitch = sum(hit_by_pitch, na.rm = TRUE),
    .groups = "drop"
  )

final_injuries <- final_injuries %>%
  mutate(playerID = as.character(playerID)) %>%
  left_join(
    batting_workload_prior,
    by = c("playerID" = "player_id", "priorYear" = "season_year")
  ) %>%
  mutate(
    prior_plate_appearances = replace_na(prior_plate_appearances, 0),
    prior_games_batting = replace_na(prior_games_batting, 0),
    prior_stolen_base_attempts = replace_na(prior_stolen_base_attempts, 0),
    prior_hit_by_pitch = replace_na(prior_hit_by_pitch, 0)
  )

#calculate total workload for non pitchers

final_injuries <- final_injuries %>%
  mutate(
    total_prior_position_workload =
      prior_defensive_innings +
      (prior_plate_appearances / 4)  # scaled so PA doesn't dominate
  )

#add debut year to injury dataset

debut_lookup <- Master %>%
  mutate(
    playerID = as.character(playerID),
    debut = as.Date(debut),
    debut_year = year(debut)
  ) %>%
  select(playerID, debut_year) %>%
  distinct(playerID, .keep_all = TRUE)

final_injuries <- final_injuries %>%
  mutate(playerID = as.character(playerID)) %>%
  left_join(debut_lookup, by = "playerID")

#calculate years experience at injury

final_injuries <- final_injuries %>%
  mutate(
    injuryYear = year(as.Date(Date)),
    years_experience = injuryYear - debut_year
  )

# -----------------------------
# 9) Add position from Fielding
# -----------------------------

fielding_pos <- Fielding %>%
  mutate(playerID = as.character(playerID)) %>%
  filter(!is.na(playerID), !is.na(POS)) %>%
  count(playerID, POS, name = "n") %>%
  group_by(playerID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(playerID, POS)

#replace position code with position text

# Create position code → full name mapping
pos_lookup <- tibble(
  POS = c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "OF", "DH"),
  POS_full = c(
    "Pitcher",
    "Catcher",
    "First Base",
    "Second Base",
    "Third Base",
    "Shortstop",
    "Left Field",
    "Center Field",
    "Right Field",
    "Outfielder",
    "Designated Hitter"
  )
)

fielding_pos <- Fielding %>%
  mutate(playerID = as.character(playerID)) %>%
  filter(!is.na(playerID), !is.na(POS)) %>%
  count(playerID, POS, name = "n") %>%
  group_by(playerID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  left_join(pos_lookup, by = "POS") %>%
  select(playerID, POS = POS_full)

final_injuries <- final_injuries %>%
  left_join(fielding_pos, by = "playerID")

youngMaster <- youngMaster %>%
  mutate(playerID = as.character(playerID)) %>%
  left_join(fielding_pos, by = "playerID")

#add Turfflag to injury dataset

# Expecting first column is Year (or "year"); others are team names (some cells blank)
turf_long <- TurfTeams %>%
  rename(Year = 1) %>%                      # assumes Year is first column
  pivot_longer(
    cols = -Year,
    names_to = "col",
    values_to = "Team"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Team = str_squish(as.character(Team))
  ) %>%
  filter(!is.na(Team), Team != "") %>%
  distinct(Year, Team) %>%
  mutate(TurfFlag = "Turf")

final_injuries <- final_injuries %>%
  mutate(
    injuryYear = lubridate::year(as.Date(Date)),
    Team_clean = str_squish(as.character(Team))
  ) %>%
  left_join(
    turf_long %>% rename(Team_clean = Team),
    by = c("injuryYear" = "Year", "Team_clean" = "Team_clean")
  ) %>%
  mutate(
    TurfFlag = if_else(is.na(TurfFlag), "Grass", TurfFlag)
  ) %>%
  select(-Team_clean)


# -----------------------------
# 10) Quick checks + export
# -----------------------------
final_injuries %>%
  summarise(
    total_rows = n(),
    missing_playerID = sum(is.na(playerID)),
    pct_missing = round(mean(is.na(playerID)) * 100, 2),
    missing_birthDate = sum(is.na(birthDate)),
    missing_POS = sum(is.na(POS))
  ) %>%
  print()

write.csv(final_injuries, "final_injuries.csv", row.names = FALSE)

# Print a sample of remaining unmatched
final_injuries %>%
  filter(is.na(playerID)) %>%
  select(Name, Date) %>%
  head(20)

#graph style
# Ensure Urbanist is loaded
sysfonts::font_add_google("Urbanist", "urbanist")
showtext_auto()

theme_capstone <- function() {
  theme_minimal(base_family = "urbanist") +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank()
    )
}

primary_blue <- "#67bed9"


#initial look at data - injuries by year

final_injuries %>%
  count(injuryYear) %>%
  ggplot(aes(x = injuryYear, y = n)) +
  geom_line(color = primary_blue, size = 1.5) +
  geom_point(color = primary_blue, size = 3) +
  labs(
    title = "Total Injuries by Year",
    x = "Year",
    y = "Number of Injuries"
  ) +
  theme_capstone()

#injuries by posiiton
final_injuries %>%
  count(POS, sort = TRUE) %>%
  ggplot(aes(x = reorder(POS, n), y = n)) +
  geom_col(fill = primary_blue) +
  coord_flip() +
  labs(
    title = "Injuries by Position",
    x = "Position",
    y = "Injury Count"
  ) +
  theme_capstone()

#prior pitching workload for pitchers
final_injuries %>%
  filter(POS == "Pitcher") %>%
  ggplot(aes(x = prior_innings_pitched)) +
  geom_histogram(
    fill = primary_blue,
    color = "white",
    bins = 30
  ) +
  labs(
    title = "Prior Season Pitching Workload",
    x = "Total Prior Pitching Innings",
    y = "Frequency"
  ) +
  theme_capstone()

#injuries by defensive workload
final_injuries %>%
  ggplot(aes(x = POS, y = prior_defensive_innings)) +
  geom_boxplot(fill = primary_blue, alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Prior Defensive Innings by Position",
    x = "Position",
    y = "Defensive Innings"
  ) +
  theme_capstone()

#grass versus turf
final_injuries %>%
  count(TurfFlag) %>%
  ggplot(aes(x = TurfFlag, y = n)) +
  geom_col(fill = primary_blue) +
  labs(
    title = "Injuries on Turf vs Grass",
    x = "Surface Type",
    y = "Injury Count"
  ) +
  theme_capstone()

#years experience at injury
final_injuries %>%
  ggplot(aes(x = years_experience)) +
  geom_histogram(
    fill = primary_blue,
    color = "white",
    bins = 25
  ) +
  labs(
    title = "Years of Experience at Injury",
    x = "Years of Experience",
    y = "Frequency"
  ) +
  theme_capstone()

#age at injury
final_injuries %>%
  ggplot(aes(x = injury_age)) +
  geom_histogram(
    fill = primary_blue,
    color = "white",
    bins = 30
  ) +
  labs(
    title = "Age at Injury (Months)",
    x = "Age in Months",
    y = "Frequency"
  ) +
  theme_capstone()

#compare injured and non injured players
#build the data table 

# Make sure these are set
final_injuries <- final_injuries %>%
  mutate(
    playerID = as.character(playerID),
    Date = as.Date(Date),
    injuryYear = year(Date),
    DL_length = as.numeric(DL_length)
  )

injury_year_summary <- final_injuries %>%
  filter(!is.na(playerID), !is.na(injuryYear)) %>%
  group_by(playerID, yearID = injuryYear) %>%
  summarise(
    InjuryFlag_current_year = 1L,
    DL_days_current_year = sum(DL_length, na.rm = TRUE),  # sums 15/60/etc from your file
    InjuryEvents_current_year = n(),
    .groups = "drop"
  )

pitching_year <- Pitching_clean %>%
  transmute(
    playerID = as.character(player_id),
    yearID = as.integer(season_year),
    innings_pitched = innings_pitched,
    games_started = games_started,
    batters_faced = batters_faced
  ) %>%
  group_by(playerID, yearID) %>%
  summarise(
    innings_pitched = sum(innings_pitched, na.rm = TRUE),
    games_started = sum(games_started, na.rm = TRUE),
    batters_faced = sum(batters_faced, na.rm = TRUE),
    .groups = "drop"
  )

batting_year <- Batting_clean %>%
  transmute(
    playerID = as.character(player_id),
    yearID = as.integer(season_year),
    plate_appearances = plate_appearances,
    stolen_base_attempts = stolen_base_attempts,
    hit_by_pitch = hit_by_pitch
  ) %>%
  group_by(playerID, yearID) %>%
  summarise(
    plate_appearances = sum(plate_appearances, na.rm = TRUE),
    stolen_base_attempts = sum(stolen_base_attempts, na.rm = TRUE),
    hit_by_pitch = sum(hit_by_pitch, na.rm = TRUE),
    .groups = "drop"
  )

fielding_year <- Fielding %>%
  mutate(
    playerID = as.character(playerID),
    yearID = as.integer(yearID),
    defensive_innings = InnOuts / 3
  ) %>%
  group_by(playerID, yearID) %>%
  summarise(
    defensive_innings = sum(defensive_innings, na.rm = TRUE),
    .groups = "drop"
  )

player_year <- bind_rows(
  pitching_year %>% select(playerID, yearID),
  batting_year  %>% select(playerID, yearID),
  fielding_year %>% select(playerID, yearID),
  injury_year_summary %>% select(playerID, yearID)
) %>%
  distinct(playerID, yearID)

player_year <- player_year %>%
  mutate(yearID = as.integer(yearID)) %>%
  filter(yearID >= 1999, yearID <= 2016)

pitching_prior <- pitching_year %>%
  mutate(yearID = yearID + 1L) %>%
  rename(
    prior_innings_pitched = innings_pitched,
    prior_games_started   = games_started,
    prior_batters_faced   = batters_faced
  )

batting_prior <- batting_year %>%
  mutate(yearID = yearID + 1L) %>%
  rename(
    prior_plate_appearances    = plate_appearances,
    prior_stolen_base_attempts = stolen_base_attempts,
    prior_hit_by_pitch         = hit_by_pitch
  )

fielding_prior <- fielding_year %>%
  mutate(yearID = yearID + 1L) %>%
  rename(
    prior_defensive_innings = defensive_innings
  )

# -------------------------------------------------
# A) Build player_year_panel (if not already created)
# -------------------------------------------------
player_year_panel <- player_year %>%
  left_join(pitching_prior, by = c("playerID", "yearID")) %>%
  left_join(batting_prior,  by = c("playerID", "yearID")) %>%
  left_join(fielding_prior, by = c("playerID", "yearID")) %>%
  left_join(injury_year_summary, by = c("playerID", "yearID")) %>%
  mutate(
    InjuryFlag_current_year    = replace_na(InjuryFlag_current_year, 0L),
    DL_days_current_year       = replace_na(DL_days_current_year, 0),
    InjuryEvents_current_year  = replace_na(InjuryEvents_current_year, 0L)
  ) %>%
  mutate(
    across(starts_with("prior_"), ~replace_na(., 0))
  )

# -------------------------------------------------
# B) Add teamID + TurfFlag (Turf/Grass)
# Requires: Pitching, Batting, Fielding, turf_long, Teams.csv
# -------------------------------------------------

# 0) Clean turf_long into a consistent join table
turf_long_clean <- turf_long %>%
  mutate(
    Year = as.integer(Year),
    Team = str_squish(as.character(Team))
  ) %>%
  filter(!is.na(Year), !is.na(Team), Team != "") %>%
  distinct(Year, Team) %>%
  mutate(TurfFlag = "Turf")

# 1) Primary teamID per player-year (team with most games)
pitch_team <- Pitching %>%
  mutate(
    playerID = as.character(playerID),
    yearID   = as.integer(yearID),
    teamID   = str_squish(as.character(teamID))
  ) %>%
  group_by(playerID, yearID, teamID) %>%
  summarise(games = sum(G, na.rm = TRUE), .groups = "drop")

bat_team <- Batting %>%
  mutate(
    playerID = as.character(playerID),
    yearID   = as.integer(yearID),
    teamID   = str_squish(as.character(teamID))
  ) %>%
  group_by(playerID, yearID, teamID) %>%
  summarise(games = sum(G, na.rm = TRUE), .groups = "drop")

fld_team <- Fielding %>%
  mutate(
    playerID = as.character(playerID),
    yearID   = as.integer(yearID),
    teamID   = str_squish(as.character(teamID))
  ) %>%
  group_by(playerID, yearID, teamID) %>%
  summarise(games = sum(G, na.rm = TRUE), .groups = "drop")

player_year_team <- bind_rows(pitch_team, bat_team, fld_team) %>%
  group_by(playerID, yearID, teamID) %>%
  summarise(games = sum(games, na.rm = TRUE), .groups = "drop") %>%
  group_by(playerID, yearID) %>%
  slice_max(games, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(playerID, yearID, teamID)

player_year_team <- player_year_team %>%
  mutate(yearID = as.integer(yearID)) %>%
  filter(yearID >= 1999, yearID <= 2016)

# 2) Join teamID onto panel

player_year_panel <- player_year_panel %>%
  left_join(player_year_team, by = c("playerID", "yearID")) %>%
  mutate(
    teamID = str_squish(as.character(teamID))
  )

# -------------------------------------------------
# Replace nickname logic with year-specific teamID mapping
# TurfLong nicknames -> Lahman teamID codes -> join to player_year_panel
# -------------------------------------------------

#add TeamID to turf long
turf_long <- turf_long %>%
  mutate(
    Team = str_squish(as.character(Team)),
    teamID = case_when(
      Team == "Blue Jays"   ~ "TOR",
      Team == "Devil Rays"  ~ "TBA",
      Team == "Rays"        ~ "TBA",
      Team == "Twins"       ~ "MIN",
      Team == "Phillies"    ~ "PHI",
      Team == "Expos"       ~ "MON",
      Team == "Reds"        ~ "CIN",
      Team == "Pirates"     ~ "PIT",
      TRUE ~ NA_character_
    )
  )
turf_team_codes <- turf_long %>%
  mutate(
    yearID = as.integer(Year),
    teamID = str_squish(teamID)
  ) %>%
  filter(!is.na(teamID)) %>%
  distinct(yearID, teamID) %>%
  mutate(TurfFlag = "Turf")

player_year_panel <- player_year_panel %>%
  left_join(turf_team_codes, by = c("yearID", "teamID")) %>%
  mutate(
    TurfFlag = if_else(is.na(TurfFlag), "Grass", TurfFlag)
  )

#add other physical stats to player panel
player_year_panel <- player_year_panel %>%
  left_join(
    youngMaster %>%
      mutate(playerID = as.character(playerID)) %>%
      select(playerID, height, weight, POS, debut, finalGame),
    by = "playerID"
  ) %>%
  mutate(
    debut_year = year(as.Date(debut))
  )

# Gut check
table(player_year_panel$TurfFlag, useNA = "ifany")
player_year_panel %>%
  select(yearID, teamID, TurfFlag) %>%
  distinct() %>%
  arrange(desc(yearID), teamID) %>%
  head(30)

#injured versus non injured
#check for normalcy
vars_to_check <- c(
  "height",
  "weight",
  "prior_innings_pitched",
  "prior_plate_appearances",
  "prior_defensive_innings",
  "prior_stolen_base_attempts",
  "prior_hit_by_pitch",
  "DL_days_current_year"
)

#skew/krutosis

shape_stats <- player_year_panel %>%
  select(InjuryFlag_current_year, all_of(vars_to_check)) %>%
  pivot_longer(cols = all_of(vars_to_check), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(InjuryFlag_current_year, variable) %>%
  summarise(
    n = n(),
    mean = mean(value),
    median = median(value),
    sd = sd(value),
    skew = e1071::skewness(value),
    kurt = e1071::kurtosis(value),
    .groups = "drop"
  )

shape_stats

#right population comparison test
get_test <- function(df, var) {
  d <- df %>%
    select(InjuryFlag_current_year, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  x0 <- d %>% filter(InjuryFlag_current_year == 0) %>% pull(var)
  x1 <- d %>% filter(InjuryFlag_current_year == 1) %>% pull(var)
  
  # Shapiro only valid for 3..5000
  sh0 <- if (length(x0) >= 3 & length(x0) <= 5000) shapiro.test(x0)$p.value else NA_real_
  sh1 <- if (length(x1) >= 3 & length(x1) <= 5000) shapiro.test(x1)$p.value else NA_real_
  
  use_t <- !is.na(sh0) && !is.na(sh1) && sh0 > 0.05 && sh1 > 0.05
  
  if (use_t) {
    tt <- t.test(x1, x0)
    tibble(
      variable = var,
      test = "t-test",
      p_value = tt$p.value,
      estimate_diff = diff(tt$estimate)  # mean(x1) - mean(x0)
    )
  } else {
    wt <- wilcox.test(x1, x0)
    tibble(
      variable = var,
      test = "wilcoxon",
      p_value = wt$p.value,
      estimate_diff = median(x1, na.rm=TRUE) - median(x0, na.rm=TRUE)
    )
  }
}

test_results <- bind_rows(lapply(vars_to_check, \(v) get_test(player_year_panel, v)))
test_results


#HOLD - MAY NEED TO TRANSFORM
comparison_table <- player_year_panel %>%
  group_by(InjuryFlag_current_year) %>%
  summarise(
    n = n(),
    mean_height = mean(height, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    mean_prior_IP = mean(prior_innings_pitched, na.rm = TRUE),
    mean_prior_PA = mean(prior_plate_appearances, na.rm = TRUE),
    mean_prior_def_inn = mean(prior_defensive_innings, na.rm = TRUE),
    mean_prior_SB_attempts = mean(prior_stolen_base_attempts, na.rm = TRUE),
    pct_turf = mean(TurfFlag == "Turf", na.rm = TRUE) * 100
  )

comparison_table

t.test(prior_innings_pitched ~ InjuryFlag_current_year, data = player_year_panel)
t.test(prior_plate_appearances ~ InjuryFlag_current_year, data = player_year_panel)
t.test(prior_defensive_innings ~ InjuryFlag_current_year, data = player_year_panel)
t.test(weight ~ InjuryFlag_current_year, data = player_year_panel)

table_turf <- table(player_year_panel$TurfFlag,
                    player_year_panel$InjuryFlag_current_year)

chisq.test(table_turf)

#visualizations

theme_set(
  theme_minimal(base_family = "urbanist") +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text  = element_text(size = 12)
    )
)

ggplot(player_year_panel,
       aes(x = factor(InjuryFlag_current_year),
           y = prior_innings_pitched,
           fill = factor(InjuryFlag_current_year))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#67bed9", "#D12028")) +
  labs(
    title = "Prior Season Innings Pitched\nInjured vs Non-Injured",
    x = "Injury Flag (0 = No Injury, 1 = Injury)",
    y = "Prior Season Innings Pitched"
  ) +
  theme_capstone()

ggplot(player_year_panel,
       aes(x = factor(InjuryFlag_current_year),
           y = prior_plate_appearances,
           fill = factor(InjuryFlag_current_year))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#67bed9", "#D12028")) +
  labs(
    title = "Prior Season Plate Appearances\nInjured vs Non-Injured",
    x = "Injury Flag",
    y = "Prior Season Plate Appearances"
  ) +
  theme_capstone()

ggplot(player_year_panel,
       aes(x = factor(InjuryFlag_current_year),
           y = prior_defensive_innings,
           fill = factor(InjuryFlag_current_year))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#67bed9", "#D12028")) +
  labs(
    title = "Prior Defensive Innings\nInjured vs Non-Injured",
    x = "Injury Flag",
    y = "Prior Defensive Innings"
  ) +
  theme_capstone()

player_year_panel %>%
  group_by(TurfFlag) %>%
  summarise(injury_rate = mean(InjuryFlag_current_year)) %>%
  ggplot(aes(x = TurfFlag, y = injury_rate, fill = TurfFlag)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("#67bed9", "#D12028")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Injury Rate by Playing Surface",
    x = "Surface Type",
    y = "Injury Rate"
  ) +
  theme_capstone()

#proportion of grass/turf players injured

turf_injury_summary <- player_year_panel %>%
  group_by(TurfFlag) %>%
  summarise(
    total_player_years = n(),
    injured_years = sum(InjuryFlag_current_year == 1),
    injury_rate = injured_years / total_player_years,
    .groups = "drop"
  )

turf_injury_summary

turf_injury_summary <- turf_injury_summary %>%
  mutate(
    se = sqrt(injury_rate * (1 - injury_rate) / total_player_years),
    lower = injury_rate - 1.96 * se,
    upper = injury_rate + 1.96 * se
  )

ggplot(turf_injury_summary,
       aes(x = TurfFlag, y = injury_rate, fill = TurfFlag)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2) +
  scale_fill_manual(values = c("Grass" = "#67bed9",
                               "Turf"  = "#D12028")) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Injury Proportion by Playing Surface\n(95% CI)",
    x = "Playing Surface",
    y = "Proportion Injured"
  ) +
  theme_capstone()

#repeat with generalized posiitions

player_year_panel <- player_year_panel %>%
  mutate(
    PositionGroup = case_when(
      POS == "Pitcher"                ~ "Pitcher",
      POS == "Catcher"                     ~ "Catcher",
      POS %in% c("First Base","Second Base","Third Base","Shortstop") ~ "Infield",
      POS %in% c("Outfielder","Center Field","Left Field","Right Field") ~ "Outfield",
      TRUE                           ~ NA_character_
    )
  )
turf_pos_summary <- player_year_panel %>%
  filter(!is.na(PositionGroup)) %>%   # removes DH & Other
  group_by(PositionGroup, TurfFlag) %>%
  summarise(
    total = n(),
    injured = sum(InjuryFlag_current_year == 1),
    injury_rate = injured / total,
    .groups = "drop"
  )

turf_pos_summary <- turf_pos_summary %>%
  mutate(
    se = sqrt(injury_rate * (1 - injury_rate) / total),
    lower = injury_rate - 1.96 * se,
    upper = injury_rate + 1.96 * se
  )

ggplot(turf_pos_summary,
       aes(x = PositionGroup,
           y = injury_rate,
           fill = TurfFlag)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6,
           alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  scale_fill_manual(values = c("Grass" = "#67bed9",
                               "Turf"  = "#D12028")) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Injury Proportion by Surface and Position",
    x = "Position Group",
    y = "Proportion Injured",
    fill = "Surface"
  ) +
  theme_capstone()

#cox proportional hazard model - time to first injury

# Ensure needed fields exist
player_year_panel <- player_year_panel %>%
  mutate(
    playerID   = as.character(playerID),
    yearID     = as.integer(yearID),
    debut_year = as.integer(debut_year),
    career_age        = yearID - debut_year
  ) %>%
  filter(!is.na(career_age), career_age >= 0)

# Identify first injury year per player (NA if never injured)
first_injury <- player_year_panel %>%
  filter(InjuryFlag_current_year == 1) %>%
  group_by(playerID) %>%
  summarise(first_injury_year = min(yearID), .groups = "drop")

# Build survival_data WITHOUT dropping censored players
survival_data <- player_year_panel %>%
  left_join(first_injury, by = "playerID") %>%
  mutate(
    event = if_else(!is.na(first_injury_year) & yearID == first_injury_year, 1L, 0L)
  ) %>%
  # keep all years for never-injured players; keep years up to first injury for injured players
  filter(is.na(first_injury_year) | yearID <= first_injury_year) %>%
  mutate(
    career_age_start = career_age,
    career_age_stop  = career_age + 1
  )

survival_data <- survival_data %>%
  mutate(
    career_age_start = career_age,
    career_age_stop  = career_age + 1
  )
cox_model <- coxph(
  Surv(career_age_start, career_age_stop, event) ~
    log1p(prior_plate_appearances) +
    log1p(prior_innings_pitched) +
    log1p(prior_defensive_innings) +
    weight +
    TurfFlag +
    PositionGroup,
  data = survival_data
)

summary(cox_model)

#metrics
summary(cox_model)$concordance
anova(cox_model)
ph_test <- cox.zph(cox_model)
ph_test
plot(ph_test)

#curves
surv_fit <- survfit(cox_model,
                    newdata = data.frame(
                      prior_plate_appearances = median(survival_data$prior_plate_appearances, na.rm=TRUE),
                      prior_innings_pitched = median(survival_data$prior_innings_pitched, na.rm=TRUE),
                      prior_defensive_innings = median(survival_data$prior_defensive_innings, na.rm=TRUE),
                      weight = median(survival_data$weight, na.rm=TRUE),
                      TurfFlag = c("Grass","Turf"),
                      PositionGroup = "Pitcher"
                    ))

plot(surv_fit, col=c("#67bed9","#D12028"), lwd=2)
legend("bottomleft", legend=c("Grass","Turf"),
       col=c("#67bed9","#D12028"), lwd=2)

#cox model by position

# Quick gut check (should be TRUE now)
survival_data %>%
  summarise(
    total_players = n_distinct(playerID),
    total_events  = sum(event),
    total_rows    = n()
  ) %>%
  print()

survival_data <- survival_data %>%
  filter(PositionGroup %in% c("Pitcher","Catcher","Infield","Outfield"))

fit_cox_by_position <- function(pos_group) {
  
  df <- survival_data %>%
    filter(PositionGroup == pos_group)
  
  model <- coxph(
    Surv(career_age_start, career_age_stop, event) ~
      log1p(prior_plate_appearances) +
      log1p(prior_innings_pitched) +
      log1p(prior_defensive_innings) +
      weight +
      TurfFlag +
      cluster(playerID),
    data = df
  )
  
  tidy_model <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(PositionGroup = pos_group)
  
  model_metrics <- tibble(
    PositionGroup = pos_group,
    concordance = summary(model)$concordance[1],
    n_players = n_distinct(df$playerID),
    n_events = sum(df$event)
  )
  
  list(
    model = model,
    coefficients = tidy_model,
    metrics = model_metrics
  )
}

positions <- c("Pitcher","Catcher","Infield","Outfield")

results_list <- lapply(positions, fit_cox_by_position)

coef_table <- bind_rows(lapply(results_list, function(x) x$coefficients))
metrics_table <- bind_rows(lapply(results_list, function(x) x$metrics))

metrics_table

#logistical model

# Keep your analysis window (optional, if you want 1999–2016 only)
player_year_panel_model <- player_year_panel %>%
  mutate(
    playerID = as.character(playerID),
    yearID   = as.integer(yearID),
    debut_year = as.integer(debut_year),
    career_age = yearID - debut_year,
    InjuryFlag_current_year = as.integer(InjuryFlag_current_year),
    TurfFlag = factor(TurfFlag, levels = c("Grass","Turf")),
    PositionGroup = factor(PositionGroup, levels = c("Pitcher","Catcher","Infield","Outfield"))
  ) %>%
  filter(yearID >= 1999, yearID <= 2016) %>%  # keep if desired
  filter(!is.na(InjuryFlag_current_year), !is.na(career_age), career_age >= 0) %>%
  # remove DH/Other if still present
  filter(PositionGroup %in% c("Pitcher","Catcher","Infield","Outfield"))

# log1p transforms (handles zeros safely)
player_year_panel_model <- player_year_panel_model %>%
  mutate(
    log_prior_IP  = log1p(prior_innings_pitched),
    log_prior_PA  = log1p(prior_plate_appearances),
    log_prior_DEF = log1p(prior_defensive_innings),
    log_prior_BFP = log1p(prior_batters_faced),
    log_prior_GS = log1p(prior_games_started),
    log_prior_HBP = log1p(prior_hit_by_pitch),
    log_prior_SBA = log1p(prior_stolen_base_attempts)
  )

set.seed(123)

all_players <- unique(player_year_panel_model$playerID)
train_players <- sample(all_players, size = floor(0.7 * length(all_players)))

train_df <- player_year_panel_model %>% filter(playerID %in% train_players)
test_df  <- player_year_panel_model %>% filter(!playerID %in% train_players)

# Quick outcome rate check
train_rate <- mean(train_df$InjuryFlag_current_year == 1, na.rm = TRUE)
test_rate  <- mean(test_df$InjuryFlag_current_year == 1, na.rm = TRUE)
c(train_rate = train_rate, test_rate = test_rate)

# Simple PR AUC without extra packages (rank-based approximation)
pr_auc <- function(y_true, y_score) {
  o <- order(y_score, decreasing = TRUE)
  y <- y_true[o]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  precision <- tp / (tp + fp)
  recall <- tp / sum(y == 1)
  # trapezoid over recall
  idx <- which(!is.na(precision) & !is.na(recall))
  precision <- precision[idx]; recall <- recall[idx]
  sum(diff(c(0, recall)) * precision)
}

eval_binary <- function(y_true, y_prob, threshold = 0.25) {
  y_true <- as.integer(y_true)
  roc_obj <- pROC::roc(y_true, y_prob, quiet = TRUE)
  auc_roc <- as.numeric(pROC::auc(roc_obj))
  auc_pr  <- pr_auc(y_true, y_prob)
  brier   <- mean((y_prob - y_true)^2)
  
  y_pred <- as.integer(y_prob >= threshold)
  tp <- sum(y_pred == 1 & y_true == 1)
  tn <- sum(y_pred == 0 & y_true == 0)
  fp <- sum(y_pred == 1 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
  
  tibble::tibble(
    threshold = threshold,
    roc_auc = auc_roc,
    pr_auc = auc_pr,
    brier = brier,
    precision = precision,
    recall = recall,
    f1 = f1,
    tp = tp, fp = fp, tn = tn, fn = fn
  )
}

all_model <- glm(
  InjuryFlag_current_year ~
    career_age +
    weight +
    log_prior_PA +
    log_prior_IP +
    log_prior_DEF +
    TurfFlag +
    PositionGroup,
  data = train_df,
  family = binomial
)

summary(all_model)

test_prob <- predict(all_model, newdata = test_df, type = "response")

# Evaluate at a more sensible threshold than 0.5 for rare events:
# try 0.10 and 0.20, or choose threshold based on desired recall
metrics_all_10 <- eval_binary(test_df$InjuryFlag_current_year, test_prob, threshold = 0.10)
metrics_all_20 <- eval_binary(test_df$InjuryFlag_current_year, test_prob, threshold = 0.20)

metrics_all_10
metrics_all_20

calib_df <- test_df %>%
  mutate(p = test_prob,
         bin = dplyr::ntile(p, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_p = mean(p),
    obs_rate = mean(InjuryFlag_current_year),
    n = n(),
    .groups = "drop"
  )

ggplot(calib_df, aes(x = mean_p, y = obs_rate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Calibration (Deciles)", x = "Mean predicted risk", y = "Observed injury rate") +
  theme_capstone()

#model metrics visualization function
evaluate_model_visual <- function(data, prob_col, actual_col, threshold, title_text) {

  df <- data %>%
    mutate(
      pred_class = if_else(.data[[prob_col]] >= threshold, 1L, 0L)
    )
  
  # Confusion Matrix
  cm <- df %>%
    count(
      Actual = .data[[actual_col]],
      Predicted = pred_class
    )
  
  cm_plot <- ggplot(cm, aes(x = factor(Predicted),
                            y = factor(Actual),
                            fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), size = 6) +
    scale_fill_gradient(low = "#ffffff", high = "#67bed9") +
    labs(
      title = paste("Confusion Matrix –", title_text),
      x = "Predicted",
      y = "Actual"
    ) +
    theme_capstone()
  
  # ROC
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  
  roc_df <- data.frame(
    tpr = roc_obj$sensitivities,
    fpr = 1 - roc_obj$specificities
  )
  
  roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
    geom_line(size = 1.2, color = "#222c67") +
    geom_abline(linetype = "dashed", color = "#d12028") +
    labs(
      title = paste0("ROC Curve – ", title_text,
                     " (AUC = ", round(auc_val, 3), ")"),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_capstone()
  
  list(confusion_plot = cm_plot,
       roc_plot = roc_plot,
       auc = auc_val)
}

#apply to above model
results_all_log <- evaluate_model_visual(
  data = dplyr::mutate(test_df, test_prob = test_prob),
  prob_col = "test_prob",
  actual_col = "InjuryFlag_current_year",
  threshold = 0.25,  
  title_text = "All Players--Logistic"
)

results_all_log$confusion_plot
results_all_log$roc_plot

#logistic regression all with additional stats
set.seed(123)
all_players <- unique(player_year_panel_model$playerID)
train_players <- sample(all_players, size = floor(0.7 * length(all_players)))

train_df <- player_year_panel_model %>% filter(playerID %in% train_players)
test_df  <- player_year_panel_model %>% filter(!playerID %in% train_players)

pr_auc <- function(y_true, y_score) {
  o <- order(y_score, decreasing = TRUE)
  y <- y_true[o]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  precision <- tp / (tp + fp)
  recall <- tp / sum(y == 1)
  idx <- which(!is.na(precision) & !is.na(recall))
  precision <- precision[idx]; recall <- recall[idx]
  sum(diff(c(0, recall)) * precision)
}

eval_binary <- function(y_true, y_prob, threshold = 0.25) {
  y_true <- as.integer(y_true)
  roc_obj <- pROC::roc(y_true, y_prob, quiet = TRUE)
  auc_roc <- as.numeric(pROC::auc(roc_obj))
  auc_pr  <- pr_auc(y_true, y_prob)
  brier   <- mean((y_prob - y_true)^2)
  
  y_pred <- as.integer(y_prob >= threshold)
  tp <- sum(y_pred == 1 & y_true == 1)
  tn <- sum(y_pred == 0 & y_true == 0)
  fp <- sum(y_pred == 1 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
  
  tibble(
    threshold = threshold,
    roc_auc = auc_roc,
    pr_auc = auc_pr,
    brier = brier,
    precision = precision,
    recall = recall,
    f1 = f1,
    tp = tp, fp = fp, tn = tn, fn = fn
  )
}

all_model <- glm(
  InjuryFlag_current_year ~
    career_age +
    height + weight +
    TurfFlag +
    PositionGroup +
    log_prior_PA +
    log_prior_DEF +
    log_prior_IP +
    log_prior_BFP +
    log_prior_GS +
    log_prior_SBA +
    log_prior_HBP,
  data = train_df,
  family = binomial
)

test_prob_all <- predict(all_model, newdata = test_df, type = "response")
metrics_all <- eval_binary(test_df$InjuryFlag_current_year, test_prob_all, threshold = 0.25)

metrics_all

#apply to above model
results_all_log <- evaluate_model_visual(
  data = dplyr::mutate(test_df, test_prob = test_prob_all),
  prob_col = "test_prob",
  actual_col = "InjuryFlag_current_year",
  threshold = 0.25,  
  title_text = "All Players--Logistic--extra data"
)

results_all_log$confusion_plot
results_all_log$roc_plot

#different models, different positions
fit_logit_group <- function(group_name, train_df, test_df, threshold = 0.25) {
  
  tr <- train_df %>% filter(PositionGroup == group_name)
  te <- test_df  %>% filter(PositionGroup == group_name)
  
  if (nrow(tr) < 200 || sum(tr$InjuryFlag_current_year == 1) < 20) {
    return(list(group = group_name, model = NULL,
                metrics = tibble(PositionGroup = group_name, note = "Too few rows/events"),
                viz = NULL))
  }
  
  f <- switch(
    group_name,
    "Pitcher"  = InjuryFlag_current_year ~ career_age + height + weight + TurfFlag +
      log_prior_IP + log_prior_BFP + log_prior_GS,
    "Catcher"  = InjuryFlag_current_year ~ career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log_prior_HBP,
    "Infield"  = InjuryFlag_current_year ~ career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log_prior_SBA + log_prior_HBP,
    "Outfield" = InjuryFlag_current_year ~ career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log_prior_SBA + log_prior_HBP
  )
  
  m <- glm(f, data = tr, family = binomial)
  te_prob <- predict(m, newdata = te, type = "response")
  
  met <- eval_binary(te$InjuryFlag_current_year, te_prob, threshold = threshold) %>%
    mutate(
      PositionGroup = group_name,
      n_test = nrow(te),
      test_event_rate = mean(te$InjuryFlag_current_year == 1)
    ) %>%
    select(PositionGroup, n_test, test_event_rate, everything())
  
  # ✅ NEW: add prob column onto te, then call your visualization function
  te_for_viz <- te %>% mutate(pred_prob = te_prob)
  
  viz <- evaluate_model_visual(
    data = te_for_viz,
    prob_col = "pred_prob",
    actual_col = "InjuryFlag_current_year",
    threshold = threshold,
    title_text = paste0("Group: ", group_name, " Logistic")
  )
  
  list(group = group_name, model = m, metrics = met, viz = viz)
}

#visualize metrics

groups <- c("Pitcher","Catcher","Infield","Outfield")
group_fits <- lapply(groups, fit_logit_group, train_df = train_df, test_df = test_df, threshold = 0.25)

metrics_by_group <- bind_rows(lapply(group_fits, \(x) x$metrics))
metrics_by_group

names(group_fits) <- groups
group_fits[["Pitcher"]]$viz$confusion_plot
group_fits[["Pitcher"]]$viz$roc_plot

names(group_fits) <- groups
group_fits[["Catcherr"]]$viz$confusion_plot
group_fits[["Catcher"]]$viz$roc_plot

names(group_fits) <- groups
group_fits[["Infield"]]$viz$confusion_plot
group_fits[["Infield"]]$viz$roc_plot

names(group_fits) <- groups
group_fits[["Outfield"]]$viz$confusion_plot
group_fits[["Outfield"]]$viz$roc_plot

#side by side metrics view

# --- 1) All-player model metrics ---
test_prob_all <- predict(all_model, newdata = test_df, type = "response")

all_metrics <- eval_binary(test_df$InjuryFlag_current_year, test_prob_all, threshold = 0.25) %>%
  mutate(
    Model = "All Players",
    n_test = nrow(test_df),
    test_event_rate = mean(test_df$InjuryFlag_current_year == 1)
  ) %>%
  select(Model, n_test, test_event_rate, everything())

# --- 2) Position-group model metrics (from group_fits list) ---
group_metrics <- bind_rows(lapply(group_fits, function(x) {
  if (is.null(x$model)) {
    # fallback if a group couldn't be fit
    return(tibble(
      Model = paste0("Group: ", x$group),
      n_test = NA_integer_,
      test_event_rate = NA_real_,
      threshold = NA_real_,
      roc_auc = NA_real_,
      pr_auc = NA_real_,
      brier = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1 = NA_real_,
      tp = NA_real_, fp = NA_real_, tn = NA_real_, fn = NA_real_,
      note = "Model not fit (too few rows/events)"
    ))
  } else {
    x$metrics %>%
      mutate(Model = paste0("Group: ", PositionGroup)) %>%
      select(-PositionGroup)
  }
}))

# --- 3) Combine into one table ---
metrics_side_by_side <- bind_rows(all_metrics, group_metrics) %>%
  mutate(
    test_event_rate = round(test_event_rate, 4),
    roc_auc = round(roc_auc, 3),
    pr_auc  = round(pr_auc, 3),
    brier   = round(brier, 4),
    precision = round(precision, 3),
    recall    = round(recall, 3),
    f1        = round(f1, 3)
  ) %>%
  arrange(match(Model, c("All Players",
                         "Group: Pitcher",
                         "Group: Catcher",
                         "Group: Infield",
                         "Group: Outfield")))

metrics_side_by_side

#repeat, optimiuzing thresholds via F1

find_optimal_threshold_f1 <- function(y_true, y_prob) {
  
  thresholds <- seq(0.01, 0.80, by = 0.01)
  
  results <- dplyr::bind_rows(lapply(thresholds, function(t) {
    eval_binary(y_true, y_prob, threshold = t)
  }))
  
  best_row <- results %>%
    filter(!is.na(f1)) %>%
    slice_max(f1, n = 1, with_ties = FALSE)
  
  best_row
}
# Predict probabilities
test_prob_all <- predict(all_model, newdata = test_df, type = "response")

# Find best threshold
best_all <- find_optimal_threshold_f1(
  test_df$InjuryFlag_current_year,
  test_prob_all
)

best_all

optimal_group_metrics <- lapply(group_fits, function(x) {
  
  if (is.null(x$model)) return(NULL)
  
  group_name <- x$group
  
  te <- test_df %>% filter(PositionGroup == group_name)
  te_prob <- predict(x$model, newdata = te, type = "response")
  
  best <- find_optimal_threshold_f1(
    te$InjuryFlag_current_year,
    te_prob
  )
  
  best %>%
    mutate(
      Model = paste0("Group: ", group_name),
      n_test = nrow(te),
      test_event_rate = mean(te$InjuryFlag_current_year == 1)
    )
})

optimal_group_metrics <- dplyr::bind_rows(optimal_group_metrics)
optimal_group_metrics

optimal_all_table <- best_all %>%
  mutate(
    Model = "All Players",
    n_test = nrow(test_df),
    test_event_rate = mean(test_df$InjuryFlag_current_year == 1)
  )

optimal_metrics_combined <- dplyr::bind_rows(
  optimal_all_table,
  optimal_group_metrics
) %>%
  select(Model, n_test, test_event_rate,
         threshold, roc_auc, pr_auc, brier,
         precision, recall, f1,
         tp, fp, tn, fn)

print(optimal_metrics_combined, width = Inf)

#include prior injuries
player_year_panel_model <- player_year_panel_model %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    prior_injury_count = lag(cumsum(InjuryFlag_current_year), default = 0),
    prior_DL_days_total = lag(cumsum(DL_days_current_year), default = 0),
    injured_last_year = lag(InjuryFlag_current_year, default = 0)
  ) %>%
  ungroup()

#check
player_year_panel_model %>%
  select(playerID, yearID, InjuryFlag_current_year,
         prior_injury_count,
         injured_last_year) %>%
  arrange(playerID, yearID) %>%
  head(20)

#correlation heatmap
# Select numeric predictors only
corr_df <- player_year_panel_model %>%
  select(
    career_age,
    weight,
    prior_injury_count,
    injured_last_year,
    prior_DL_days_total,
    prior_plate_appearances,
    prior_innings_pitched,
    prior_defensive_innings
  ) %>%
  mutate(across(everything(), as.numeric))

corr_matrix <- cor(corr_df, use = "pairwise.complete.obs")

corr_long <- as.data.frame(corr_matrix) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
  filter(!is.na(Correlation))

ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  
  # Add correlation value text
  geom_text(
    aes(
      label = sprintf("%.2f", Correlation),
      color = abs(Correlation) > 0.4
    ),
    size = 4,
    fontface = "bold"
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  
  scale_fill_gradient2(
    low = "#d12028",     # negative
    mid = "white",
    high = "#67bed9",    # positive
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  
  coord_fixed() +
  labs(
    title = "Correlation Heatmap — Player Year Panel Model",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  
  theme_capstone() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

train_df <- player_year_panel_model %>% filter(playerID %in% train_players)
test_df  <- player_year_panel_model %>% filter(!playerID %in% train_players)

#all player model with prior injuries
all_model_with_history <- glm(
  InjuryFlag_current_year ~
    career_age +
    height + weight +
    TurfFlag +
    PositionGroup +
    log_prior_PA +
    log_prior_DEF +
    log_prior_IP +
    log_prior_BFP +
    log_prior_GS +
    log_prior_SBA +
    log_prior_HBP +
    prior_injury_count +
    injured_last_year,
  data = train_df,
  family = binomial
)

#metrics
test_prob_all_hist <- predict(all_model_with_history,
                              newdata = test_df,
                              type = "response")

best_all_hist <- find_optimal_threshold_f1(
  test_df$InjuryFlag_current_year,
  test_prob_all_hist
)

best_all_hist

#add injury history for player positions
# ============================================================
# Logistic models by PositionGroup WITH prior injury history

# ----------------------------
# 2) Train/test split BY PLAYER (prevents leakage across seasons)
# ----------------------------
set.seed(123)
all_players <- unique(player_year_panel_model$playerID)
train_players <- sample(all_players, size = floor(0.7 * length(all_players)))

train_df <- player_year_panel_model %>% filter(playerID %in% train_players)
test_df  <- player_year_panel_model %>% filter(!playerID %in% train_players)

# ----------------------------
# 3) Metrics helpers: ROC AUC, PR AUC, Brier, Precision/Recall/F1
# ----------------------------
pr_auc <- function(y_true, y_score) {
  o <- order(y_score, decreasing = TRUE)
  y <- as.integer(y_true[o])
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  precision <- tp / (tp + fp)
  recall <- tp / sum(y == 1)
  idx <- which(!is.na(precision) & !is.na(recall))
  precision <- precision[idx]; recall <- recall[idx]
  sum(diff(c(0, recall)) * precision)
}

eval_binary <- function(y_true, y_prob, threshold) {
  y_true <- as.integer(y_true)
  roc_obj <- pROC::roc(y_true, y_prob, quiet = TRUE)
  auc_roc <- as.numeric(pROC::auc(roc_obj))
  auc_pr  <- pr_auc(y_true, y_prob)
  brier   <- mean((y_prob - y_true)^2)
  
  y_pred <- as.integer(y_prob >= threshold)
  tp <- sum(y_pred == 1 & y_true == 1)
  tn <- sum(y_pred == 0 & y_true == 0)
  fp <- sum(y_pred == 1 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) NA_real_
  else 2 * precision * recall / (precision + recall)
  
  tibble(
    threshold = threshold,
    roc_auc = auc_roc,
    pr_auc = auc_pr,
    brier = brier,
    precision = precision,
    recall = recall,
    f1 = f1,
    tp = tp, fp = fp, tn = tn, fn = fn
  )
}

find_optimal_threshold_f1 <- function(y_true, y_prob, t_min = 0.01, t_max = 0.80, step = 0.01) {
  thresholds <- seq(t_min, t_max, by = step)
  grid <- bind_rows(lapply(thresholds, function(t) eval_binary(y_true, y_prob, threshold = t)))
  grid <- grid %>% mutate(threshold = thresholds)
  grid %>% filter(!is.na(f1)) %>% slice_max(f1, n = 1, with_ties = FALSE)
}

# ----------------------------
# 4) ALL-PLAYERS model (with prior injury)
# ----------------------------
all_model_hist <- glm(
  InjuryFlag_current_year ~
    career_age +
    height + weight +
    TurfFlag +
    PositionGroup +
    log_prior_PA +
    log_prior_DEF +
    log_prior_IP +
    log1p(prior_batters_faced) +
    log1p(prior_games_started) +
    log1p(prior_stolen_base_attempts) +
    log1p(prior_hit_by_pitch) +
    prior_injury_count +
    injured_last_year +
    log1p(prior_DL_days_total),
  data = train_df,
  family = binomial
)

test_prob_all <- predict(all_model_hist, newdata = test_df, type = "response")

best_all <- find_optimal_threshold_f1(test_df$InjuryFlag_current_year, test_prob_all) %>%
  mutate(
    Model = "All Players",
    n_test = nrow(test_df),
    test_event_rate = mean(test_df$InjuryFlag_current_year == 1)
  )

# ✅ NEW: visuals for ALL-PLAYERS logistic using the F1-optimal threshold
viz_all_hist <- evaluate_model_visual(
  data = mutate(test_df, pred_prob = test_prob_all),
  prob_col = "pred_prob",
  actual_col = "InjuryFlag_current_year",
  threshold = best_all$threshold[1],
  title_text = paste0("All Players Logistic + History (thr=", round(best_all$threshold[1], 2), ")")
)

# ----------------------------
# 5) Group-specific formulas (with prior injury)
# ----------------------------
fit_logit_group_with_history <- function(group_name, train_df, test_df) {
  
  tr <- train_df %>% filter(PositionGroup == group_name)
  te <- test_df  %>% filter(PositionGroup == group_name)
  
  # guardrails
  if (nrow(tr) < 200 || sum(tr$InjuryFlag_current_year == 1) < 20) {
    return(list(group = group_name, model = NULL, best = tibble(
      Model = paste0("Group: ", group_name),
      n_test = nrow(te),
      test_event_rate = mean(te$InjuryFlag_current_year == 1),
      note = "Too few rows/events to fit reliably"
    ),
    viz = NULL))
  }
  
  f <- switch(
    group_name,
    
    "Pitcher" = InjuryFlag_current_year ~
      career_age + height + weight + TurfFlag +
      log_prior_IP + log1p(prior_batters_faced) + log1p(prior_games_started) +
      prior_injury_count + injured_last_year + log1p(prior_DL_days_total),
    
    "Catcher" = InjuryFlag_current_year ~
      career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log1p(prior_hit_by_pitch) +
      prior_injury_count + injured_last_year + log1p(prior_DL_days_total),
    
    "Infield" = InjuryFlag_current_year ~
      career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log1p(prior_stolen_base_attempts) + log1p(prior_hit_by_pitch) +
      prior_injury_count + injured_last_year + log1p(prior_DL_days_total),
    
    "Outfield" = InjuryFlag_current_year ~
      career_age + height + weight + TurfFlag +
      log_prior_DEF + log_prior_PA + log1p(prior_stolen_base_attempts) + log1p(prior_hit_by_pitch) +
      prior_injury_count + injured_last_year + log1p(prior_DL_days_total)
  )
  
  m <- glm(f, data = tr, family = binomial)
  te_prob <- predict(m, newdata = te, type = "response")
  
  best <- find_optimal_threshold_f1(te$InjuryFlag_current_year, te_prob) %>%
    mutate(
      Model = paste0("Group: ", group_name),
      n_test = nrow(te),
      test_event_rate = mean(te$InjuryFlag_current_year == 1)
    )
  
  # ✅ NEW: visuals for GROUP logistic using its own F1-optimal threshold
  viz <- evaluate_model_visual(
    data = mutate(te, pred_prob = te_prob),
    prob_col = "pred_prob",
    actual_col = "InjuryFlag_current_year",
    threshold = best$threshold[1],
    title_text = paste0("Group: ", group_name, " Logistic + History (thr=", round(best$threshold[1], 2), ")")
  )
  
  list(group = group_name, model = m, best = best, viz = viz)
}

groups <- c("Pitcher","Catcher","Infield","Outfield")
group_fits_hist <- lapply(groups, fit_logit_group_with_history, train_df = train_df, test_df = test_df)
names(group_fits_hist) <- groups

best_groups <- bind_rows(lapply(group_fits_hist, function(x) x$best))

# ✅ NEW: collect group visuals into a named list for easy plotting
viz_groups_hist <- lapply(group_fits_hist, function(x) x$viz)

# Example: show group plots
# viz_groups_hist[["Pitcher"]]$confusion_plot
# viz_groups_hist[["Pitcher"]]$roc_plot

# ----------------------------
# 6) Combine side-by-side metrics
# ----------------------------
metrics_optimal_side_by_side <- bind_rows(best_all, best_groups) %>%
  select(Model, n_test, test_event_rate,
         threshold, roc_auc, pr_auc, brier,
         precision, recall, f1,
         tp, fp, tn, fn, everything()) %>%
  mutate(
    test_event_rate = round(test_event_rate, 4),
    threshold = round(threshold, 2),
    roc_auc = round(roc_auc, 3),
    pr_auc  = round(pr_auc, 3),
    brier   = round(brier, 4),
    precision = round(precision, 3),
    recall    = round(recall, 3),
    f1        = round(f1, 3)
  )

print(metrics_optimal_side_by_side, width = Inf, n = Inf)

#✅ View plots:
viz_all_hist$confusion_plot
viz_all_hist$roc_plot

viz_groups_hist[["Pitcher"]]$confusion_plot
viz_groups_hist[["Pitcherr"]]$roc_plot

viz_groups_hist[["Catcher"]]$confusion_plot
viz_groups_hist[["Catcher"]]$roc_plot

viz_groups_hist[["Infield"]]$confusion_plot
viz_groups_hist[["Infield"]]$roc_plot

viz_groups_hist[["Outfield"]]$confusion_plot
viz_groups_hist[["Outfield"]]$roc_plot

# ----------------------------
# 7) (Optional) inspect coefficients for each model
# ----------------------------
# --- All Players ---
cat("\n============================\n")
cat("Model: All Players\n")
cat("============================\n")
print(summary(all_model_hist))

# --- By Position ---
for (g in names(group_fits_hist)) {
  
  if (!is.null(group_fits_hist[[g]]$model)) {
    
    cat("\n============================\n")
    cat("Model:", g, "\n")
    cat("============================\n")
    
    print(summary(group_fits_hist[[g]]$model))
  }
}

#graph the coefficients
#function definition

plot_logit_forest <- function(model, model_name) {
  
  df <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      significant = p.value < 0.05,
      term_clean = term
    )
  
  ggplot(df,
         aes(x = estimate,
             y = reorder(term_clean, estimate))) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2) +
    geom_point(aes(shape = significant), size = 3) +
    scale_shape_manual(values = c(`TRUE` = 8, `FALSE` = 16)) +
    scale_x_log10() +
    labs(
      title = paste0(model_name, " — Logistic Coefficients"),
      x = "Odds Ratio (log scale)",
      y = ""
    ) +
    theme_capstone()
}

#call the function
plot_logit_forest(all_model_hist, "All Players")
plot_logit_forest(group_fits_hist[["Pitcher"]]$model, "Pitcher")
plot_logit_forest(group_fits_hist[["Catcher"]]$model, "Catcher")
plot_logit_forest(group_fits_hist[["Infield"]]$model, "Infield")
plot_logit_forest(group_fits_hist[["Outfield"]]$model, "Outfield")

#only significant terms
plot_logit_forest_sig <- function(model, model_name) {
  
  df <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    filter(p.value < 0.05) %>%
    mutate(term_clean = term)
  
  # Handle case with no significant predictors
  if (nrow(df) == 0) {
    message(paste("No significant predictors for", model_name))
    return(NULL)
  }
  
  ggplot(df,
         aes(x = estimate,
             y = reorder(term_clean, estimate))) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2) +
    geom_point(size = 3, shape = 8) +  # star for significant
    scale_x_log10() +
    labs(
      title = paste0(model_name, " — Significant Predictors Only"),
      x = "Odds Ratio (log scale)",
      y = ""
    ) +
    theme_capstone()
}

#call the function
plot_logit_forest_sig(all_model_hist, "All Players")
plot_logit_forest_sig(group_fits_hist[["Pitcher"]]$model, "Pitcher")
plot_logit_forest_sig(group_fits_hist[["Catcher"]]$model, "Catcher")
plot_logit_forest_sig(group_fits_hist[["Infield"]]$model, "Infield")
plot_logit_forest_sig(group_fits_hist[["Outfield"]]$model, "Outfield")

#*******************************************************************************
# decision tree models
 # ----------------------------
 # 1) Prepare modeling data
 # ----------------------------
 tree_data <- player_year_panel_model %>%
   mutate(
     playerID = as.character(playerID),
     yearID   = as.integer(yearID),
     # outcome as factor for rpart classification
     InjuryFlag_current_year = factor(InjuryFlag_current_year, levels = c(0, 1)),
     TurfFlag = factor(TurfFlag, levels = c("Grass","Turf")),
     PositionGroup = factor(PositionGroup, levels = c("Pitcher","Catcher","Infield","Outfield"))
   ) %>%
   filter(!is.na(InjuryFlag_current_year)) %>%
   filter(PositionGroup %in% c("Pitcher","Catcher","Infield","Outfield"))
 
 # ----------------------------
 # 2) Train/test split by playerID (prevents leakage)
 # ----------------------------
 set.seed(123)
 all_players <- unique(tree_data$playerID)
 train_players <- sample(all_players, size = floor(0.7 * length(all_players)))
 
 train_df <- tree_data %>% filter(playerID %in% train_players)
 test_df  <- tree_data %>% filter(!playerID %in% train_players)
 
 # ----------------------------
 # 3) Metrics helpers: ROC AUC, PR AUC, Brier, Precision/Recall/F1
 # ----------------------------
 pr_auc <- function(y_true_factor, y_score) {
   y_true <- as.integer(as.character(y_true_factor))
   o <- order(y_score, decreasing = TRUE)
   y <- y_true[o]
   tp <- cumsum(y == 1)
   fp <- cumsum(y == 0)
   precision <- tp / (tp + fp)
   recall <- tp / sum(y == 1)
   idx <- which(!is.na(precision) & !is.na(recall))
   precision <- precision[idx]; recall <- recall[idx]
   sum(diff(c(0, recall)) * precision)
 }
 
 eval_binary <- function(y_true_factor, y_prob, threshold) {
   y_true <- as.integer(as.character(y_true_factor))
   roc_obj <- pROC::roc(y_true, y_prob, quiet = TRUE)
   auc_roc <- as.numeric(pROC::auc(roc_obj))
   auc_pr  <- pr_auc(y_true_factor, y_prob)
   brier   <- mean((y_prob - y_true)^2)
   
   y_pred <- as.integer(y_prob >= threshold)
   tp <- sum(y_pred == 1 & y_true == 1)
   tn <- sum(y_pred == 0 & y_true == 0)
   fp <- sum(y_pred == 1 & y_true == 0)
   fn <- sum(y_pred == 0 & y_true == 1)
   
   precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
   recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
   f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) NA_real_
   else 2 * precision * recall / (precision + recall)
   
   tibble(
     threshold = threshold,
     roc_auc = auc_roc,
     pr_auc = auc_pr,
     brier = brier,
     precision = precision,
     recall = recall,
     f1 = f1,
     tp = tp, fp = fp, tn = tn, fn = fn
   )
 }
 
 find_optimal_threshold_f1 <- function(y_true_factor, y_prob, t_min = 0.01, t_max = 0.80, step = 0.01) {
   thresholds <- seq(t_min, t_max, by = step)
   grid <- bind_rows(lapply(thresholds, function(t) eval_binary(y_true_factor, y_prob, threshold = t)))
   grid <- grid %>% mutate(threshold = thresholds)
   grid %>% filter(!is.na(f1)) %>% slice_max(f1, n = 1, with_ties = FALSE)
 }
 
 # ----------------------------
 # 4) Define FULL predictor set (match your logistic)
 #    NOTE: rpart handles numeric predictors fine.
 # ----------------------------
 tree_formula_all <- as.formula(
   InjuryFlag_current_year ~
     career_age + I() +
     TurfFlag + PositionGroup +
     log_prior_PA + log_prior_DEF + log_prior_IP +
     prior_injury_count +
     injured_last_year +
     log1p(prior_DL_days_total)
 )
 
 # Group model uses same fields EXCEPT PositionGroup
 tree_formula_group <- as.formula(
   InjuryFlag_current_year ~
     career_age + I(career_age^2) + weight +
     TurfFlag +
     log_prior_PA + log_prior_DEF + log_prior_IP +
     prior_injury_count +
     injured_last_year +
     log1p(prior_DL_days_total)
 )
 
 # ----------------------------
 # 5) Fit ALL PLAYERS tree
 # ----------------------------
 tree_all <- rpart(
   tree_formula_all,
   data = train_df,
   method = "class",
   control = rpart.control(cp = 0.002, minsplit = 200, xval = 10)
 )
 
 # Predict probabilities for class "1"
 prob_all <- predict(tree_all, newdata = test_df, type = "prob")[, "1"]
 
 best_all <- find_optimal_threshold_f1(test_df$InjuryFlag_current_year, prob_all) %>%
   mutate(
     Model = "All Players (Tree)",
     n_test = nrow(test_df),
     test_event_rate = mean(as.integer(as.character(test_df$InjuryFlag_current_year)) == 1)
   )
 
 #visualize the metrics
 # ---- visualize the metrics for TREE (All Players) ----
 test_df_for_viz <- test_df %>%
   mutate(pred_prob = prob_all)   # ✅ same length as test_df
 
 results_all_tree <- evaluate_model_visual(
   data = test_df_for_viz,
   prob_col = "pred_prob",
   actual_col = "InjuryFlag_current_year",
   threshold = best_all$threshold[1],   # ✅ use your F1-optimal threshold
   title_text = "All Players — Decision Tree"
 )
 
 results_all_tree$confusion_plot
 results_all_tree$roc_plot
 
 # Optional decision tree plot
 rpart.plot(tree_all, type = 3, extra = 106, under = TRUE, tweak = 1.0)
 
 # ----------------------------
 # 6) Fit one tree per PositionGroup
 # ----------------------------
 fit_tree_by_group <- function(group_name) {
   
   tr <- train_df %>% filter(PositionGroup == group_name)
   te <- test_df  %>% filter(PositionGroup == group_name)
   
   # guardrails
   if (nrow(tr) < 250 || sum(as.integer(as.character(tr$InjuryFlag_current_year)) == 1) < 30) {
     return(list(
       group = group_name,
       model = NULL,
       best = tibble(
         Model = paste0("Group: ", group_name, " (Tree)"),
         n_test = nrow(te),
         test_event_rate = mean(as.integer(as.character(te$InjuryFlag_current_year)) == 1),
         note = "Too few rows/events"
       ),
       viz = NULL
     ))
   }
   
   tree <- rpart(
     tree_formula_group,
     data = tr,
     method = "class",
     model = TRUE,
     control = rpart.control(cp = 0.002, minsplit = 100, xval = 10)
   )
   
   # predicted prob of class "1"
   prob <- predict(tree, newdata = te, type = "prob")[, "1"]
   
   best <- find_optimal_threshold_f1(te$InjuryFlag_current_year, prob) %>%
     mutate(
       Model = paste0("Group: ", group_name, " (Tree)"),
       n_test = nrow(te),
       test_event_rate = mean(as.integer(as.character(te$InjuryFlag_current_year)) == 1)
     )
   
   # ✅ build a DF for viz function
   te_for_viz <- te %>% mutate(pred_prob = prob)
   
   viz <- evaluate_model_visual(
     data = te_for_viz,
     prob_col = "pred_prob",
     actual_col = "InjuryFlag_current_year",
     threshold = best$threshold[1],
     title_text = paste0("Group: ", group_name, " Tree (thr=", round(best$threshold[1], 2), ")")
   )
   
   list(group = group_name, model = tree, best = best, viz = viz)
 }
 
 groups <- c("Pitcher","Catcher","Infield","Outfield")
 tree_group_fits <- lapply(groups, fit_tree_by_group)
 names(tree_group_fits) <- groups
 
 # metrics table
 best_groups_tree <- bind_rows(lapply(tree_group_fits, \(x) x$best))
 print(best_groups_tree, width = Inf, n = Inf)
 
 # example visuals
 tree_group_fits[["Pitcher"]]$viz$confusion_plot
 tree_group_fits[["Pitcher"]]$viz$roc_plot
 
 tree_group_fits[["Catcher"]]$viz$confusion_plot
 tree_group_fits[["Catcher"]]$viz$roc_plot
 
 tree_group_fits[["Infield"]]$viz$confusion_plot
 tree_group_fits[["Infield"]]$viz$roc_plot
 
 tree_group_fits[["Outfield"]]$viz$confusion_plot
 tree_group_fits[["Outfield"]]$viz$roc_plot
 
#plot the decision trees for each group
 plot_tree_capstone <- function(tree_model, group_name) {
   
   rpart.plot(
     tree_model,
     main = paste0("Decision Tree — ", group_name),
     
     type = 2,
     extra = 104,
     under = TRUE,
     fallen.leaves = TRUE,
     
     # 🎨 Capstone colors
     box.col = "#67bed9",
     shadow.col = "#222c67",
     branch.col = "#222c67",
     split.col = "#d12028",
     col = "#222c67",
     
     tweak = 1.4  # ✅ only one sizing control
   )
 }
 
 names(tree_group_fits) <- groups
 
 for (g in groups) {
   
   if (is.null(tree_group_fits[[g]]$model)) {
     message("Skipping ", g, " — no model")
     next
   }
   
   plot_tree_capstone(tree_group_fits[[g]]$model, g)
 }
 
 
 # ----------------------------
 # 7) Combine metrics side-by-side
 # ----------------------------
 tree_metrics_side_by_side <- bind_rows(best_all, best_groups) %>%
   select(Model, n_test, test_event_rate,
          threshold, roc_auc, pr_auc, brier,
          precision, recall, f1,
          tp, fp, tn, fn, everything()) %>%
   mutate(
     test_event_rate = round(test_event_rate, 4),
     threshold = round(threshold, 2),
     roc_auc = round(roc_auc, 3),
     pr_auc  = round(pr_auc, 3),
     brier   = round(brier, 4),
     precision = round(precision, 3),
     recall    = round(recall, 3),
     f1        = round(f1, 3)
   )
 
 print(tree_metrics_side_by_side, width = Inf, n = Inf)
 
 # ----------------------------
 # 8) Variable importance (top 15)
 # ----------------------------
 imp_all <- sort(tree_all$variable.importance, decreasing = TRUE)
 print(head(imp_all, 15))
 
 imp_by_group <- lapply(tree_group_fits, function(x) {
   if (is.null(x$model)) return(NULL)
   sort(x$model$variable.importance, decreasing = TRUE) |> head(15)
 })
 names(imp_by_group) <- groups
 imp_by_group
 
 #plot variable importance

 plot_tree_importance <- function(imp_vec, title_text = "Tree Variable Importance", top_n = 15) {
   if (is.null(imp_vec) || length(imp_vec) == 0) return(NULL)
   
   df <- enframe(imp_vec, name = "variable", value = "importance") %>%
     arrange(desc(importance)) %>%
     slice_head(n = top_n)
   
   ggplot(df, aes(x = reorder(variable, importance), y = importance)) +
     geom_col(fill = "#67bed9") +
     coord_flip() +
     labs(title = title_text, x = "", y = "Variable importance") +
     theme_capstone()
 }
 
 imp_all <- sort(tree_all$variable.importance, decreasing = TRUE)
 
 plot_tree_importance(
   imp_all,
   title_text = "All Players — Decision Tree Variable Importance (Top 15)",
   top_n = 15
 )
 
 #for each player category
 
 imp_group_df <- lapply(groups, function(g) {
   m <- tree_group_fits[[g]]$model
   if (is.null(m)) return(NULL)
   
   enframe(sort(m$variable.importance, decreasing = TRUE),
           name = "variable", value = "importance") %>%
     slice_head(n = 15) %>%
     mutate(PositionGroup = g)
 }) %>% bind_rows()
 
 ggplot(imp_group_df, aes(x = reorder(variable, importance), y = importance)) +
   geom_col(fill = "#67bed9") +
   coord_flip() +
   facet_wrap(~ PositionGroup, scales = "free_y") +
   labs(title = "Tree Variable Importance by Position Group (Top 15 each)",
        x = "", y = "Variable importance") +
   theme_capstone()
 
 #one graph for each position category
 # Make sure the list is named
 names(tree_group_fits) <- groups
 
 for (g in groups) {
   m <- tree_group_fits[[g]]$model
   if (is.null(m)) {
     message("Skipping ", g, " (no model).")
     next
   }
   
   imp_g <- sort(m$variable.importance, decreasing = TRUE)
   
   print(
     plot_tree_importance(
       imp_g,
       title_text = paste0("Group: ", g, " — Tree Variable Importance (Top 15)"),
       top_n = 15
     )
   )
 }
 
 #only the Variables used in splits
 plot_tree_importance_splits_only <- function(tree_model, title_text, top_n = 15) {
   imp <- tree_model$variable.importance
   if (is.null(imp) || length(imp) == 0) return(NULL)
   
   used_vars <- unique(as.character(tree_model$frame$var))
   used_vars <- used_vars[used_vars != "<leaf>"]
   
   df <- enframe(imp, name = "variable", value = "importance") %>%
     filter(variable %in% used_vars) %>%
     arrange(desc(importance)) %>%
     slice_head(n = top_n)
   
   if (nrow(df) == 0) {
     message("No split variables found to plot for: ", title_text)
     return(NULL)
   }
   
   ggplot(df, aes(x = reorder(variable, importance), y = importance)) +
     geom_col(fill = "#67bed9") +
     coord_flip() +
     labs(title = title_text, x = "", y = "Variable importance (split vars only)") +
     theme_capstone()
 }
 
 # All players
 plot_tree_importance_splits_only(
   tree_all,
   "All Players — Tree Importance (Variables Used in Splits)"
 )
 
 names(tree_group_fits) <- groups
 
#one plot for each player group 
 for (g in groups) {
   m <- tree_group_fits[[g]]$model
   if (is.null(m)) next
   
   print(
     plot_tree_importance_splits_only(
       m,
       paste0("Group: ", g, " — Tree Importance (Split Vars Only)")
     )
   )
 }
 
# build risk model based on decision tree
 # Ensure the list is named
 names(tree_group_fits) <- groups  # c("Pitcher","Catcher","Infield","Outfield")
 
 # ------------------------------------------------------------
 # 1) Predict group-specific tree probabilities for every row
 # ------------------------------------------------------------
 player_year_panel_model <- player_year_panel_model %>%
   mutate(
     tree_prob_group = NA_real_,
     PositionGroup = as.character(PositionGroup)  # safer for indexing
   )
 
 for (g in groups) {
   m <- tree_group_fits[[g]]$model
   if (is.null(m)) next
   
   idx <- which(player_year_panel_model$PositionGroup == g)
   if (length(idx) == 0) next
   
   player_year_panel_model$tree_prob_group[idx] <-
     predict(m, newdata = player_year_panel_model[idx, ], type = "prob")[, "1"]
 }
 
 # ------------------------------------------------------------
 # 2) Compute group baseline injury rate p (force numeric)
 # ------------------------------------------------------------
 group_rates <- player_year_panel_model %>%
   mutate(y = as.integer(as.character(InjuryFlag_current_year))) %>%
   filter(PositionGroup %in% groups) %>%
   group_by(PositionGroup) %>%
   summarise(
     p = mean(y, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   mutate(p = as.numeric(p))   # ✅ ensure numeric
 
 # ------------------------------------------------------------
 # 3) Assign group-specific risk categories using 0.75p and 1.5p
 # ------------------------------------------------------------
  # Remove any old/bad p column first (prevents list-column issues)
 player_year_panel_model <- player_year_panel_model %>%
   select(-any_of("p"))
 
 # Join fresh baseline p (should be numeric in group_rates)
 player_year_panel_model <- player_year_panel_model %>%
   left_join(group_rates, by = "PositionGroup") %>%
   mutate(
     # Risk cutpoints: Low < 0.75p, Medium < 1.5p, else High
     RiskCategory_group = case_when(
       is.na(tree_prob_group) ~ NA_character_,
       tree_prob_group < 0.75 * p ~ "Low",
       tree_prob_group < 1.5 * p ~ "Medium",
       TRUE ~ "High"
     ),
     RiskCategory_group = factor(RiskCategory_group, levels = c("Low","Medium","High"))
   )
 # ------------------------------------------------------------
 # 4) Summaries by group + risk category
 # ------------------------------------------------------------
 risk_summary_group <- player_year_panel_model %>%
   mutate(y = as.integer(as.character(InjuryFlag_current_year))) %>%
   filter(!is.na(RiskCategory_group), PositionGroup %in% groups) %>%
   group_by(PositionGroup, RiskCategory_group) %>%
   summarise(
     n = n(),
     injury_rate = mean(y, na.rm = TRUE),
     .groups = "drop"
   )
 
 risk_summary_group
 
 #graphs
 for (g in groups) {
   
   p <- risk_summary_group %>%
     filter(PositionGroup == g)
   
   print(
     ggplot(p, aes(x = RiskCategory_group, y = n, fill = RiskCategory_group)) +
       geom_col(width = 0.65) +
       
       # Show count
       geom_text(aes(label = n),
                 vjust = -0.5,
                 size = 5,
                 fontface = "bold",
                 color = "#222c67") +
       
       # Show injury rate inside bar
       geom_text(aes(label = percent(injury_rate, accuracy = 0.1)),
                 vjust = 1.5,
                 size = 4,
                 color = "white",
                 fontface = "bold") +
       
       labs(
         title = paste0("Risk Distribution — ", g),
         subtitle = "Bar height = Number of Player-Seasons\nWhite label = Observed Injury Rate",
         x = "",
         y = "Number of Player-Seasons"
       ) +
       
       scale_fill_manual(values = c(
         "Low" = "#67bed9",
         "Medium" = "#222c67",
         "High" = "#d12028"
       )) +
       
       theme_capstone() +
       ylim(0, max(p$n) * 1.15)
   )
 }
 

 #prep files for Andy and download
 names(tree_group_fits) <- groups
 
 # ------------------------------------------------------------
 # 1) Train/test split by playerID (reproducible)
 # ------------------------------------------------------------
 set.seed(123)
 all_players <- unique(as.character(player_year_panel_model$playerID))
 train_players <- sample(all_players, size = floor(0.7 * length(all_players)))
 
 # ------------------------------------------------------------
 # 2) Create labeled dataframe + remove any old p columns
 # ------------------------------------------------------------
 labeled_player_year_panel_model <- player_year_panel_model %>%
   mutate(
     playerID = as.character(playerID),
     yearID   = as.integer(yearID),
     PositionGroup = str_squish(as.character(PositionGroup)),
     TurfFlag = str_squish(as.character(TurfFlag)),
     split = if_else(playerID %in% train_players, "Train", "Test")
   ) %>%
   filter(PositionGroup %in% groups) %>%
   # IMPORTANT: remove any existing p-like columns so join can't create p.x/p.y
   select(-any_of(c("p", "p.x", "p.y", "pred_p", "Injury_risk")))
 
 # ------------------------------------------------------------
 # 3) Compute baseline injury rate p by PositionGroup (TRAIN only)
 # ------------------------------------------------------------
 group_rates_train <- labeled_player_year_panel_model %>%
   filter(split == "Train") %>%
   mutate(y = as.integer(as.character(InjuryFlag_current_year))) %>%
   group_by(PositionGroup) %>%
   summarise(p = mean(y, na.rm = TRUE), .groups = "drop") %>%
   mutate(
     PositionGroup = str_squish(as.character(PositionGroup)),
     p = as.numeric(p)
   )
 
 # ------------------------------------------------------------
 # 4) Predict pred_p using POSITION-SPECIFIC tree model
 # ------------------------------------------------------------
 labeled_player_year_panel_model$pred_p <- NA_real_
 
 for (g in groups) {
   m <- tree_group_fits[[g]]$model
   if (is.null(m)) next
   
   idx <- which(labeled_player_year_panel_model$PositionGroup == g)
   if (length(idx) == 0) next
   
   labeled_player_year_panel_model$pred_p[idx] <-
     predict(m, newdata = labeled_player_year_panel_model[idx, ], type = "prob")[, "1"]
 }
 
 # ------------------------------------------------------------
 # 5) Join p + assign Injury_risk (force p numeric after join)
 # ------------------------------------------------------------
 low_mult  <- 0.75
 high_mult <- 1.5
 
 labeled_player_year_panel_model <- labeled_player_year_panel_model %>%
   left_join(group_rates_train, by = "PositionGroup") %>%
   mutate(
     # Force p to numeric even if it came in as character/list
     p = as.numeric(unlist(p)),
     Injury_risk = case_when(
       is.na(pred_p) ~ NA_character_,
       is.na(p) ~ NA_character_,
       pred_p < low_mult  * p ~ "Low",
       pred_p < high_mult * p ~ "Medium",
       TRUE ~ "High"
     ),
     Injury_risk = factor(Injury_risk, levels = c("Low","Medium","High"))
   )
 
 # ------------------------------------------------------------
 # 6) Quick checks
 # ------------------------------------------------------------
 print(labeled_player_year_panel_model %>% count(split, PositionGroup), n = Inf)
 print(labeled_player_year_panel_model %>% count(PositionGroup, Injury_risk), n = Inf)
 
 # ------------------------------------------------------------
 # 7) Export to CSV
 # ------------------------------------------------------------
 write.csv(
   labeled_player_year_panel_model,
   "labeled_player_year_panel_model.csv",
   row.names = FALSE
 )
 
 cat("Wrote: labeled_player_year_panel_model.csv to ", getwd(), "\n", sep = "")

 
 #short version - 3 seasons
 
 three_yr_model_labeled <- labeled_player_year_panel_model %>%
   mutate(yearID = as.integer(yearID)) %>%
   filter(yearID >= 2013, yearID <= 2015)
 
 # Quick checks
 three_yr_model_labeled %>%
   count(yearID)
 
 three_yr_model_labeled %>%
   count(PositionGroup)
 
 three_yr_model_labeled %>%
   count(split)
 
 write.csv(
   three_yr_model_labeled,
   "three_yr_model_labeled_2012_2015.csv",
   row.names = FALSE
 )
 