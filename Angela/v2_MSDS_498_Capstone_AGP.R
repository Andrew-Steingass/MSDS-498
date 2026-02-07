library(cvTools) # explicit creation of folds for cross-validation
library(ModelMetrics) # used for precision-recall evaluation of classifiers
library(car) # for recode function
library(h2o)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
install.packages(
  "stringdist",
  repos = c("https://markvanderloo.r-universe.dev", "https://cloud.r-project.org")
)
library(stringdist)
#install.packages("forcats")
library(forcats)

library(sysfonts)
library(showtext)
library(ggplot2)
library(scales)
library(lubridate)
library(survival)
library(broom)

# Add the Urbanist font from Google Fonts
sysfonts::font_add_google(name = "Urbanist", family = "urbanist")

# Enable showtext so the font renders in ggplot
showtext_auto()



setwd("C:\\Users\\Owner\\OneDrive\\Desktop\\MSDS 498\\Datasets")
Master = read.csv("Master.csv")
injuries = read.csv("injuries.csv")

str(Master)
str(injuries)
head(Master, 10)
head(injuries, 10)

#Clean up injuries df

injuries$Name <- injuries$Relinquished
injuries <- injuries %>% select(-Acquired)
injuries <- injuries %>% select(-Relinquished)
injuries <- injuries %>%
  mutate(
    Name = str_replace_all(Name, "[^[:alnum:][:space:]]+", ""),
    Name = str_trim(Name)
  )

head(injuries, 10)

sapply(injuries, function(x) sum(is.na(x)))
injuries[!complete.cases(injuries), ]

table(injuries$Name)

#injuries$Name[duplicated(injuries$Name)]
# Get the unique names that are duplicated
dup_names <- unique(injuries$Name[duplicated(injuries$Name)])

# Show just one row per duplicated name
injuries[injuries$Name %in% dup_names, ] %>%
  distinct(Name, .keep_all = TRUE)

#combine First and Last to form name

Master$Name <- paste(Master$nameFirst, Master$nameLast)

head(Master, 10)

sapply(Master, function(x) sum(is.na(x)))
Master[!complete.cases(Master), ]

table(Master$Name)

#Master$Name[duplicated(Master$Name)]
# Get the unique names that are duplicated
mas_dup_names <- unique(Master$Name[duplicated(Master$Name)])

# Show just one row per duplicated name
Master[Master$Name %in% dup_names, ] %>%
  distinct(Name, .keep_all = TRUE)

#shrink master file to those players with last game 2000 or later and recheck for duplicates
#youngMaster <- Master[Master$birthYear >= 1950, ]
youngMaster <- Master[Master$finalGame >= as.Date("1999-01-01"), ]

dup_names <- unique(youngMaster$Name[duplicated(youngMaster$Name)])

# Show just one row per duplicated name
youngMaster[youngMaster$Name %in% dup_names, ] %>%
  distinct(Name, .keep_all = TRUE)

#check if those duplicate names appear in injury dataset
injury_matches <- injuries[injuries$Name %in% dup_names, ]
head(injury_matches)

#remove these 20 entries from injury dataset
final_injuries <- injuries[!injuries$Name %in% dup_names, ]

#look up and add playerID to Injury final dataset

final_injuries <- final_injuries %>%
  left_join(
    youngMaster %>% select(Name, playerID),
    by = "Name"
  )

#gut check for NAs - there are 1508!!!!
sum(is.na(final_injuries$playerID))

#fuzzy join adjacent for the remainder

# 2) Find which rows still have NA playerID
to_match <- final_injuries %>% filter(is.na(playerID))

# Build a distance matrix between injury names and master names
dist_matrix <- stringdistmatrix(
  to_match$Name,
  youngMaster$Name,
  method = "jw"  # Jaro-Winkler; good for name matching
)


best_match <- apply(dist_matrix, 1, function(row) {
  best_i <- which.min(row)                 # index of closest match
  best_dist <- row[best_i]                 # the distance
  if (best_dist < 0.15) best_i else NA     # threshold, adjust as needed
})

# Extract the matched playerIDs
matched_ids <- youngMaster$playerID[best_match]

# Assign back to final_injuries
final_injuries$playerID[is.na(final_injuries$playerID)] <- matched_ids

#gut check for NAs - there are still 500!!!!
sum(is.na(final_injuries$playerID))  
  
#identify remaining to matcah (500 NAs)
to_match <- final_injuries %>% filter(is.na(playerID))

#simply remaining names to redo player id match
final_injuries <- final_injuries %>%
  mutate(
    # only simplify names where playerID is still NA
    Name = if_else(
      is.na(playerID),
      # extract first and last word
      str_trim(paste(
        word(Name, 1),       # first word
        word(Name, -1)       # last word
      )),
      Name
    )
  )
final_injuries <- final_injuries %>%
  left_join(
    youngMaster %>% select(Name, playerID),
    by = "Name"
  )

#gut check for NAs - there are still 500!!!!
sum(is.na(final_injuries$playerID))  

#remove 2 many to many matches due to warning
final_injuries <- final_injuries[-c(2802, 4470), ]

write.csv(final_injuries, "final_injuries.csv", row.names = FALSE)

#Birth Date compilation and transfer to final_injuries for age calculationi

youngMaster <- youngMaster %>%
  mutate(
    birthDate = make_date(
      year  = birthYear,
      month = birthMonth,
      day   = birthDay
    )
  )

young_birth <- youngMaster %>%
  select(playerID, birthDate) %>%
  filter(!is.na(playerID)) %>%          # ✅ prevents NA-to-NA explosion
  group_by(playerID) %>%
  summarise(birthDate = first(na.omit(birthDate)), .groups = "drop")

final_injuries <- final_injuries %>%
  left_join(young_birth, by = c("playerID.x" = "playerID"))

#calculate age in months
final_injuries <- final_injuries %>%
  mutate(
    Date = as.Date(Date),  # ensure Date is Date class
    injury_Age = time_length(interval(birthDate, Date), "month")
  )

#Add position to final_injuries

Fielding <- read.csv("Fielding.csv")

fielding_pos <- Fielding %>%
  filter(!is.na(playerID), !is.na(POS)) %>%
  count(playerID, POS, name = "n") %>%
  group_by(playerID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(playerID, POS)

final_injuries <- final_injuries %>%
  mutate(playerID.x = as.character(playerID.x))

fielding_pos <- fielding_pos %>%
  mutate(playerID = as.character(playerID))

final_injuries <- final_injuries %>%
  left_join(
    fielding_pos,
    by = c("playerID.x" = "playerID")
  )

#Gut check
nrow(final_injuries)

final_injuries %>%
  summarise(
    total = n(),
    missing_position = sum(is.na(POS))
  )

#add position to youngMaster dataset similarly
youngMaster <- youngMaster %>%
  mutate(playerID = as.character(playerID))

fielding_pos <- fielding_pos %>%
  mutate(playerID = as.character(playerID))

youngMaster <- youngMaster %>%
  left_join(fielding_pos, by = "playerID")


# EDA of injury only dataset

str(final_injuries)
dim(final_injuries)
summary(final_injuries)
sapply(final_injuries, function(x) sum(is.na(x)))

# graphs
cat_vars <- names(final_injuries)[sapply(final_injuries, function(x) is.factor(x) || is.character(x))]
cat_vars
for (var in cat_vars) {
  cat("\n--------", var, "--------\n")
  print(table(final_injuries[[var]], useNA = "ifany"))
}

ggplot(final_injuries, aes(x = fct_lump_n(Injury_Type, 10))) +
  geom_bar(fill = "#67bed9") +
  labs(
    title = "Top 10 Injury Types",
    x = "Injury Type",
    y = "Count"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(family = "urbanist", size = 36, face = "bold"),
    axis.title = element_text(family = "urbanist", size = 24),
    axis.text = element_text(family = "urbanist", size = 24)
  ) +
  coord_flip()

# count frequencies of each value
dl_counts <- sort(table(final_injuries$DL_length), decreasing = TRUE)

# take the top 10
top10_dl <- head(dl_counts, 10)

final_injuries %>%
  count(DL_length) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(as.factor(DL_length), n), y = n)) +
  geom_col(fill = "#67bed9") +
  labs(
    title = "Top 10 Most Frequent DL_length Values",
    x = "DL_length",
    y = "Count"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(family = "urbanist", size = 36, face = "bold"),
    axis.title = element_text(family = "urbanist", size = 24),
    axis.text = element_text(family = "urbanist", size = 24)
  ) +
  coord_flip()

#filter youngMaster data table for only (at anytime) injured players
master_filtered <- youngMaster %>%
  filter(playerID %in% final_injuries$playerID.x)

master_filtered %>%
  summarise(
    mean_birthyear = mean(birthYear, na.rm = TRUE),
    sd_birthyear   = sd(birthYear, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    sd_height   = sd(height, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight   = sd(weight, na.rm = TRUE)
  )

master_filtered %>%
  count(bats, sort = TRUE)

master_filtered %>%
  count(throws, sort = TRUE)

# visualizations

ggplot(master_filtered, aes(x = birthYear)) +
  geom_histogram(binwidth = 1, fill = "#67bed9") +
  theme_minimal() +
  labs(title = "Birth Year Distribution of Injured Players")

ggplot(master_filtered, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "#67bed9") +
  theme_minimal() +
  labs(title = "Height Distribution of Injured Players")

ggplot(master_filtered, aes(x = weight)) +
  geom_histogram(binwidth = 1, fill = "#67bed9") +
  theme_minimal() +
  labs(title = "Weight Distribution of Injured Players")

ggplot(master_filtered, aes(x = bats)) +
  geom_bar(fill = "#67bed9") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Bat Handedness of Injured Players")

ggplot(master_filtered, aes(x = throws)) +
  geom_bar(fill = "#67bed9") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Throw Handedness of Injured Players")

#compare injured players to non injured players

#THIS IS NOT AGE AT TIME OF INJURY

non_injured <- youngMaster %>%
  filter(!playerID %in% master_filtered$playerID)

compare_injured_vs_non <- function(injured_df, non_df, var) {
  
  x_inj <- injured_df[[var]] %>% na.omit()
  x_non <- non_df[[var]] %>% na.omit()
  
  pval <- if (length(x_inj) >= 2 && length(x_non) >= 2) {
    t.test(x_inj, x_non)$p.value   # Welch two-sample t-test
  } else {
    NA_real_
  }
  
  tibble(
    variable        = var,
    n_injured       = length(x_inj),
    mean_injured    = mean(x_inj),
    sd_injured      = sd(x_inj),
    n_non_injured   = length(x_non),
    mean_non        = mean(x_non),
    sd_non          = sd(x_non),
    p_value         = pval
  )
}

final_compare_table <- bind_rows(
  compare_injured_vs_non(master_filtered, non_injured, "birthYear"),
  compare_injured_vs_non(master_filtered, non_injured, "height"),
  compare_injured_vs_non(master_filtered, non_injured, "weight")
)

final_compare_table

final_compare_table %>%
  mutate(
    p_value = signif(p_value, 3),
    mean_injured = round(mean_injured, 2),
    mean_non = round(mean_non, 2),
    sd_injured = round(sd_injured, 2),
    sd_non = round(sd_non, 2)
  )

# BOXPLOTS
plot_df <- bind_rows(
  master_filtered %>%
    transmute(group = "Injured",
              birthYear, height, weight),
  non_injured %>%
    transmute(group = "Non-injured",
              birthYear, height, weight)
) %>%
  pivot_longer(
    cols = c(birthYear, height, weight),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    group = factor(group, levels = c("Non-injured", "Injured")),
    variable = factor(variable, levels = c("birthYear", "height", "weight"))
  )
ggplot(plot_df, aes(x = group, y = value, fill = group)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.25) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Non-injured" = "grey80", "Injured" = "#67bed9")) +
  labs(
    title = "Injured vs Non-injured: Distribution Comparison",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.position = "none"
  )

# Injuries by date time series plot

injuries_by_date <- final_injuries %>%
  mutate(Date = as.Date(Date)) %>%
  count(Date)

ggplot(injuries_by_date, aes(x = Date, y = n)) +
  geom_line(color = "#67bed9", linewidth = 1.2) +
  geom_point(color = "#67bed9", size = 1.8) +
  labs(
    title = "Injury Counts Over Time",
    x = "Date",
    y = "Number of Injuries"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#Injury counts by age
ggplot(final_injuries, aes(x = injury_Age)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "#67bed9",
    alpha = 0.75
  ) +
  geom_density(color = "#222c67", linewidth = 1) +
  labs(
    title = "Distribution of Age at Injury",
    x = "Age at Injury (months)",
    y = "Density"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#boxplot
ggplot(final_injuries, aes(y = injury_Age)) +
  geom_boxplot(fill = "#67bed9", width = 0.2) +
  labs(
    title = "Age at Injury (Months)",
    y = "Age at Injury (months)",
    x = NULL
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 16)
  )

#Injuries by player

injuries_by_player <- final_injuries %>%
  filter(!is.na(playerID.x)) %>%
  count(playerID.x, name = "injury_count")
library(ggplot2)
library(forcats)

top_n <- 25  # adjust as needed

injuries_top <- injuries_by_player %>%
  slice_max(injury_count, n = top_n) %>%
  mutate(playerID.x = fct_reorder(playerID.x, injury_count))

ggplot(injuries_top, aes(x = playerID.x, y = injury_count)) +
  geom_col(fill = "#67bed9") +
  coord_flip() +
  labs(
    title = paste("Top", top_n, "Players by Injury Count"),
    x = "Player ID",
    y = "Number of Injuries"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

#Histogram version

ggplot(injuries_by_player, aes(x = injury_count)) +
  geom_histogram(
    bins = 30,
    fill = "#67bed9",
    color = "white"
  ) +
  labs(
    title = "Distribution of Injuries per Player",
    x = "Number of Injuries per Player",
    y = "Number of Players"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#injuries by position

injuries_by_position <- final_injuries %>%
  filter(!is.na(POS)) %>%
  count(POS, name = "injury_count") %>%
  arrange(desc(injury_count))

ggplot(injuries_by_position, aes(x = reorder(POS, injury_count), y = injury_count)) +
  geom_col(fill = "#67bed9") +
  coord_flip() +
  labs(
    title = "Injury Count by Position",
    x = "Position",
    y = "Number of Injuries"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#EDA with position data included
library(forcats)

# 0A) One POS per player (primary/most frequent)
fielding_pos <- Fielding %>%
  filter(!is.na(playerID), !is.na(POS)) %>%
  count(playerID, POS, name = "n") %>%
  group_by(playerID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  mutate(playerID = as.character(playerID))

# 0B) Add POS to youngMaster
youngMaster_pos <- youngMaster %>%
  mutate(playerID = as.character(playerID)) %>%
  left_join(fielding_pos, by = "playerID")

# 0C) Define injured players based on final_injuries
injured_players <- final_injuries %>%
  filter(!is.na(playerID.x)) %>%
  distinct(playerID.x) %>%
  pull(playerID.x) %>%
  as.character()

youngMaster_pos <- youngMaster_pos %>%
  mutate(
    injury_status = if_else(playerID %in% injured_players, "Injured", "Non-injured"),
    injury_status = factor(injury_status, levels = c("Non-injured", "Injured"))
  )
youngMaster_pos <- youngMaster_pos %>%
  rename(POS = POS.y) %>%     # keep Fielding position
  select(-POS.x)              # drop the old one if you don’t need it

# 0D) Add POS to final_injuries (player-level POS lookup)
final_injuries_pos <- final_injuries %>%
  mutate(playerID.x = as.character(playerID.x),
         Date = as.Date(Date)) %>%
  left_join(fielding_pos, by = c("playerID.x" = "playerID"))

within_pos_summary <- youngMaster_pos %>%
  filter(!is.na(POS)) %>%
  group_by(POS, injury_status) %>%
  summarise(
    n = n(),
    mean_birthYear = mean(birthYear, na.rm = TRUE),
    sd_birthYear   = sd(birthYear, na.rm = TRUE),
    mean_height    = mean(height, na.rm = TRUE),
    sd_height      = sd(height, na.rm = TRUE),
    mean_weight    = mean(weight, na.rm = TRUE),
    sd_weight      = sd(weight, na.rm = TRUE),
    .groups = "drop"
  )

within_pos_summary

pvals_within_pos <- function(df, var) {
  df %>%
    filter(!is.na(POS)) %>%
    group_by(POS) %>%
    summarise(
      p_value = {
        x <- df[df$POS == first(POS) & df$injury_status == "Injured",   var][[1]]
        y <- df[df$POS == first(POS) & df$injury_status == "Non-injured", var][[1]]
        x <- na.omit(x); y <- na.omit(y)
        if (length(x) >= 2 && length(y) >= 2) t.test(x, y)$p.value else NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(variable = var) %>%
    select(variable, POS, p_value)
}

within_pos_pvals <- bind_rows(
  pvals_within_pos(youngMaster_pos, "birthYear"),
  pvals_within_pos(youngMaster_pos, "height"),
  pvals_within_pos(youngMaster_pos, "weight")
)

within_pos_pvals

#box plots injured versus non injured
# Full position name lookup
pos_lookup <- c(
  "P"  = "Pitcher",
  "C"  = "Catcher",
  "1B" = "First Base",
  "2B" = "Second Base",
  "3B" = "Third Base",
  "SS" = "Shortstop",
  "LF" = "Left Field",
  "CF" = "Center Field",
  "RF" = "Right Field",
  "OF" = "Outfield"
)

plot_df <- youngMaster_pos %>%
  filter(
    !is.na(POS),
    POS != "DH"        # ❌ remove DH
  ) %>%
  mutate(
    POS_full = recode(POS, !!!pos_lookup)
  ) %>%
  select(POS_full, injury_status, birthYear, height, weight) %>%
  pivot_longer(
    cols = c(birthYear, height, weight),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      birthYear = "Birth Year",
      height    = "Height",
      weight    = "Weight"
    ),
    variable = factor(variable, levels = c("Birth Year", "Height", "Weight")),
    injury_status = factor(injury_status, levels = c("Non-injured", "Injured"))
  )

ggplot(plot_df, aes(x = injury_status, y = value, fill = injury_status)) +
  geom_boxplot(width = 0.45, outlier.alpha = 0.2) +
  facet_grid(variable ~ POS_full, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Non-injured" = "#d12208",   # ✅ new color
      "Injured"     = "#67bed9"
    )
  ) +
  labs(
    title = "Injured vs Non-injured Within Each Position",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

#injuries per player by position
injuries_per_player <- final_injuries_pos %>%
  filter(!is.na(playerID.x)) %>%
  count(playerID.x, name = "injury_count") %>%
  left_join(fielding_pos, by = c("playerID.x" = "playerID")) %>%
  filter(!is.na(POS))
injury_rate_by_pos <- injuries_per_player %>%
  group_by(POS) %>%
  summarise(
    players = n(),
    mean_injuries_per_player = mean(injury_count),
    sd_injuries_per_player   = sd(injury_count),
    median_injuries_per_player = median(injury_count),
    pct_players_1plus = mean(injury_count >= 1),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_injuries_per_player))

injury_rate_by_pos

# Full position name lookup
pos_lookup <- c(
  "P"  = "Pitcher",
  "C"  = "Catcher",
  "1B" = "First Base",
  "2B" = "Second Base",
  "3B" = "Third Base",
  "SS" = "Shortstop",
  "LF" = "Left Field",
  "CF" = "Center Field",
  "RF" = "Right Field",
  "OF" = "Outfield"
)

plot_df <- injuries_per_player %>%
  filter(!is.na(POS), POS != "DH") %>%
  mutate(
    POS_full = recode(POS, !!!pos_lookup)
  )

ggplot(plot_df, aes(x = injury_count)) +
  geom_histogram(
    binwidth = 1,
    center = 0,
#    boundary = 0,
    fill = "#67bed9",
    color = "white"
  ) +
  facet_wrap(~ POS_full, scales = "free_y") +
  scale_x_continuous(
    limits = c(0, max(plot_df$injury_count, na.rm = TRUE)),
    breaks = seq(0, max(plot_df$injury_count, na.rm = TRUE), by = 2)
  ) +
  labs(
    title = "Injuries per Player by Position",
    x = "Number of Injuries per Player",
    y = "Number of Players"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

age_df <- final_injuries_pos %>%
  filter(!is.na(POS.x), is.finite(injury_Age), !is.na(injury_Age), injury_Age >= 0)

ggplot(age_df, aes(x = injury_Age)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "#67bed9", color = "white", alpha = 0.75) +
  geom_density(linewidth = 1) +
  facet_wrap(~ POS.x, scales = "free_y") +
  labs(
    title = "Age at Injury (Months) by Position",
    x = "Age at Injury (months)",
    y = "Density"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )
ggplot(age_df, aes(x = fct_reorder(POS.x, injury_Age, .fun = median), y = injury_Age)) +
  geom_boxplot(fill = "#67bed9", width = 0.35, outlier.alpha = 0.2) +
  coord_flip() +
  labs(
    title = "Age at Injury by Position",
    x = "Position",
    y = "Age at Injury (months)"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.text = element_text(size = 12)
  )

#statsp
within_pos_tests <- function(df, var) {
  df %>%
    filter(!is.na(POS), !is.na(.data[[var]])) %>%
    group_by(POS) %>%
    summarise(
      n_injured = sum(injury_status == "Injured"),
      n_non     = sum(injury_status == "Non-injured"),
      mean_injured = mean(.data[[var]][injury_status == "Injured"], na.rm = TRUE),
      mean_non     = mean(.data[[var]][injury_status == "Non-injured"], na.rm = TRUE),
      p_value = {
        x <- .data[[var]][injury_status == "Injured"]
        y <- .data[[var]][injury_status == "Non-injured"]
        x <- na.omit(x); y <- na.omit(y)
        if (length(x) >= 2 && length(y) >= 2) t.test(x, y)$p.value else NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}

within_pos_results <- bind_rows(
  within_pos_tests(youngMaster_pos, "birthYear"),
  within_pos_tests(youngMaster_pos, "height"),
  within_pos_tests(youngMaster_pos, "weight")
)

print(within_pos_results, n = Inf)

#plot proportion of players injured by position - multiple injuries for same player count as one

injured_players <- final_injuries %>%
  filter(!is.na(playerID.x)) %>%
  distinct(playerID.x) %>%
  pull(playerID.x) %>%
  as.character()

prop_inj_by_pos <- youngMaster_pos %>%
  mutate(
    playerID = as.character(playerID),
    injured_flag = playerID %in% injured_players
  ) %>%
  filter(!is.na(POS), !is.na(playerID)) %>%
  group_by(POS) %>%
  summarise(
    n_players = n_distinct(playerID),
    n_injured = n_distinct(playerID[injured_flag]),
    prop_injured = n_injured / n_players,
    .groups = "drop"
  ) %>%
  arrange(desc(prop_injured))

prop_inj_by_pos

ggplot(prop_inj_by_pos,
       aes(x = fct_reorder(POS, prop_injured), y = prop_injured)) +
  geom_col(fill = "#67bed9") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Players with ≥1 Injury by Position",
    x = "Position",
    y = "Proportion Injured"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#calculate number of injuries per player by position

final_injuries_pos <- final_injuries_pos %>%
  rename(POS = POS.y) %>%   # keep Fielding position
  select(-POS.x)            # drop the other one

# Denominator: unique players by position
players_by_pos <- youngMaster_pos %>%
  filter(!is.na(POS), !is.na(playerID)) %>%
  summarise(n_players = n_distinct(playerID), .by = POS)

# Numerator: injury occurrences by position (each row = one injury)
injuries_by_pos <- final_injuries_pos %>%
  filter(!is.na(POS), !is.na(playerID.x)) %>%
  count(POS, name = "n_injuries")

# Combine: injuries per player by position
prop_injury_occurrence <- players_by_pos %>%
  left_join(injuries_by_pos, by = "POS") %>%
  mutate(
    n_injuries = replace_na(n_injuries, 0),
    injuries_per_player = n_injuries / n_players
  ) %>%
  arrange(desc(injuries_per_player))

prop_injury_occurrence


ggplot(prop_injury_occurrence,
       aes(x = fct_reorder(POS, injuries_per_player),
           y = injuries_per_player)) +
  geom_col(fill = "#67bed9") +
  coord_flip() +
  labs(
    title = "Injury Occurrences per Player by Position",
    x = "Position",
    y = "Injuries per Player"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

#Cox proportional hazards model 

if ("POS.y" %in% names(youngMaster_pos)) {
  youngMaster_pos <- youngMaster_pos %>%
    rename(POS = POS.y) %>%
    select(-any_of("POS.x"))
}

if ("POS.y" %in% names(final_injuries_pos)) {
  final_injuries_pos <- final_injuries_pos %>%
    rename(POS = POS.y) %>%
    select(-any_of("POS.x"))
}

# ----------------------------
# 1) PLAYER-LEVEL EVENT TIMES (FIRST injury only)
# final_injuries_pos is injury-level with:
# - playerID.x
# - injury_Age (months)
# ----------------------------
injury_time <- final_injuries_pos %>%
  mutate(playerID.x = as.character(playerID.x)) %>%
  filter(
    !is.na(playerID.x),
    is.finite(injury_Age),
    !is.na(injury_Age),
    injury_Age >= 0
  ) %>%
  group_by(playerID.x) %>%
  summarise(
    time_months = min(injury_Age),  # time to FIRST injury
    event = 1L,
    .groups = "drop"
  ) %>%
  rename(playerID = playerID.x)

# ----------------------------
# 2) PLAYER-LEVEL CENSOR TIMES (non-injured right-censored at finalGame)
# youngMaster_pos is player-level with:
# - playerID
# - birthDate
# - finalGame (date of final game)
# ----------------------------
censor_time <- youngMaster_pos %>%
  mutate(
    playerID = as.character(playerID),
    birthDate = as.Date(birthDate),
    finalGame = as.Date(finalGame)
  ) %>%
  filter(
    !is.na(playerID),
    !is.na(birthDate),
    !is.na(finalGame)
  ) %>%
  mutate(
    time_months = time_length(interval(birthDate, finalGame), "month"),
    event = 0L
  ) %>%
  select(playerID, time_months, event)

# ----------------------------
# 3) COMBINE: event time overrides censor time for injured players
# Start from all players (censor_time), then replace with injury_time where available.
# ----------------------------
survival_df <- censor_time %>%
  left_join(injury_time, by = "playerID", suffix = c("_cens", "_inj")) %>%
  transmute(
    playerID,
    time_months = if_else(!is.na(time_months_inj), time_months_inj, time_months_cens),
    event       = if_else(!is.na(event_inj),       event_inj,       event_cens)
  )

# ----------------------------
# 4) ADD COVARIATES: POS, height, weight (from youngMaster_pos)
# Also remove DH (recommended) + drop unusable rows
# ----------------------------
survival_df <- survival_df %>%
  left_join(
    youngMaster_pos %>%
      mutate(playerID = as.character(playerID)) %>%
      select(playerID, POS, height, weight),
    by = "playerID"
  ) %>%
  filter(
    is.finite(time_months),
    !is.na(time_months),
    time_months > 0,
    !is.na(POS),
    POS != "DH",
    !is.na(height),
    !is.na(weight)
  ) %>%
  mutate(
    POS = droplevels(factor(POS))
  )

# ----------------------------
# 5) FIT COX PROPORTIONAL HAZARDS MODEL
# ----------------------------
cox_model <- coxph(
  Surv(time_months, event) ~ height + weight + POS,
  data = survival_df
)

summary(cox_model)

# ----------------------------
# 6) HAZARD RATIOS + 95% CI
# ----------------------------
hazard_ratios <- exp(cbind(
  HR = coef(cox_model),
  confint(cox_model)
))

hazard_ratios

# ----------------------------
# 7) MODEL DIAGNOSTICS: proportional hazards test
# ----------------------------
ph_test <- cox.zph(cox_model)
ph_test
plot(ph_test)

# ============================================================
# Kaplan–Meier curves with:
# - Full position names in legend
# - X-axis starts at 200 months
# - X-axis label: "Age at First Injury (in months)"
# ============================================================

library(survival)

# Full position labels (match your POS codes)
pos_full <- c(
  "P"  = "Pitcher",
  "C"  = "Catcher",
  "1B" = "First Base",
  "2B" = "Second Base",
  "3B" = "Third Base",
  "SS" = "Shortstop",
  "LF" = "Left Field",
  "CF" = "Center Field",
  "RF" = "Right Field",
  "OF" = "Outfield",
  "DH" = "Designated Hitter"
)

# ----------------------------
# Overall KM curve
# ----------------------------
km_all <- survfit(Surv(time_months, event) ~ 1, data = survival_df)

plot(
  km_all,
  xlim = c(200, max(survival_df$time_months, na.rm = TRUE)),
  xlab = "Age at First Injury (in months)",
  ylab = "Injury-free probability",
  main = "Kaplan–Meier: Injury-free survival (All players)"
)

# ----------------------------
# KM by position
# ----------------------------
km_pos <- survfit(Surv(time_months, event) ~ POS, data = survival_df)

# Build full-text legend labels in the same order as the strata
# strata look like "POS=1B", "POS=2B", ...
pos_codes <- sub("^POS=", "", names(km_pos$strata))
legend_labels <- unname(pos_full[pos_codes])

# Fallback if any codes aren't in pos_full
legend_labels[is.na(legend_labels)] <- pos_codes[is.na(legend_labels)]

plot(
  km_pos,
  xlim = c(200, max(survival_df$time_months, na.rm = TRUE)),
  xlab = "Age at First Injury (in months)",
  ylab = "Injury-free probability",
  main = "Kaplan–Meier: Injury-free survival by position",
  col = seq_along(km_pos$strata),
  lwd = 2
)

legend(
  "bottomleft",
  legend = legend_labels,
  col = seq_along(km_pos$strata),
  lty = 1,
  lwd = 2,
  cex = 0.8
)

# ----------------------------
# 9) OPTIONAL: quick dataset checks
# ----------------------------
table(survival_df$event)          # 0=censored, 1=injured
summary(survival_df$time_months)  # time distribution
table(survival_df$POS)            # position counts

# REDO LOGISTIC REGRESSION WITHOUT AGE

injured_players <- final_injuries_pos %>%
  filter(!is.na(playerID.x)) %>%
  distinct(playerID.x) %>%
  pull(playerID.x) %>%
  as.character()

model_df <- youngMaster_pos %>%
  mutate(
    playerID = as.character(playerID),
    injured = if_else(playerID %in% injured_players, 1L, 0L)
  )

model_df_clean <- model_df %>%
  filter(
    !is.na(POS),
    POS != "DH",
    !is.na(height),
    !is.na(weight)
  ) %>%
  mutate(
    POS = droplevels(factor(POS))
  )
model_df_clean$POS <- relevel(model_df_clean$POS, ref = "1B")

logit_no_age <- glm(
  injured ~ height + weight + POS,
  data = model_df_clean,
  family = binomial(link = "logit")
)

summary(logit_no_age)

odds_ratios <- exp(cbind(
  OR = coef(logit_no_age),
  confint(logit_no_age)
))

odds_ratios

model_df_clean <- model_df_clean %>%
  mutate(predicted_prob = predict(logit_no_age, type = "response"))

roc_obj <- roc(model_df_clean$injured, model_df_clean$predicted_prob)
auc(roc_obj)
plot(roc_obj)

threshold <- 0.30

model_df_clean <- model_df_clean %>%
  mutate(predicted_class = if_else(predicted_prob >= threshold, 1L, 0L))

table(
  Predicted = model_df_clean$predicted_class,
  Actual    = model_df_clean$injured
)

#another take on EDA - average injury days by active season

days_injured_per_player <- final_injuries_pos %>%
  mutate(playerID = as.character(playerID.x)) %>%
  filter(!is.na(playerID), !is.na(DL_length)) %>%
  group_by(playerID) %>%
  summarise(
    total_days_injured = sum(DL_length, na.rm = TRUE),
    .groups = "drop"
  )
youngMaster_pos <- youngMaster_pos %>%
  mutate(
    debut_year = lubridate::year(debut),
    last_year  = lubridate::year(finalGame)
  )
youngMaster_pos <- youngMaster_pos %>%
  mutate(
    adj_debut_year = pmax(debut_year, 2000, na.rm = TRUE),
    active_seasons = last_year - adj_debut_year + 1
  ) %>%
  filter(active_seasons > 0)
youngMaster_pos <- youngMaster_pos %>%
  left_join(days_injured_per_player, by = "playerID") %>%
  mutate(
    total_days_injured = replace_na(total_days_injured, 0)
  )
youngMaster_pos <- youngMaster_pos %>%
  mutate(
    avg_days_injured_per_season =
      total_days_injured / active_seasons
  )
summary(youngMaster_pos$avg_days_injured_per_season)

# Top injury-burden players
youngMaster_pos %>%
  arrange(desc(avg_days_injured_per_season)) %>%
  select(playerID, POS, active_seasons,
         total_days_injured, avg_days_injured_per_season) %>%
  head(10)
summary(youngMaster_pos$avg_days_injured_per_season)

#Histograms for injured players only
# Full position labels
pos_lookup <- c(
  "P"  = "Pitcher",
  "C"  = "Catcher",
  "1B" = "First Base",
  "2B" = "Second Base",
  "3B" = "Third Base",
  "SS" = "Shortstop",
  "LF" = "Left Field",
  "CF" = "Center Field",
  "RF" = "Right Field",
  "OF" = "Outfield",
  "DH" = "Designated Hitter"
)

plot_df <- youngMaster_pos %>%
  filter(
    !is.na(avg_days_injured_per_season),
    avg_days_injured_per_season > 0,   # ✅ injured players only
    is.finite(avg_days_injured_per_season),
    !is.na(POS),
    POS != "DH"
  ) %>%
  mutate(
    POS_full = recode(POS, !!!pos_lookup, .default = POS)
  )

ggplot(plot_df,
       aes(x = avg_days_injured_per_season)) +
  geom_histogram(
    bins = 30,
    fill = "#67bed9",
    color = "white",
    alpha = 0.85
  ) +
  facet_wrap(~ POS_full, scales = "free_y") +
  labs(
    title = "Distribution of Average Days Injured per Active Season\n(Injured Players Only)",
    x = "Average Days Injured per Season",
    y = "Number of Players"
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16)
  )

#Model on top 10% injury burden

threshold <- quantile(
  youngMaster_pos$avg_days_injured_per_season,
  probs = 0.85,
  na.rm = TRUE
)

youngMaster_pos <- youngMaster_pos %>%
  mutate(
    high_burden = avg_days_injured_per_season >= threshold
  )

#logistic regression on 85% quartile
outlier_model <- glm(
  high_burden ~ POS + height + weight,
  data = youngMaster_pos %>%
    filter(!is.na(high_burden), POS != "DH"),
  family = binomial
)

summary(outlier_model)

exp(cbind(
  OR = coef(outlier_model),
  confint(outlier_model)
))

#graph the results

or_df <- tidy(
  outlier_model,
  conf.int = TRUE,
  conf.level = 0.90,   # keep 0.95 if you prefer
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)")

or_df_sig <- or_df %>%
  filter(conf.low > 1 | conf.high < 1)

#graph only the significant factors (CI does not include 1)

pos_lookup <- c(
  "POS1B" = "First Base",
  "POS2B" = "Second Base",
  "POS3B" = "Third Base",
  "POSC"  = "Catcher",
  "POSCF" = "Center Field",
  "POSLF" = "Left Field",
  "POSOF" = "Outfield",
  "POSP"  = "Pitcher",
  "POSSS" = "Shortstop",
  "height" = "Height (per inch)",
  "weight" = "Weight (per pound)"
)

or_df_sig <- or_df_sig %>%
  mutate(
    term_clean = recode(term, !!!pos_lookup),
    term_clean = fct_reorder(term_clean, estimate)
  )
ggplot(or_df_sig, aes(x = estimate, y = term_clean)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, color = "#67bed9") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    color = "#67bed9"
  ) +
  scale_x_log10() +
  labs(
    title = "Significant Predictors of High Injury Burden",
    subtitle = "Odds Ratios (Significant at Selected Confidence Level)",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_family = "urbanist") +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16)
  )

#Cox based risk score (by tertile)

tier_df <- youngMaster_pos %>%
  filter(!is.na(POS), POS != "DH", !is.na(height), !is.na(weight)) %>%
  mutate(
    POS = factor(POS, levels = levels(survival_df$POS))  # match Cox model levels
  ) %>%
  filter(!is.na(POS))  # drops any positions not in survival_df

tier_df <- tier_df %>%
  mutate(
    cox_risk_score = predict(cox_model, newdata = ., type = "lp"),
    injury_risk_tier = factor(ntile(cox_risk_score, 3),
                              labels = c("Low", "Medium", "High"))
  )

#applying to NEW players - risk category - risk of earlier 1st injury NOT average days/active season

new_players <- tibble(
  playerID = c("NEW001", "NEW002"),
  POS = c("SS", "P"),
  height = c(72, 74),
  weight = c(190, 215)
)

new_players <- new_players %>%
  mutate(
    POS = factor(POS, levels = levels(survival_df$POS))
  )
new_players <- new_players %>%
  mutate(
    cox_risk_score = predict(cox_model, newdata = new_players, type = "lp")
  )
risk_cutpoints <- quantile(
  tier_df$cox_risk_score,
  probs = c(1/3, 2/3)
)
new_players <- new_players %>%
  mutate(
    injury_risk_tier = case_when(
      cox_risk_score <= risk_cutpoints[1] ~ "Low",
      cox_risk_score <= risk_cutpoints[2] ~ "Medium",
      TRUE ~ "High"
    )
  )

#redoing cox model as train/test

# ============================================================
# Cox PH model from scratch + risk tiers (15/85 percentiles)
# Train/test split: 70% / 30% (by player)
# Covariates: birthYear + height + weight + POS
# Outcome: time to FIRST injury (months), censor at finalGame
# ============================================================

library(dplyr)
library(lubridate)
library(survival)

set.seed(498)

# ----------------------------
# 0) Clean POS columns if needed (keeps Fielding POS)
# ----------------------------
if ("POS.y" %in% names(youngMaster_pos)) {
  youngMaster_pos <- youngMaster_pos %>% rename(POS = POS.y) %>% select(-any_of("POS.x"))
}
if ("POS.y" %in% names(final_injuries_pos)) {
  final_injuries_pos <- final_injuries_pos %>% rename(POS = POS.y) %>% select(-any_of("POS.x"))
}

# ----------------------------
# 1) Build player-level event times: FIRST injury only
# Requires: final_injuries_pos has playerID.x + injury_Age (months) already computed
# ----------------------------
injury_time <- final_injuries_pos %>%
  mutate(playerID = as.character(playerID.x)) %>%
  filter(!is.na(playerID), is.finite(injury_Age), !is.na(injury_Age), injury_Age >= 0) %>%
  group_by(playerID) %>%
  summarise(
    time_months = min(injury_Age),  # time to FIRST injury
    event = 1L,
    .groups = "drop"
  )

# ----------------------------
# 2) Build player-level censor times from birthDate to finalGame (months)
# Requires: youngMaster_pos has playerID, birthDate, finalGame
# ----------------------------
censor_time <- youngMaster_pos %>%
  mutate(
    playerID  = as.character(playerID),
    birthDate = as.Date(birthDate),
    finalGame = as.Date(finalGame)
  ) %>%
  filter(!is.na(playerID), !is.na(birthDate), !is.na(finalGame)) %>%
  mutate(
    time_months = time_length(interval(birthDate, finalGame), "month"),
    event = 0L
  ) %>%
  select(playerID, time_months, event)

# ----------------------------
# 3) Combine (injury overrides censor), then add covariates
# ----------------------------
survival_df <- censor_time %>%
  left_join(injury_time, by = "playerID", suffix = c("_cens", "_inj")) %>%
  transmute(
    playerID,
    time_months = if_else(!is.na(time_months_inj), time_months_inj, time_months_cens),
    event       = if_else(!is.na(event_inj),       event_inj,       event_cens)
  ) %>%
  left_join(
    youngMaster_pos %>%
      mutate(
        playerID = as.character(playerID),
        debut = as.Date(debut),
        debut_year = lubridate::year(debut)
      ) %>%
      select(playerID, debut_year, height, weight, POS),
    by = "playerID"
  ) %>%
  filter(
    is.finite(time_months), !is.na(time_months), time_months > 0,
    !is.na(debut_year), !is.na(height), !is.na(weight),
    !is.na(POS), POS != "DH"  # remove DH
  ) %>%
  mutate(
    POS = droplevels(factor(POS))
  )

# ----------------------------
# 4) Train/Test split (70/30) BY PLAYER
# ----------------------------
all_players <- unique(survival_df$playerID)
train_players <- sample(all_players, size = floor(0.70 * length(all_players)))

train_df <- survival_df %>% filter(playerID %in% train_players)
test_df  <- survival_df %>% filter(!playerID %in% train_players)

# Ensure test POS uses the same levels as train POS
test_df <- test_df %>%
  mutate(POS = factor(POS, levels = levels(train_df$POS))) %>%
  filter(!is.na(POS))  # drops unseen POS levels in test (rare, but safe)

# ----------------------------
# 5) Fit Cox PH model on TRAIN
# ----------------------------
cox_train <- coxph(
  Surv(time_months, event) ~ debut_year + height + weight + POS,
  data = train_df,
  x = TRUE
)

train_summary  <- summary(cox_train)
print(train_summary)

# ----------------------------
# 6) Key TRAIN model statistics
# ----------------------------
cat("\n===== TRAIN MODEL STATS =====\n")
cat("N (train):", nrow(train_df), "\n")
cat("Events (train):", sum(train_df$event), "\n")
cat("Concordance (train):", train_summary$concordance[1], "\n")
cat("Concordance SE (train):", train_summary$concordance[2], "\n")
cat("LogLik (train):", train_summary$loglik[2], "\n")
cat("LR test p (train):", signif(train_summary$logtest["pvalue"], 4), "\n")
cat("Wald test p (train):", signif(train_summary$waldtest["pvalue"], 4), "\n")
cat("Score test p (train):", signif(train_summary$sctest["pvalue"], 4), "\n")
cat("AIC (train):", AIC(cox_train), "\n")

# PH assumption diagnostics (train)
ph_train <- cox.zph(cox_train)
cat("\n===== PH TEST (TRAIN) =====\n")
print(ph_train)
# plot(ph_train)  # optional

# ----------------------------
# 7) Evaluate on TEST (out-of-sample)
# - Predict linear predictor (risk score)
# - Compute TEST concordance (C-index)
# ----------------------------
test_df <- test_df %>%
  mutate(
    risk_score = predict(cox_train, newdata = test_df, type = "lp")
  )

# C-index on test using survival::concordance
c_test <- concordance(Surv(time_months, event) ~ I(-risk_score), data = test_df)

cat("\n===== TEST MODEL STATS =====\n")
cat("N (test):", nrow(test_df), "\n")
cat("Events (test):", sum(test_df$event), "\n")
cat("Concordance (test):", c_test$concordance, "\n")
cat("Concordance SE (test):", sqrt(c_test$var), "\n")

# ----------------------------
# 8) Risk tiers using TRAIN percentiles (15/85 cutpoints)
# - Low: <= 15th percentile
# - High: >= 85th percentile
# - Medium: everything else
# ----------------------------
train_df <- train_df %>%
  mutate(
    risk_score = predict(cox_train, newdata = train_df, type = "lp")
  )

cut_lo <- quantile(train_df$risk_score, probs = 0.15, na.rm = TRUE)
cut_hi <- quantile(train_df$risk_score, probs = 0.85, na.rm = TRUE)

assign_tier <- function(x, lo, hi) {
  case_when(
    x <= lo ~ "Low",
    x >= hi ~ "High",
    TRUE    ~ "Medium"
  )
}

train_df <- train_df %>%
  mutate(
    risk_tier = factor(assign_tier(risk_score, cut_lo, cut_hi),
                       levels = c("Low", "Medium", "High"))
  )

test_df <- test_df %>%
  mutate(
    risk_tier = factor(assign_tier(risk_score, cut_lo, cut_hi),
                       levels = c("Low", "Medium", "High"))
  )

# ----------------------------
# 9) Tier summaries (train + test)
# ----------------------------
tier_summary <- function(df, label) {
  df %>%
    group_by(risk_tier) %>%
    summarise(
      dataset = label,
      n = n(),
      events = sum(event),
      event_rate = mean(event),
      median_time_months = median(time_months),
      .groups = "drop"
    )
}

tier_results <- bind_rows(
  tier_summary(train_df, "Train"),
  tier_summary(test_df, "Test")
)

print(tier_results)

# ----------------------------
# 10) Optional: KM curves by tier (TEST)
# ----------------------------
km_tier_test <- survfit(Surv(time_months, event) ~ risk_tier, data = test_df)

plot(
  km_tier_test,
  xlim = c(225, max(test_df$time_months, na.rm = TRUE)),
  xlab = "Age at First Injury (in months)",
  ylab = "Injury-free probability",
  main = "Kaplan–Meier by Risk Tier (Test Set)",
  col = c("#222c67","#67bed9","#d12028" ),
  lwd = 2
)

legend(
  "bottomleft",
  legend = levels(test_df$risk_tier),
  col = c("#222c67","#67bed9","#d12028"),
  lty = 1,
  lwd = 2,
  cex = 0.9
)

#run again without debut year

# ----------------------------
# 1) Build player-level event times: FIRST injury only
# Requires: final_injuries_pos has playerID.x + injury_Age (months) already computed
# ----------------------------
injury_time <- final_injuries_pos %>%
  mutate(playerID = as.character(playerID.x)) %>%
  filter(!is.na(playerID), is.finite(injury_Age), !is.na(injury_Age), injury_Age >= 0) %>%
  group_by(playerID) %>%
  summarise(
    time_months = min(injury_Age),  # time to FIRST injury
    event = 1L,
    .groups = "drop"
  )

# ----------------------------
# 2) Build player-level censor times from birthDate to finalGame (months)
# Requires: youngMaster_pos has playerID, birthDate, finalGame
# ----------------------------
censor_time <- youngMaster_pos %>%
  mutate(
    playerID  = as.character(playerID),
    birthDate = as.Date(birthDate),
    finalGame = as.Date(finalGame)
  ) %>%
  filter(!is.na(playerID), !is.na(birthDate), !is.na(finalGame)) %>%
  mutate(
    time_months = time_length(interval(birthDate, finalGame), "month"),
    event = 0L
  ) %>%
  select(playerID, time_months, event)

# ----------------------------
# 3) Combine (injury overrides censor), then add covariates
# ----------------------------
survival_df <- censor_time %>%
  left_join(injury_time, by = "playerID", suffix = c("_cens", "_inj")) %>%
  transmute(
    playerID,
    time_months = if_else(!is.na(time_months_inj), time_months_inj, time_months_cens),
    event       = if_else(!is.na(event_inj),       event_inj,       event_cens)
  ) %>%
  left_join(
    youngMaster_pos %>%
      mutate(
        playerID = as.character(playerID)
      ) %>%
      select(playerID, height, weight, POS),
    by = "playerID"
  ) %>%
  filter(
    is.finite(time_months), !is.na(time_months), time_months > 0,
    !is.na(height), !is.na(weight),
    !is.na(POS), POS != "DH"  # remove DH
  ) %>%
  mutate(
    POS = droplevels(factor(POS))
  )

# ----------------------------
# 4) Train/Test split (70/30) BY PLAYER
# ----------------------------
all_players <- unique(survival_df$playerID)
train_players <- sample(all_players, size = floor(0.70 * length(all_players)))

train_nody_df <- survival_df %>% filter(playerID %in% train_players)
test_nody_df  <- survival_df %>% filter(!playerID %in% train_players)

# Ensure test POS uses the same levels as train POS
test_nody_df <- test_df %>%
  mutate(POS = factor(POS, levels = levels(train_nody_df$POS))) %>%
  filter(!is.na(POS))  # drops unseen POS levels in test (rare, but safe)

# ----------------------------
# 5) Fit Cox PH model on TRAIN
# ----------------------------
cox_train_nody <- coxph(
  Surv(time_months, event) ~ height + weight + POS,
  data = train_nody_df,
  x = TRUE
)

train_summary_nody <- summary(cox_train_nody)
print(train_summary_nody)

# ----------------------------
# 6) Key TRAIN model statistics
# ----------------------------
cat("\n===== TRAIN MODEL STATS =====\n")
cat("N (train):", nrow(train_nody_df), "\n")
cat("Events (train):", sum(train_nody_df$event), "\n")
cat("Concordance (train):", train_summary_nody $concordance[1], "\n")
cat("Concordance SE (train):", train_summary_nody $concordance[2], "\n")
cat("LogLik (train):", train_summary_nody $loglik[2], "\n")
cat("LR test p (train):", signif(train_summary_nody $logtest["pvalue"], 4), "\n")
cat("Wald test p (train):", signif(train_summary_nody $waldtest["pvalue"], 4), "\n")
cat("Score test p (train):", signif(train_summary_nody $sctest["pvalue"], 4), "\n")
cat("AIC (train):", AIC(cox_train_nody), "\n")

# PH assumption diagnostics (train)
ph_train_nody <- cox.zph(cox_train_nody)
cat("\n===== PH TEST (TRAIN) =====\n")
print(ph_train_nody)
plot(ph_train_nody)  # optional

# ----------------------------
# 7) Evaluate on TEST (out-of-sample)
# - Predict linear predictor (risk score)
# - Compute TEST concordance (C-index)
# ----------------------------
test_nody_df <- test_df %>%
  mutate(
    risk_score = predict(cox_train_nody, newdata = test_df, type = "lp")
  )

# C-index on test using survival::concordance
c_test_nody <- concordance(Surv(time_months, event) ~ I(-risk_score), data = test_nody_df)

cat("\n===== TEST MODEL STATS =====\n")
cat("N (test):", nrow(test_nody_df), "\n")
cat("Events (test):", sum(test_nody_df$event), "\n")
cat("Concordance (test):", c_test_nody$concordance, "\n")
cat("Concordance SE (test):", sqrt(c_test_nody$var), "\n")

# ----------------------------
# 8) Risk tiers using TRAIN percentiles (15/85 cutpoints)
# - Low: <= 15th percentile
# - High: >= 85th percentile
# - Medium: everything else
# ----------------------------
train_nody_df <- train_nody_df %>%
  mutate(
    risk_score = predict(cox_train_nody, newdata = train_nody_df, type = "lp")
  )

cut_lo <- quantile(train_nody_df$risk_score, probs = 0.15, na.rm = TRUE)
cut_hi <- quantile(train_nody_df$risk_score, probs = 0.85, na.rm = TRUE)

assign_tier <- function(x, lo, hi) {
  case_when(
    x <= lo ~ "Low",
    x >= hi ~ "High",
    TRUE    ~ "Medium"
  )
}

train_nody_df <- train_nody_df %>%
  mutate(
    risk_tier = factor(assign_tier(risk_score, cut_lo, cut_hi),
                       levels = c("Low", "Medium", "High"))
  )

test_nody_df <- test_nody_df %>%
  mutate(
    risk_tier = factor(assign_tier(risk_score, cut_lo, cut_hi),
                       levels = c("Low", "Medium", "High"))
  )

# ----------------------------
# 9) Tier summaries (train + test)
# ----------------------------
tier_summary_nody <- function(df, label) {
  df %>%
    group_by(risk_tier) %>%
    summarise(
      dataset = label,
      n = n(),
      events = sum(event),
      event_rate = mean(event),
      median_time_months = median(time_months),
      .groups = "drop"
    )
}

tier_results_nody <- bind_rows(
  tier_summary(train_nody_df, "Train"),
  tier_summary(test_nody_df, "Test")
)

print(tier_results_nody)

# ----------------------------
# 10) Optional: KM curves by tier (TEST)
# ----------------------------
km_tier_test <- survfit(Surv(time_months, event) ~ risk_tier, data = test_nody_df)

plot(
  km_tier_test,
  xlim = c(225, max(test_df$time_months, na.rm = TRUE)),
  xlab = "Age at First Injury (in months) - Debut Year NOT included",
  ylab = "Injury-free probability",
  main = "Kaplan–Meier by Risk Tier (Test Set)",
  col = c("#222c67","#67bed9","#d12028" ),
  lwd = 2
)

legend(
  "bottomleft",
  legend = levels(test_df$risk_tier),
  col = c("#222c67","#67bed9","#d12028"),
  lty = 1,
  lwd = 2,
  cex = 0.9
)

#summary metrics comparing two models
model_comparison <- tibble(
  model = c("With debut year", "Without debut year"),
  
  formula = c(
    "debut_year + height + weight + POS",
    "height + weight + POS"
  ),
  
  train_n = c(
    nrow(train_df),
    nrow(train_df)
  ),
  
  train_events = c(
    sum(train_df$event),
    sum(train_df$event)
  ),
  
  test_n = c(
    nrow(test_df),
    nrow(test_df)
  ),
  
  test_events = c(
    sum(test_df$event),
    sum(test_df$event)
  ),
  
  train_c_index = c(
    train_summary$concordance[1],
    train_summary_nody$concordance[1]
  ),
  
  test_c_index = c(
    c_test$concordance,
    c_test_nody$concordance
  ),
  
  test_c_index_se = c(
    sqrt(c_test$var),
    sqrt(c_test_nody$var)
  ),
  
  aic_train = c(
    AIC(cox_train),
    AIC(cox_train_nody)
  ),
  
  loglik_train = c(
    train_summary $loglik[2],
    train_summary_nody $loglik[2]
  ),
  
  ph_global_p = c(
    ph_train$table["GLOBAL", "p"],
    ph_train_nody$table["GLOBAL", "p"]
  )
)

model_comparison

#save to file
write.csv(
  model_comparison,
  "cox_model_comparison.csv",
  row.names = FALSE
)

#********************************************************************************

#clarify data labels, especially position. 
#start KM injury free survival by position graph at 200 months
#add injury data and performance data to youngMaster dataset
#try clustering model - low, medium, high risk of injury
#need % of season completion - estimate of workload? pitches thrown by pitchers
