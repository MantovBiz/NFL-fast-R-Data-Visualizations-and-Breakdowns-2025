library(nflreadr)
library(nflfastR)
library(tidyverse)
library(gt)
library(gtExtras)
library(webshot2)

# ── Load Data ────────────────────────────────────────────────────────────────
pbp <- load_participation(2025, include_pbp = TRUE)

# Keep only plays that are passes or rushes
pbp_plays <- pbp %>%
  filter(pass == 1 | rush == 1)

# ── Offense: Personnel Groups ─────────────────────────────────────────────────
# Extract number of RBs/FBs and TEs from the personnel string, then combine into a grouping label (e.g. "11" = 1 RB, 1 TE)
pbp_plays <- pbp_plays %>%
  mutate(
    rb = rowSums(cbind(
      as.numeric(str_extract(offense_personnel, "(?<= )\\d+(?= RB)")),
      as.numeric(str_extract(offense_personnel, "(?<= )\\d+(?= FB)"))
    ), na.rm = TRUE),
    te = as.numeric(str_extract(offense_personnel, "(?<= )\\d+(?= TE)")) %>% replace_na(0),
    personnel = paste0(rb, te)   # e.g. "11", "12", "21"
  )

# Summarize offensive personnel usage by team
offense_summary <- pbp_plays %>%
  group_by(posteam, personnel) %>%
  summarise(
    Plays   = n(),
    YPP     = mean(yards_gained, na.rm = TRUE),
    EPA     = mean(epa,          na.rm = TRUE),
    Success = mean(success,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# ── Offense Table: Chicago Bears ──────────────────────────────────────────────
offense_table <- offense_summary %>%
  filter(posteam == "CHI") %>%
  select(team_wordmark, personnel, Plays, EPA, Success, YPP) %>%
  arrange(desc(Plays)) %>%
  gt() %>%
  gt_img_rows(columns = team_wordmark) %>%
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>%
  fmt_percent(columns = c(Success), decimals = 2) %>%
  gt_theme_espn() %>%
  opt_row_striping() %>%
  tab_header(title = "Chicago Bears — Offensive Personnel Breakdown (2025)") %>%
  tab_caption(caption = "By Manny Tovar | Data via NFLFastR") %>%
  cols_label(
    team_wordmark = "Team",
    personnel = "Personnel Group"
  )

# ── Defense: Coverage Types ───────────────────────────────────────────────────
pbp_coverage <- pbp %>%
  filter(pass == 1) %>%
  mutate(defense_coverage_type = case_when(
    defense_coverage_type == "COVER_3" ~ "Cover 3",
    defense_coverage_type == "COVER_4" ~ "Cover 4",
    defense_coverage_type == "COVER_1" ~ "Cover 1",
    defense_coverage_type == "COVER_0" ~ "Cover 0",
    defense_coverage_type == "COVER_9" ~ "Cover 9",
    defense_coverage_type == "COVER_6" ~ "Cover 6",
    defense_coverage_type == "2_MAN"   ~ "Cover 2 Man",
    defense_coverage_type == "COMBO"   ~ "Combo Coverage",
    TRUE                               ~ "Other"
  ))

# Summarize defensive coverage usage by team
defense_summary <- pbp_coverage %>%
  filter(defense_coverage_type != "Other") %>%
  group_by(defteam, defense_coverage_type) %>%
  summarise(
    Plays   = n(),
    EPA     = mean(epa,          na.rm = TRUE),
    Success = mean(success,      na.rm = TRUE),
    YPP     = mean(yards_gained, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))

# ── Defense Table: Chicago Bears ──────────────────────────────────────────────
defense_table <- defense_summary %>%
  filter(defteam == "CHI") %>%
  select(team_wordmark, defense_coverage_type, Plays, EPA, Success, YPP) %>%
  arrange(desc(Plays)) %>%
  gt() %>%
  gt_img_rows(columns = team_wordmark) %>%
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>%
  fmt_percent(columns = c(Success), decimals = 2) %>%
  gt_theme_espn() %>%
  opt_row_striping() %>%
  tab_header(title = "Chicago Bears — Defensive Coverage Breakdown (2025)") %>%
  tab_caption(caption = "By Manny Tovar | Data via NFLFastR") %>%
  cols_label(
    team_wordmark = "Team",
    defense_coverage_type = "Coverage Type"
  )

# ── View Tables ───────────────────────────────────────────────────────────────
offense_table
defense_table

# ── Season type comparison ────────────────────────────────────────────────────
season_comparison <- pbp_plays %>%
  filter(posteam == "CHI") %>%
  mutate(season_type = if_else(season_type == "REG", "Regular Season", "Playoffs")) %>%
  group_by(season_type, personnel) %>%
  summarise(
    Plays   = n(),
    EPA     = mean(epa,          na.rm = TRUE),
    Success = mean(success,      na.rm = TRUE),
    YPP     = mean(yards_gained, na.rm = TRUE),
    .groups = "drop"
  ) %>%  # drop tiny sample sizes
  arrange(season_type, desc(Plays))

# View it
season_comparison
# ── Offense: Regular Season vs Playoffs ───────────────────────────────────────
offense_season_comparison <- pbp_plays %>%
  filter(posteam == "CHI") %>%
  mutate(season_type = if_else(season_type == "REG", "Regular Season", "Playoffs")) %>%
  group_by(season_type, personnel) %>%
  summarise(
    Plays   = n(),
    EPA     = mean(epa,          na.rm = TRUE),
    Success = mean(success,      na.rm = TRUE),
    YPP     = mean(yards_gained, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(season_type, desc(Plays))

# ── Offense: Regular Season vs Playoffs table ─────────────────────────────────
chi_wordmark <- teams_colors_logos %>%
  filter(team_abbr == "CHI") %>%
  pull(team_wordmark)

offense_season_comparison %>%
  mutate(Team = chi_wordmark) %>%
  select(Team, season_type, personnel, Plays, EPA, Success, YPP) %>%
  gt(groupname_col = "season_type") %>%
  gt_img_rows(columns = Team) %>%
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>%
  fmt_percent(columns = c(Success), decimals = 2) %>%
  gt_theme_espn() %>%
  opt_row_striping() %>%
  tab_header(title = "Chicago Bears — Offensive Personnel: Regular Season vs Playoffs (2025)") %>%
  tab_caption(caption = "By Peyton Berger | Data: FTN via nflverse") %>%
  cols_label(
    Team = "Team",
    personnel = "Personnel Group"
  )

# ── Defense Season type comparison ───────────────────────────────────────────
defense_season_comparison %>%
  mutate(Team = chi_wordmark) %>%
  select(Team, season_type, defense_coverage_type, Plays, EPA, Success, YPP) %>%
  gt(groupname_col = "season_type") %>%
  gt_img_rows(columns = Team) %>%
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>%
  fmt_percent(columns = c(Success), decimals = 2) %>%
  gt_theme_espn() %>%
  opt_row_striping() %>%
  tab_header(title = "Chicago Bears — Defensive Coverage: Regular Season vs Playoffs (2025)") %>%
  tab_caption(caption = "By Manny Tovar | Data via NFL") %>%
  cols_label(
    Team = "Team",
    defense_coverage_type = "Coverage Type"
  )
