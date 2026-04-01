# ============================================================
# AFL Predictions — Bold & Calibrated
#   - Leak-free, shrunken HGA
#   - Difference features
#   - Ridge regression + logistic calibration
#   - Temperature scaling to sharpen probabilities
#   - Same outputs as your pipeline for rd = 0
# ============================================================

# ---------------------------
# 1) Libraries & Parameters
# ---------------------------
suppressPackageStartupMessages({
  library(fitzRoy)
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(glmnet)
})

# Round to predict
rd <- 4

# Knobs you can tune
LAST_N_GAMES <- 6       # rolling horizon (if you rebuild ratings upstream)
HGA_ALPHA    <- 12      # prior strength for HGA shrinkage (higher => more shrink)
HGA_CAP      <- 0.12    # cap absolute HGA (probability advantage) at ±0.12
TEMP         <- 0.85    # temperature scaling for sharpening (T<1 => bolder)
PROB_FLOOR   <- 0.02    # clamp probabilities
PROB_CEIL    <- 0.98

set.seed(2026)

# ---------------------------------------
# 2) Expect `result_orig_withstats25` ready
#    If not present, try loading the RDS
# ---------------------------------------
if (!exists("result_orig_withstats25")) {
  if (file.exists("result_withstats_18to25.rds")) {
    result_orig_withstats25 <- readRDS("result_withstats_18to25.rds")
  } else {
    stop("`result_orig_withstats25` not found and RDS is missing. Load or build it before running.")
  }
}

# Ensure types
result_orig_withstats25 <- result_orig_withstats25 %>%
  mutate(Date = as.Date(Date))

# Venue presence check — if Venue was accidentally dropped,
# we try to reconstruct it from a lightweight results fetch.
if (!"Venue" %in% names(result_orig_withstats25)) {
  message("`Venue` not found in result_orig_withstats25. Attempting to reattach from results...")
  # Fetch historical results and map by (Game) if available
  # (Note: if your Game IDs don’t align, you’ll need your original join here)
  results_2018_2026 <- tryCatch({
    bind_rows(
      fitzRoy::fetch_results_afltables(2018:2025) %>%
        select(Game, Date, Home.Team, Away.Team, Venue),
      fitzRoy::fetch_results(2026) %>%
        transmute(
          Game = matchId,
          Date = as.Date(match.date),
          Home.Team = match.homeTeam.name,
          Away.Team = match.awayTeam.name,
          Venue = venue.name
        )
    ) %>% distinct(Game, .keep_all = TRUE)
  }, error = function(e) NULL)
  if (!is.null(results_2018_2026)) {
    result_orig_withstats25 <- result_orig_withstats25 %>%
      left_join(results_2018_2026 %>% select(Game, Venue), by = "Game")
  }
  if (!"Venue" %in% names(result_orig_withstats25)) {
    stop("Could not construct `Venue`. Ensure Venue is present in `result_orig_withstats25`.")
  }
}


a25 <- fitzRoy::fetch_player_stats(2026)

a25_clean_home <- a25 %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
  summarise(hInside.50s      = sum(inside50s, na.rm = T),
            hMarks.Inside.50 = sum(marksInside50, na.rm = T),
            hTackles         = sum(tackles, na.rm = T),
            htr = sum(ratingPoints, na.rm = T),
            hTacklesi50         = sum(tacklesInside50, na.rm = T),
            hinterceptMarks         = sum(extendedStats.interceptMarks, na.rm = T),
            hOne.Percenters  = sum(onePercenters, na.rm = T),
            hcontestedPossessions = sum(contestedPossessions, na.rm = T),
            hshotsAtGoal = sum(shotsAtGoal, na.rm = T),
            hclearances = sum(clearances.totalClearances, na.rm = T),
            hhitouts = sum(extendedStats.hitoutsToAdvantage, na.rm = T),
            hturnovers = sum(turnovers, na.rm = T) ) %>%
  filter(home.team.name == team.name) %>%
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
         "Away.team"=  away.team.name, htr, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50,hinterceptMarks,
         hOne.Percenters, hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts, hturnovers  )

a25_clean_away <- a25 %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
  summarise(aInside.50s      = sum(inside50s, na.rm = T),
            aMarks.Inside.50 = sum(marksInside50, na.rm = T),
            aTackles         = sum(tackles, na.rm = T),
            atr = sum(ratingPoints, na.rm = T),
            aTacklesi50         = sum(tacklesInside50, na.rm = T),
            ainterceptMarks         = sum(extendedStats.interceptMarks, na.rm = T),
            aOne.Percenters  = sum(onePercenters, na.rm = T),
            acontestedPossessions = sum(contestedPossessions, na.rm = T),
            ashotsAtGoal = sum(shotsAtGoal, na.rm = T),
            aclearances = sum(clearances.totalClearances, na.rm = T),
            ahitouts = sum(extendedStats.hitoutsToAdvantage, na.rm = T),
            aturnovers = sum(turnovers, na.rm = T)  ) %>%
  filter(away.team.name == team.name) %>%
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
         "Away.team"=  away.team.name, atr, aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50,ainterceptMarks,
         aOne.Percenters, acontestedPossessions, ashotsAtGoal, aclearances, ahitouts, aturnovers)

a25_clean <- left_join(a25_clean_home, a25_clean_away, by = c("Season", "Round", "Date", "Venue", "Home.team", "Away.team"))

stats25_sum <- a25_clean %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "western bulldogs", "Footscray", Home.team),
         Away.team = ifelse(tolower(Away.team) == "western bulldogs", "Footscray", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "gold coast suns", "Gold Coast", Home.team),
         Away.team = ifelse(tolower(Away.team) == "gold coast suns", "Gold Coast", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "west coast eagles", "West Coast", Home.team),
         Away.team = ifelse(tolower(Away.team) == "west coast eagles", "West Coast", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "sydney swans", "Sydney", Home.team),
         Away.team = ifelse(tolower(Away.team) == "sydney swans", "Sydney", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "adelaide crows", "Adelaide", Home.team),
         Away.team = ifelse(tolower(Away.team) == "adelaide crows", "Adelaide", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "geelong cats", "Geelong", Home.team),
         Away.team = ifelse(tolower(Away.team) == "geelong cats", "Geelong", Away.team)) %>%
  mutate(Home.team = ifelse(tolower(Home.team) == "gws giants", "GWS", Home.team),
         Away.team = ifelse(tolower(Away.team) == "gws giants", "GWS", Away.team)) %>%
  ungroup() %>%
  select(-Round)

# dput(names(results_25))
# 
results_25 <- fitzRoy::fetch_results(2026)  %>%
  mutate(    Margin = homeTeamScore.matchScore.totalScore-awayTeamScore.matchScore.totalScore)  %>% 
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "western bulldogs", "Footscray",   match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "western bulldogs", "Footscray",   match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "gold coast suns", "Gold Coast",   match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "gold coast suns", "Gold Coast",   match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "west coast eagles", "West Coast", match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "west coast eagles", "West Coast", match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "sydney swans", "Sydney",          match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "sydney swans", "Sydney",          match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "adelaide crows", "Adelaide",      match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "adelaide crows", "Adelaide",      match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "geelong cats", "Geelong",         match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "geelong cats", "Geelong",         match.awayTeam.name)) %>%
  mutate(match.homeTeam.name = ifelse(tolower(match.homeTeam.name) == "gws giants", "GWS",               match.homeTeam.name),
         match.awayTeam.name = ifelse(tolower(match.awayTeam.name) == "gws giants", "GWS",               match.awayTeam.name))%>% 
  select(
    Game = matchId,
    Date = match.date,
    Round = round.name,
    Home.Team = match.homeTeam.name,
    Home.Goals = homeTeamScore.matchScore.goals,
    Home.Behinds = homeTeamScore.matchScore.behinds,
    Home.Points = homeTeamScore.matchScore.totalScore,
    Away.Team = match.awayTeam.name,
    Away.Goals = awayTeamScore.matchScore.goals,
    Away.Behinds = awayTeamScore.matchScore.behinds,
    Away.Points = awayTeamScore.matchScore.totalScore,
    Venue = venue.name,
    Margin,
    Season = round.year,
    Round.Type = round.name,
    Round.Number = round.roundNumber
  ) %>%
  mutate(
    Season = as.numeric(Season),
    # Ensure Round.Number has leading zeros
    Round.Number = ifelse(Round.Number < 10, paste0("0", Round.Number), as.character(Round.Number)),
    # Create seas_rnd column
    seas_rnd = as.numeric(paste0(Season, Round.Number))
  )


# results_25 <- fitzRoy::fetch_results_afltables(2025) %>%
#   mutate(Round.Number = ifelse(Round.Number < 10, paste0("0",Round.Number), Round.Number)) %>%
#   mutate(seas_rnd =as.numeric(paste0(Season, Round.Number)))
# # dput(names(results_25))
result_25_withstats <- results_25 %>%
  select(-Venue) %>%
  mutate(Date = as.Date(Date)) %>% 
  left_join(stats25_sum, by = c("Date",
                                "Home.Team"= "Home.team",
                                "Season",
                                "Away.Team"="Away.team"))  %>%
  # filter(Round.Type != "Finals")
  filter(Game != 14786)




result_orig_withstats25 <- rbind(result_orig_withstats25, result_25_withstats) 









# ---------------------------------------
# 3) Leak-free, shrunken HGA (per row)
# ---------------------------------------
cleaned_data <- result_orig_withstats25 %>%
  arrange(Date)

# League-average home win rate (as fallback for HGA)
league_home_rate <- mean(cleaned_data$Margin > 0, na.rm = TRUE)
if (is.na(league_home_rate) || !is.finite(league_home_rate)) league_home_rate <- 0.55

# Compute cumulative games & wins BEFORE each match at (Home.Team, Venue)
hga_by_row <- cleaned_data %>%
  group_by(Home.Team, Venue) %>%
  mutate(
    cg = lag(cumsum(!is.na(Margin)), default = 0L),                # games before this match
    cw = lag(cumsum(if_else(Margin > 0, 1L, 0L)), default = 0L)    # wins before this match
  ) %>%
  ungroup() %>%
  select(Game, Home.Team, Venue, cg, cw)

# Shrink to league baseline using a Beta prior, then cap
hga_by_row <- hga_by_row %>%
  mutate(
    HGA_pre = if_else(
      cg > 0,
      (cw + HGA_ALPHA * league_home_rate) / (cg + HGA_ALPHA),
      league_home_rate
    ) - 0.5,
    HGA_pre = pmax(pmin(HGA_pre, HGA_CAP), -HGA_CAP)
  )

cleaned_data <- cleaned_data %>%
  left_join(hga_by_row %>% select(Game, HGA_pre), by = "Game") %>%
  mutate(HomeGroundAdvantage = HGA_pre) %>%
  select(-HGA_pre) %>% 
  mutate(homeopp_tr = replace_na(atr, 0),
         awayopp_tr = replace_na(htr, 0))
########
# ratings
# Function to calculate rolling averages while ensuring only past data is used
calculate_rolling_avg <- function(df, team_col, metrics, last_n_games = 4) {
  df %>%
    arrange(Date) %>%
    group_by(.data[[team_col]]) %>%
    mutate(
      across(
        all_of(metrics),
        ~ lag(rollapplyr(.x, width = last_n_games, FUN = mean, fill = NA, align = "right")),
        .names = "{.col}_Avg"
      )
    ) %>%
    ungroup()
}

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("htr", "hshotsAtGoal","hcontestedPossessions", "hMarks.Inside.50", "hTacklesi50", "hclearances")) #%>%
# mutate(Home.OffensiveRating = Home.ShotsAtGoal_Avg*2 + Home.MarksInside50_Avg*1 + Home.Clearances_Avg*0.2)

cleaned_data <- cleaned_data %>%
  calculate_rolling_avg(., "Away.Team", c("atr","ashotsAtGoal", "aMarks.Inside.50","aTacklesi50", "aclearances")) #%>%
# mutate(Away.OffensiveRating =  Away.ShotsAtGoal_Avg*2 + Away.MarksInside50_Avg*1 + Away.Clearances_Avg*0.2)

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("homeopp_tr","hTackles", "hcontestedPossessions", "hinterceptMarks")) #%>%
# mutate(Home.DefensiveRating =  Home.Tackles_Avg*0.8 + Home.ContestedPossessions_Avg*0.15 + Home.InterceptMarks_Avg*3 + Away.ShotsAtGoal_Avg*-1)
cleaned_data <- cleaned_data %>%
  calculate_rolling_avg(., "Away.Team", c("awayopp_tr","aTackles", "acontestedPossessions", "ainterceptMarks")) #%>%
# mutate(Away.DefensiveRating =   Away.Tackles_Avg*0.8 + Away.ContestedPossessions_Avg*0.15 + Away.InterceptMarks_Avg*3)

horlm_model <- lm(Margin ~  hshotsAtGoal_Avg +  hclearances_Avg, data = cleaned_data)
home_off_rats <- predict(horlm_model, newdata = cleaned_data)
horlm_model <- lm(Margin ~ hcontestedPossessions_Avg  + ashotsAtGoal_Avg, data = cleaned_data)
home_def_rats <- predict(horlm_model, newdata = cleaned_data)

horlm_model <- lm(-Margin ~   ashotsAtGoal_Avg + aMarks.Inside.50_Avg + aclearances_Avg, data = cleaned_data)
away_off_rats <- predict(horlm_model, newdata = cleaned_data)
horlm_model <- lm(-Margin ~   aTackles_Avg + acontestedPossessions_Avg + ainterceptMarks_Avg , data = cleaned_data)
away_def_rats <- predict(horlm_model, newdata = cleaned_data)
# summary(horlm_model)

cleaned_data <- cleaned_data %>% 
  mutate(Home.DefensiveRating = home_def_rats, 
         Home.OffensiveRating = home_off_rats, 
         Away.DefensiveRating = away_def_rats, 
         Away.OffensiveRating = away_off_rats)


# ---------------------------------------
# 4) Train with difference features + Ridge
# ---------------------------------------
# Ensure ratings exist. If not, stop with a helpful message.
needed_cols <- c("Home.OffensiveRating","Away.OffensiveRating",
                 "Home.DefensiveRating","Away.DefensiveRating")
missing_cols <- setdiff(needed_cols, names(cleaned_data))
if (length(missing_cols) > 0) {
  stop(paste0(
    "Missing rating columns: ", paste(missing_cols, collapse = ", "),
    "\nPlease keep your earlier rating-building steps before this script,\n",
    "or ask me to integrate team-central rolling & rating code here."
  ))
}

training_data <- cleaned_data %>%
  filter(Round.Type != "Finals") %>%
  filter(
    !is.na(Home.OffensiveRating), !is.na(Away.OffensiveRating),
    !is.na(Home.DefensiveRating), !is.na(Away.DefensiveRating),
    !is.na(HomeGroundAdvantage),
    !is.na(Margin)
  ) %>%
  mutate(
    OffDiff = Home.OffensiveRating - Away.OffensiveRating,
    DefDiff = Home.DefensiveRating - Away.DefensiveRating
  )

# Ridge regression on margin
x_tr <- model.matrix(Margin ~ OffDiff + DefDiff + HomeGroundAdvantage, data = training_data)[, -1]
y_tr <- training_data$Margin

set.seed(2026)
cv_ridge <- cv.glmnet(x_tr, y_tr, alpha = 0)         # ridge
ridge     <- glmnet(x_tr, y_tr, alpha = 0, lambda = cv_ridge$lambda.1se)

# ---------------------------------------
# 5) Probability calibration + sharpening
# ---------------------------------------
# Fit logistic calibration: P(Home win | predicted margin)
train_pred_margin <- as.numeric(predict(ridge, newx = x_tr))
train_home_win    <- as.integer(training_data$Margin > 0)

calib_glm <- glm(train_home_win ~ train_pred_margin, family = binomial())

# Temperature scaling to sharpen probabilities (T<1 => bolder)
sigmoid <- function(z) 1 / (1 + exp(-z))
logit   <- function(p) log(p / (1 - p))
sharpen_prob <- function(p, T = TEMP, floor = PROB_FLOOR, ceil = PROB_CEIL) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)   # numerical safety
  p_sharp <- sigmoid(logit(p) / T)
  pmin(pmax(p_sharp, floor), ceil)
}

# ---------------------------------------
# 6) Build fixtures for rd = 0 and predict
# ---------------------------------------
# Latest ratings per team (from last seen matches)
latest_home_ratings <- cleaned_data %>%
  group_by(Home.Team) %>%
  filter(Date == max(Date)) %>%
  select(Home.Team, Home.OffensiveRating, Home.DefensiveRating) %>%
  rename(Team = Home.Team) %>%
  ungroup()

latest_away_ratings <- cleaned_data %>%
  group_by(Away.Team) %>%
  filter(Date == max(Date)) %>%
  select(Away.Team, Away.OffensiveRating, Away.DefensiveRating) %>%
  rename(Team = Away.Team) %>%
  ungroup()

# Latest venue HGA as-of-now (shrunken & centered), for fixtures
hga_latest <- cleaned_data %>%
  group_by(Home.Team, Venue) %>%
  summarise(
    GamesPlayed = sum(!is.na(Margin)),
    GamesWon    = sum(Margin > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    HomeGroundAdvantage = if_else(
      GamesPlayed > 0,
      (GamesWon + HGA_ALPHA * league_home_rate) / (GamesPlayed + HGA_ALPHA),
      league_home_rate
    ) - 0.5,
    HomeGroundAdvantage = pmax(pmin(HomeGroundAdvantage, HGA_CAP), -HGA_CAP)
  )

# Team name normalization (same mapping you used)
normalize_team_names <- function(df, home_col, away_col) {
  df %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "western bulldogs", "Footscray", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "western bulldogs", "Footscray", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "gold coast suns", "Gold Coast", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "gold coast suns", "Gold Coast", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "west coast eagles", "West Coast", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "west coast eagles", "West Coast", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "sydney swans", "Sydney", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "sydney swans", "Sydney", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "adelaide crows", "Adelaide", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "adelaide crows", "Adelaide", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "geelong cats", "Geelong", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "geelong cats", "Geelong", .data[[away_col]])
    ) %>%
    mutate(
      "{home_col}" := ifelse(tolower(.data[[home_col]]) == "gws giants", "GWS", .data[[home_col]]),
      "{away_col}" := ifelse(tolower(.data[[away_col]]) == "gws giants", "GWS", .data[[away_col]])
    )
}

# Fetch fixtures for 2026 round `rd`
fix_data <- fitzRoy::fetch_fixture(2026)%>%
  filter(round.roundNumber == rd) %>%
  select(
    compSeason.name,
    round.roundNumber,
    "home.team.name" = home.team.club.name,
    "away.team.name" = away.team.club.name,
    venue.name
  ) %>%
  normalize_team_names("home.team.name", "away.team.name") %>%
  left_join(latest_home_ratings, by = c("home.team.name" = "Team")) %>%
  left_join(latest_away_ratings, by = c("away.team.name" = "Team"),
            suffix = c(".home", ".away")) %>%
  left_join(hga_latest, by = c("home.team.name" = "Home.Team", "venue.name" = "Venue")) %>%
  mutate(
    HomeGroundAdvantage = coalesce(HomeGroundAdvantage, league_home_rate - 0.5),
    HomeGroundAdvantage = pmax(pmin(HomeGroundAdvantage, HGA_CAP), -HGA_CAP)
  )

# Prepare features and predict
model_test_data <- fix_data %>%
  transmute(
    OffDiff = Home.OffensiveRating - Away.OffensiveRating,
    DefDiff = Home.DefensiveRating - Away.DefensiveRating,
    HomeGroundAdvantage
  )

x_te <- model.matrix(~ OffDiff + DefDiff + HomeGroundAdvantage, data = model_test_data)[, -1]
predicted_margins <- as.numeric(predict(ridge, newx = x_te))

# Calibrate -> Probability, then sharpen (be bolder)
pr_raw   <- predict(calib_glm, newdata = data.frame(train_pred_margin = predicted_margins), type = "response")
pr_bold  <- sharpen_prob(pr_raw, T = TEMP, floor = PROB_FLOOR, ceil = PROB_CEIL)

# ---------------------------
# 7) Build outputs (same files)
# ---------------------------
fix_data_pred <- fix_data %>%
  mutate(
    PredictedMargin = predicted_margins,
    pr = pr_bold,
    Winner = ifelse(pr > 0.5, home.team.name, away.team.name)
  )

if (!dir.exists("test26")) dir.create("test26", recursive = TRUE)

# Full prediction dump
write.csv(fix_data_pred, paste0("test26/predfull_", rd, ".csv"), row.names = FALSE)

# Clean output for tips
pred_clean <- fix_data_pred %>%
  select(
    "RoundNumber"     = round.roundNumber,
    "HomeTeam"        = home.team.name,
    "AwayTeam"        = away.team.name,
    Winner,
    "HomeProbability" = pr,
    "VenueName"       = venue.name,
    "PredictedMargin"
  )
write.csv(pred_clean, paste0("test26/chakri_round_", rd, ".csv"), row.names = FALSE)

# Optional “optibits” nudges (kept to preserve the file; you can tune/disable)
pred_bits_opti <- pred_clean %>%
  mutate(HomeProbability = case_when(
    HomeProbability <= 0.44 & HomeProbability >= 0.23 ~ HomeProbability - 0.06,
    HomeProbability >= 0.56 & HomeProbability <= 0.77 ~ HomeProbability + 0.06,
    TRUE ~ HomeProbability
  ))
write.csv(pred_bits_opti, paste0("test26/chakri_round_optibits", rd, ".csv"), row.names = FALSE)

# Append to season predictions

  seas_preds <- read.csv("test26/chakri_2026_allpreds.csv")
  seas_preds <- rbind(seas_preds, pred_bits_opti)
  write.csv(seas_preds, "test26/chakri_2026_allpreds.csv", row.names = FALSE)

  

message("Done. Files written to: ",
        "\n  - lm26/predfull_", rd, ".csv",
        "\n  - lm26/chakri_round_", rd, ".csv",
        "\n  - lm26/chakri_round_optibits", rd, ".csv",
        "\n  - lm26/chakri_2026_allpreds.csv")
