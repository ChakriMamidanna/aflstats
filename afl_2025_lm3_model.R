library(fitzRoy)
library(tidyverse)
library(zoo)

# Load AFL data using fitzRoy
# data <- fitzRoy::fetch_results_afltables(2015, 2022)
# a <- c()
# afl_player_stats <- c()
# for(year in 2016:2024){
# a <- fitzRoy::fetch_player_stats_afl(year)
# a_clean_home <- a %>%
#   filter(status == "CONCLUDED") %>%
#   mutate(Season = year(utcStartTime),
#          Date = as.Date(utcStartTime)) %>%
#   group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
#   summarise(hInside.50s      = sum(inside50s),
#             hMarks.Inside.50 = sum(marksInside50),
#             hTackles         = sum(tackles),
#             hTacklesi50         = sum(tacklesInside50),
#             hinterceptMarks         = sum(extendedStats.interceptMarks),
#             hOne.Percenters  = sum(onePercenters),
#             hcontestedPossessions = sum(contestedPossessions),
# hshotsAtGoal = sum(shotsAtGoal),
#             hclearances = sum(clearances.totalClearances),
#             hhitouts = sum(extendedStats.hitoutsToAdvantage),
#             hturnovers = sum(turnovers) ) %>%
#   filter(home.team.name == team.name) %>%
#   select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
#          "Away.team"=  away.team.name, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50,hinterceptMarks,
#          hOne.Percenters, hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts, hturnovers  )
# 
# a_clean_away <- a %>%
#   filter(status == "CONCLUDED") %>%
#   mutate(Season = year(utcStartTime),
#          Date = as.Date(utcStartTime)) %>%
#   group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
#   summarise(aInside.50s      = sum(inside50s),
#             aMarks.Inside.50 = sum(marksInside50),
#             aTackles         = sum(tackles),
#             aTacklesi50         = sum(tacklesInside50),
#             ainterceptMarks         = sum(extendedStats.interceptMarks),
#             aOne.Percenters  = sum(onePercenters),
#             acontestedPossessions = sum(contestedPossessions),
#             ashotsAtGoal = sum(shotsAtGoal),
#             aclearances = sum(clearances.totalClearances),
#             ahitouts = sum(extendedStats.hitoutsToAdvantage),
#             aturnovers = sum(turnovers)  ) %>%
#   filter(away.team.name == team.name) %>%
#   select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
#          "Away.team"=  away.team.name,aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50,ainterceptMarks,
#          aOne.Percenters, acontestedPossessions, ashotsAtGoal, aclearances, ahitouts, aturnovers)
# 
# a_clean <- left_join(a_clean_home, a_clean_away, by = c("Season", "Round", "Date", "Venue", "Home.team", "Away.team"))
# 
#   afl_player_stats <- rbind(a_clean,afl_player_stats)
# }
# 
# stats_sum <- afl_player_stats %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "western bulldogs", "Footscray", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "western bulldogs", "Footscray", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "gold coast suns", "Gold Coast", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "gold coast suns", "Gold Coast", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "west coast eagles", "West Coast", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "west coast eagles", "West Coast", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "sydney swans", "Sydney", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "sydney swans", "Sydney", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "adelaide crows", "Adelaide", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "adelaide crows", "Adelaide", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "geelong cats", "Geelong", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "geelong cats", "Geelong", Away.team)) %>%
#   mutate(Home.team = ifelse(tolower(Home.team) == "gws giants", "GWS", Home.team),
#          Away.team = ifelse(tolower(Away.team) == "gws giants", "GWS", Away.team)) %>%
#   ungroup() %>%
#   select(-Round)
# 
# 
# results_orig <- fitzRoy::fetch_results_afltables(2016:2024)%>%
#   mutate(Round.Number = ifelse(Round.Number < 10, paste0("0",Round.Number), Round.Number)) %>%
#   mutate(seas_rnd =as.numeric(paste0(Season, Round.Number)))
# 
# result_orig_withstats <- results_orig %>%
#   select(-Venue) %>%
#   left_join(stats_sum, by = c("Date",
#                               "Home.Team"= "Home.team",
#                               "Season",
#                               "Away.Team"="Away.team"))  %>%
#   # filter(Round.Type != "Finals")
#   filter(Game != 14786)
# 
# # result_orig_withstats <- read.csv("result_withstats_15to23.csv")
# # write.csv(result_orig_withstats, "result_withstats_15to23.csv", row.names = F)
# saveRDS(result_orig_withstats, "result_withstats_16to24.rds")
result_orig_withstats <- readRDS("result_withstats_16to24.rds")

a25 <- fitzRoy::fetch_player_stats_afl(2025)
a25_clean_home <- a25 %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
  summarise(hInside.50s      = sum(inside50s),
            hMarks.Inside.50 = sum(marksInside50),
            hTackles         = sum(tackles),
            hTacklesi50         = sum(tacklesInside50),
            hinterceptMarks         = sum(extendedStats.interceptMarks),
            hOne.Percenters  = sum(onePercenters),
            hcontestedPossessions = sum(contestedPossessions),
            hshotsAtGoal = sum(shotsAtGoal),
            hclearances = sum(clearances.totalClearances),
            hhitouts = sum(extendedStats.hitoutsToAdvantage),
            hturnovers = sum(turnovers) ) %>%
  filter(home.team.name == team.name) %>%
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
         "Away.team"=  away.team.name, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50,hinterceptMarks,
         hOne.Percenters, hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts, hturnovers  )

a25_clean_away <- a25 %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
  summarise(aInside.50s      = sum(inside50s),
            aMarks.Inside.50 = sum(marksInside50),
            aTackles         = sum(tackles),
            aTacklesi50         = sum(tacklesInside50),
            ainterceptMarks         = sum(extendedStats.interceptMarks),
            aOne.Percenters  = sum(onePercenters),
            acontestedPossessions = sum(contestedPossessions),
            ashotsAtGoal = sum(shotsAtGoal),
            aclearances = sum(clearances.totalClearances),
            ahitouts = sum(extendedStats.hitoutsToAdvantage),
            aturnovers = sum(turnovers)  ) %>%
  filter(away.team.name == team.name) %>%
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
         "Away.team"=  away.team.name,aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50,ainterceptMarks,
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


results_25 <- fitzRoy::fetch_results_afltables(2025)%>%
  mutate(Round.Number = ifelse(Round.Number < 10, paste0("0",Round.Number), Round.Number)) %>%
  mutate(seas_rnd =as.numeric(paste0(Season, Round.Number)))

result_25_withstats <- results_25 %>%
  select(-Venue) %>%
  left_join(stats25_sum, by = c("Date",
                                "Home.Team"= "Home.team",
                                "Season",
                                "Away.Team"="Away.team"))  %>%
  # filter(Round.Type != "Finals")
  filter(Game != 14786)


result_orig_withstats25 <- rbind(result_orig_withstats, result_25_withstats) 

# Preprocess data

# Preprocess data

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


cleaned_data <- result_orig_withstats %>% 
  filter(!is.na(Home.Points), !is.na(Away.Points)) %>% # Remove incomplete matches
  mutate(
    Margin = Home.Points - Away.Points, # Calculate margin
    Date = as.Date(Date, format = "%Y-%m-%d")
  )  %>%
  mutate(
    Home.ShotsAtGoal = replace_na(hshotsAtGoal, 0),
    Home.MarksInside50 = replace_na(hMarks.Inside.50, 0),
    Home.Clearances = replace_na(hclearances, 0),
    Away.ShotsAtGoal = replace_na(ashotsAtGoal, 0),
    Away.MarksInside50 = replace_na(aMarks.Inside.50, 0),
    Away.Clearances = replace_na(aclearances, 0)
  ) %>%
  mutate(
    Home.Tackles = replace_na(hTackles, 0),
    Home.ContestedPossessions = replace_na(hcontestedPossessions, 0),
    Home.InterceptMarks = replace_na(hinterceptMarks, 0),
    Away.Tackles = replace_na(aTackles, 0),
    Away.ContestedPossessions = replace_na(acontestedPossessions, 0),
    Away.InterceptMarks = replace_na(ainterceptMarks, 0)
  ) %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))





# Compute rolling averages for offensive and defensive stats using only past games
# cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("Home.ShotsAtGoal", "Home.MarksInside50", "Home.Clearances"))
# cleaned_data <- calculate_rolling_avg(cleaned_data, "Away.Team", c("Away.ShotsAtGoal", "Away.MarksInside50", "Away.Clearances"))
# cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("Home.Tackles", "Home.ContestedPossessions", "Home.InterceptMarks"))
# cleaned_data <- calculate_rolling_avg(cleaned_data, "Away.Team", c("Away.Tackles", "Away.ContestedPossessions", "Away.InterceptMarks"))

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("Home.ShotsAtGoal", "Home.MarksInside50", "Home.Clearances")) %>%
  mutate(Home.OffensiveRating = Home.ShotsAtGoal_Avg*2 + Home.MarksInside50_Avg*1 + Home.Clearances_Avg*0.2)
cleaned_data <- calculate_rolling_avg(cleaned_data, "Away.Team", c("Away.ShotsAtGoal", "Away.MarksInside50", "Away.Clearances")) %>%
  mutate(Away.OffensiveRating = Away.ShotsAtGoal_Avg*2 + Away.MarksInside50_Avg*1 + Away.Clearances_Avg*0.2)

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("Home.Tackles", "Home.ContestedPossessions", "Home.InterceptMarks")) %>%
  mutate(Home.DefensiveRating = Home.Tackles_Avg*0.8 + Home.ContestedPossessions_Avg*0.15 + Home.InterceptMarks_Avg*3)
cleaned_data <- calculate_rolling_avg(cleaned_data, "Away.Team", c("Away.Tackles", "Away.ContestedPossessions", "Away.InterceptMarks")) %>%
  mutate(Away.DefensiveRating = Away.Tackles_Avg*0.8 + Away.ContestedPossessions_Avg*0.15 + Away.InterceptMarks_Avg*3)


# Part 3: Home Ground Advantage (Last 12 Months)
home_ground_advantage <- cleaned_data %>%
  filter(Date >= max(Date) - 365) %>%
  group_by(Home.Team, Venue) %>%
  summarise(
    GamesPlayed = n(),
    GamesWon = sum(if_else(Margin > 0, 1, 0)),
    .groups = "drop"
  ) %>%
  mutate(HomeGroundAdvantage = GamesWon / GamesPlayed)

cleaned_data <- cleaned_data %>%
  # filter(Round.Type != "Finals") %>%
  left_join(home_ground_advantage, by = c("Home.Team", "Venue")) %>%
  mutate(HomeGroundAdvantage = replace_na(HomeGroundAdvantage, 0)) %>% 
  select("Game", "Date", "Round", "Home.Team", "Home.Goals", "Home.Behinds", 
         "Home.Points", "Away.Team", "Away.Goals", "Away.Behinds", "Away.Points", 
         "Margin", "Season", "Round.Type", "Round.Number", "seas_rnd", 
         "Venue", "hInside.50s", "hMarks.Inside.50", "hTackles", "hTacklesi50", 
         "hinterceptMarks", "hOne.Percenters", "hcontestedPossessions", 
         "hshotsAtGoal", "Home.ShotsAtGoal_Avg", "Home.MarksInside50_Avg", 
         "Home.Clearances_Avg", "Away.ShotsAtGoal_Avg", "Away.MarksInside50_Avg", 
         "Away.Clearances_Avg", "Home.Tackles_Avg", "Home.ContestedPossessions_Avg", 
         "Home.InterceptMarks_Avg", "Away.Tackles_Avg", "Away.ContestedPossessions_Avg", 
         "Away.InterceptMarks_Avg", "Home.OffensiveRating", "Away.OffensiveRating", 
         "Home.DefensiveRating", "Away.DefensiveRating", "hclearances", "hhitouts", "hturnovers", "aInside.50s", 
         "aMarks.Inside.50", "aTackles", "aTacklesi50", "ainterceptMarks", 
         "aOne.Percenters", "acontestedPossessions", "ashotsAtGoal", "aclearances", 
         "ahitouts", "aturnovers", "Home.ShotsAtGoal", "Home.MarksInside50", 
         "Home.Clearances", "Away.ShotsAtGoal", "Away.MarksInside50", 
         "Away.Clearances", "Home.Tackles", "Home.ContestedPossessions", 
         "Home.InterceptMarks", "Away.Tackles", "Away.ContestedPossessions", 
         "Away.InterceptMarks", "GamesPlayed", 
         "GamesWon", "HomeGroundAdvantage")

# 
# Filter training data (2016-2022)
training_data <- cleaned_data %>%
  filter(Round.Type != "Finals") %>%
  # filter(Date >= as.Date("2024-01-01") & Date <= as.Date("2024-12-31")) # %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2023-12-31"))

# # Filter test data (2023-2024)
test_data <- cleaned_data %>%
  filter(Date >= as.Date("2024-01-01") & Date <= as.Date("2024-12-31"))



# Prepare training data for the linear model
model_training_data <- training_data %>%
  filter(!is.na(Home.OffensiveRating), !is.na(Away.DefensiveRating)) %>%
  mutate(
    OffensiveDifference = Home.OffensiveRating - Away.OffensiveRating,
    DefensiveDifference = Home.DefensiveRating - Away.DefensiveRating
  ) %>%
  select(Margin, OffensiveDifference, DefensiveDifference, HomeGroundAdvantage)

# Train the model
lm_model <- lm(Margin ~ OffensiveDifference + DefensiveDifference + HomeGroundAdvantage, data = model_training_data)

# # Display model summary
# # hn2 <- summary(lm_model)
# # hn
#
#
# Prepare test data for predictions
model_test_data <- test_data %>%
  mutate(
    OffensiveDifference = Home.OffensiveRating - Away.OffensiveRating,
    DefensiveDifference = Home.DefensiveRating - Away.DefensiveRating
  ) %>%
  select(OffensiveDifference, DefensiveDifference, HomeGroundAdvantage)

# Make predictions
predicted_margins <- predict(lm_model, newdata = model_test_data)

# Add predictions to the test dataset
test_data <- test_data %>%
  mutate(PredictedMargin = predicted_margins)
# dput(names(test_data))

hasd <- test_data %>%
mutate(correct_pick = case_when(Margin < 0 & PredictedMargin < 0 ~ 1,
                                Margin > 0 & PredictedMargin > 0 ~ 1,
                                Margin == 0 & PredictedMargin == 0 ~ 1,
                                TRUE ~ 0)) %>%
  select(
    "Game", "Date", "Round", "Home.Team", "Away.Team", "Margin","PredictedMargin",correct_pick,
    "Home.Points", "Away.Points", "Home.Goals", "Home.Behinds", "Away.Goals", "Away.Behinds",
    "Season", "Round.Type", "Round.Number", "seas_rnd",
    "Venue","Home.ShotsAtGoal_Avg", "Home.MarksInside50_Avg",
    "Home.Clearances_Avg", "Home.OffensiveRating", "Away.ShotsAtGoal_Avg",
    "Away.MarksInside50_Avg", "Away.Clearances_Avg", "Away.OffensiveRating",
    "Home.Tackles", "Home.ContestedPossessions", "Home.InterceptMarks",
    "Away.Tackles", "Away.ContestedPossessions", "Away.InterceptMarks",
    "Home.Tackles_Avg", "Home.ContestedPossessions_Avg", "Home.InterceptMarks_Avg",
    "Home.DefensiveRating", "Away.Tackles_Avg", "Away.ContestedPossessions_Avg",
    "Away.InterceptMarks_Avg", "Away.DefensiveRating", "GamesPlayed",
    "GamesWon", "HomeGroundAdvantage"
  )

hasd %>%
  mutate(mae = abs(Margin-PredictedMargin)) %>%
  filter(Round.Type != "Finals") %>%
  group_by(Season) %>%
  summarise(correct_pick = sum(correct_pick),
            allgames = n(),
            mae = sum(mae)/allgames)


# Get the latest date for each team
latest_home_ratings <- cleaned_data %>%
  group_by(Home.Team) %>%
  filter(Date == max(Date)) %>%
  select(Home.Team, Home.OffensiveRating, Home.DefensiveRating) %>%
  rename(Team = Home.Team) %>%
  ungroup()

# Also include Away team ratings (for completeness)
latest_away_ratings <- cleaned_data %>%
  group_by(Away.Team) %>%
  filter(Date == max(Date)) %>%
  select(Away.Team, Away.OffensiveRating, Away.DefensiveRating) %>%
  rename(Team = Away.Team) %>%
  ungroup()

# Combine home and away ratings (ensuring each team appears only once)
current_team_ratings <- left_join(latest_home_ratings, latest_away_ratings, by = c("Team")) %>% 
  mutate(all_home =Home.OffensiveRating + Home.DefensiveRating, 
         all_away = Away.OffensiveRating + Away.DefensiveRating)

rd = 0
fix_data <- fitzRoy::fetch_fixture(2025) %>% 
  filter(round.roundNumber == rd) %>% 
  select(compSeason.name, round.roundNumber, home.team.name, away.team.name, venue.name)%>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "western bulldogs", "Footscray", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "western bulldogs", "Footscray", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "gold coast suns", "Gold Coast", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "gold coast suns", "Gold Coast", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "west coast eagles", "West Coast", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "west coast eagles", "West Coast", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "sydney swans", "Sydney", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "sydney swans", "Sydney", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "adelaide crows", "Adelaide", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "adelaide crows", "Adelaide", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "geelong cats", "Geelong", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "geelong cats", "Geelong", away.team.name)) %>%
  mutate(home.team.name = ifelse(tolower(home.team.name) == "gws giants", "GWS", home.team.name),
         away.team.name = ifelse(tolower(away.team.name) == "gws giants", "GWS", away.team.name)) %>%
  left_join(latest_home_ratings, by = c("home.team.name"="Team")) %>%
  left_join(latest_away_ratings, by = c("away.team.name"="Team")) %>%
  left_join(home_ground_advantage, by = c("home.team.name"="Home.Team","venue.name"= "Venue")) %>%
  mutate(HomeGroundAdvantage = replace_na(HomeGroundAdvantage, 0))

model_test_data <- fix_data %>%
  mutate(
    OffensiveDifference = Home.OffensiveRating - Away.OffensiveRating,
    DefensiveDifference = Home.DefensiveRating - Away.DefensiveRating
  ) %>%
  select(OffensiveDifference, DefensiveDifference, HomeGroundAdvantage)

# Make predictions
predicted_margins <- predict(lm_model, newdata = model_test_data)

fix_data <- fix_data %>%
  mutate(PredictedMargin = predicted_margins)

