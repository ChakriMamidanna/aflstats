library(fitzRoy)
library(tidyverse)
library(zoo)
library(gmailr)

rd = 28
# pred_bits_opti
# Load AFL data using fitzRoy
# data <- fitzRoy::fetch_results_afltables(2015, 2022)
# a <- c()
# a1 <- fitzRoy::fetch_player_stats_afl(2024)
# a2 <- a1 %>% 
#   mutate(Season = year(utcStartTime),
#          Date = as.Date(utcStartTime)) %>%
#   group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
#   summarise(tr = sum(ratingPoints))

# afl_player_stats <- c()
# for(year in 2018:2024){
# a <- fitzRoy::fetch_player_stats_afl(year) %>% 
#   filter(!is.na(extendedStats.hitoutsToAdvantage))
# a_clean_home <- a  %>%
#   filter(status == "CONCLUDED") %>%
# mutate(Season = year(utcStartTime),
#        Date = as.Date(utcStartTime)) %>%
# group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
#   summarise(hInside.50s      = sum(inside50s),
# hMarks.Inside.50 = sum(marksInside50),
# htr = sum(ratingPoints),
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
#          "Away.team"=  away.team.name,htr, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50,hinterceptMarks,
#          hOne.Percenters, hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts, hturnovers  ) 
# 
# 
# a_clean_away <- a  %>%
#   filter(status == "CONCLUDED") %>%
#   mutate(Season = year(utcStartTime),
#          Date = as.Date(utcStartTime)) %>%
#   group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,   team.name) %>%
#   summarise(aInside.50s      = sum(inside50s),
#             aMarks.Inside.50 = sum(marksInside50),
#             aTackles         = sum(tackles),
#             aTacklesi50         = sum(tacklesInside50),
#             atr = sum(ratingPoints),
#             ainterceptMarks         = sum(extendedStats.interceptMarks),
#             aOne.Percenters  = sum(onePercenters),
#             acontestedPossessions = sum(contestedPossessions),
#             ashotsAtGoal = sum(shotsAtGoal),
#             aclearances = sum(clearances.totalClearances),
#             ahitouts = sum(extendedStats.hitoutsToAdvantage),
#             aturnovers = sum(turnovers)  ) %>%
#   filter(away.team.name == team.name) %>%
#   select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
#          "Away.team"=  away.team.name, atr, aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50,ainterceptMarks,
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
# results_orig <- fitzRoy::fetch_results_afltables(2018:2024) %>%
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
# # colSums(is.na(result_orig_withstats))
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
results_25 <- fitzRoy::fetch_results(2025) %>%
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

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

cleaned_data <- result_orig_withstats25 %>% 
  filter(!is.na(Home.Points), !is.na(Away.Points)) %>% # Remove incomplete matches
  mutate(
    Margin = Home.Points - Away.Points, # Calculate margin
    Date = as.Date(Date, format = "%Y-%m-%d")
  )  %>%
  mutate(
    Home.ShotsAtGoal = replace_na(hshotsAtGoal, 0),
    Home.MarksInside50 = replace_na(hMarks.Inside.50, 0),
    Home.Tacklesi50 = replace_na(hTacklesi50, 0),
    Home.Clearances = replace_na(hclearances, 0),
    Away.ShotsAtGoal = replace_na(ashotsAtGoal, 0),
    Away.MarksInside50 = replace_na(aMarks.Inside.50, 0),
    Away.Tacklesi50 = replace_na(aTacklesi50, 0),
    Away.Clearances = replace_na(aclearances, 0),
    homeopp_tr = replace_na(atr, 0),
    awayopp_tr = replace_na(htr, 0)
  ) %>%
  mutate( 
    Home.Tackles = replace_na(hTackles, 0),
    Home.ContestedPossessions = replace_na(hcontestedPossessions, 0),
    Home.InterceptMarks = replace_na(hinterceptMarks, 0),
    Away.Tackles = replace_na(aTackles, 0),
    Away.ContestedPossessions = replace_na(acontestedPossessions, 0),
    Away.InterceptMarks = replace_na(ainterceptMarks, 0)
  ) #%>%
  # filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31")) 

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("htr", "Home.ShotsAtGoal","Home.ContestedPossessions", "Home.MarksInside50", "Home.Tacklesi50", "Home.Clearances")) #%>%
# mutate(Home.OffensiveRating = Home.ShotsAtGoal_Avg*2 + Home.MarksInside50_Avg*1 + Home.Clearances_Avg*0.2)

cleaned_data <- cleaned_data %>%
  calculate_rolling_avg(., "Away.Team", c("atr","Away.ShotsAtGoal", "Away.MarksInside50","Away.Tacklesi50", "Away.Clearances")) #%>%
# mutate(Away.OffensiveRating =  Away.ShotsAtGoal_Avg*2 + Away.MarksInside50_Avg*1 + Away.Clearances_Avg*0.2)

cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("homeopp_tr","Home.Tackles", "Home.ContestedPossessions", "Home.InterceptMarks")) #%>%
# mutate(Home.DefensiveRating =  Home.Tackles_Avg*0.8 + Home.ContestedPossessions_Avg*0.15 + Home.InterceptMarks_Avg*3 + Away.ShotsAtGoal_Avg*-1)
cleaned_data <- cleaned_data %>%
  calculate_rolling_avg(., "Away.Team", c("awayopp_tr","Away.Tackles", "Away.ContestedPossessions", "Away.InterceptMarks")) #%>%
# mutate(Away.DefensiveRating =   Away.Tackles_Avg*0.8 + Away.ContestedPossessions_Avg*0.15 + Away.InterceptMarks_Avg*3)

horlm_model <- lm(Margin ~  Home.ShotsAtGoal_Avg +  Home.Clearances_Avg, data = cleaned_data)
home_off_rats <- predict(horlm_model, newdata = cleaned_data)
horlm_model <- lm(Margin ~ Home.ContestedPossessions_Avg  + Away.ShotsAtGoal_Avg, data = cleaned_data)
home_def_rats <- predict(horlm_model, newdata = cleaned_data)

horlm_model <- lm(-Margin ~   Away.ShotsAtGoal_Avg + Away.MarksInside50_Avg + Away.Clearances_Avg, data = cleaned_data)
away_off_rats <- predict(horlm_model, newdata = cleaned_data)
horlm_model <- lm(-Margin ~   Away.Tackles_Avg + Away.ContestedPossessions_Avg + Away.InterceptMarks_Avg , data = cleaned_data)
away_def_rats <- predict(horlm_model, newdata = cleaned_data)
# summary(horlm_model)

cleaned_data <- cleaned_data %>% 
  mutate(Home.DefensiveRating = home_def_rats, 
         Home.OffensiveRating = home_off_rats, 
         Away.DefensiveRating = away_def_rats, 
         Away.OffensiveRating = away_off_rats)


# cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("htr", "Home.ShotsAtGoal", "Home.MarksInside50", "Home.Clearances")) %>%
#   mutate(Home.OffensiveRating =htr_Avg*0.5 + Home.ShotsAtGoal_Avg*2 + Home.MarksInside50_Avg*1 + Home.Clearances_Avg*0.2)
# cleaned_data <- cleaned_data %>% 
#   calculate_rolling_avg(., "Away.Team", c("atr","Away.ShotsAtGoal", "Away.MarksInside50", "Away.Clearances")) %>%
#   mutate(Away.OffensiveRating = atr_Avg*0.5 + Away.ShotsAtGoal_Avg*-2 + Away.MarksInside50_Avg*-1 + Away.Clearances_Avg*-0.2)
# 
# cleaned_data <- calculate_rolling_avg(cleaned_data, "Home.Team", c("homeopp_tr","Home.Tackles", "Home.ContestedPossessions", "Home.InterceptMarks")) %>%
#   mutate(Home.DefensiveRating =homeopp_tr_Avg*-0.25 +Away.ShotsAtGoal_Avg*-2 + Home.Tackles_Avg*0.8 + Home.ContestedPossessions_Avg*0.15 + Home.InterceptMarks_Avg*3)
# cleaned_data <- cleaned_data %>% 
#   calculate_rolling_avg(., "Away.Team", c("awayopp_tr","Away.Tackles", "Away.ContestedPossessions", "Away.InterceptMarks")) %>%
#   mutate(Away.DefensiveRating =awayopp_tr_Avg*0.25 + Home.ShotsAtGoal_Avg*2 + Away.Tackles_Avg*-0.8 + Away.ContestedPossessions_Avg*-0.15 + Away.InterceptMarks_Avg*-3)



# a21 <- cleaned_data %>% filter(is.na(awayopp_tr_Avg))
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

# cleaned_data %>%
# ggplot(aes(Margin, Home.OffensiveRating)) + geom_point() + geom_smooth()

cleaned_data <- cleaned_data %>%
  # filter(Round.Type != "Finals") %>%
  left_join(home_ground_advantage, by = c("Home.Team", "Venue")) %>%
  mutate(HomeGroundAdvantage = replace_na(HomeGroundAdvantage, 0)) %>% 
  select("Game", "Date", "Round", "Home.Team", "Home.Goals", "Home.Behinds", 
         "Home.Points", "Away.Team", "Away.Goals", "Away.Behinds", "Away.Points", 
         "Margin", "Season", "Round.Type", "Round.Number", "seas_rnd", 
         "Venue",htr, htr_Avg,atr, atr_Avg, homeopp_tr, homeopp_tr_Avg,awayopp_tr, awayopp_tr_Avg, "hInside.50s", "hMarks.Inside.50", "hTackles", "hTacklesi50", 
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
  filter(Round.Type != "Finals") #%>%
  # filter(Date >= as.Date("2024-01-01") & Date <= as.Date("2024-12-31")) # %>%
  # filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2024-12-31"))




# Prepare training data for the linear model
model_training_data <- training_data %>%
  filter(!is.na(Home.OffensiveRating), !is.na(Away.DefensiveRating)) %>%
  mutate(
    htr_coeff = htr_Avg/homeopp_tr_Avg, 
    atr_coeff = atr_Avg/awayopp_tr_Avg, 
    # OffensiveDifference = (htr_coeff*Home.OffensiveRating) - (atr_coeff*Away.OffensiveRating),
    # DefensiveDifference = (htr_coeff*Home.DefensiveRating) - (atr_coeff*Away.DefensiveRating),
    OffensiveDifference = (Home.OffensiveRating) - (Away.OffensiveRating),
    DefensiveDifference = (Home.DefensiveRating) - (Away.DefensiveRating)
  ) %>%
  select("Game", "Date", "Round", "Home.Team", "Home.Goals", "Home.Behinds", 
         "Home.Points", "Away.Team", "Away.Goals", "Away.Behinds", "Away.Points", 
         "Margin", "Season", "Round.Type", "Round.Number", "seas_rnd", 
         "Venue",htr_Avg, atr_Avg, homeopp_tr_Avg, htr_coeff, atr_coeff,Home.OffensiveRating, Away.OffensiveRating,Home.DefensiveRating,Away.DefensiveRating, OffensiveDifference, DefensiveDifference, HomeGroundAdvantage)

# Train the model
lm_model <- lm(Margin ~ Home.OffensiveRating + Away.OffensiveRating + Home.DefensiveRating + Away.DefensiveRating + HomeGroundAdvantage, data = model_training_data)
summary(lm_model)
# 
# # Display model summary
# # hn2 <- summary(lm_model)
# # hn
#
#
# Prepare test data for predictions


# dput(names(test_data))
map_margin_to_outcome <- function(score, marg_elo = 50) {
  # Ensure the score is within the bounds of [-80, 80]
  score <- pmin(pmax(score, -50), 50)
  
  # Inverse the score transformation
  elo_perc <- (asin(score / marg_elo) / pi) + 0.5
  
  return(elo_perc)
}

marg_elo = 60
map_elo_to_score <- function(elo_perc, marg.max = marg_elo, marg.min = -marg_elo){
  # score <- (elo_perc*(marg.max - marg.min)) + marg.min
  
  score <- (sin((elo_perc - 0.5) * pi)) * marg_elo #(elo_perc - 0.5)^2 * 320 * sign(elo_perc - 0.5)
  score %>% pmin(60) %>% pmax(-60)
}

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

fix_data <- fitzRoy::fetch_fixture(2025) %>% 
  filter(round.roundNumber == rd) %>% 
  select(compSeason.name, round.roundNumber, "home.team.name" =home.team.club.name, "away.team.name" =away.team.club.name, venue.name)%>%
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
  select( OffensiveDifference, DefensiveDifference,Home.OffensiveRating, Away.OffensiveRating,Home.DefensiveRating,Away.DefensiveRating, HomeGroundAdvantage)

# Make predictions
predicted_margins <- predict(lm_model, newdata = model_test_data)

fix_data_pred <- fix_data %>%
  mutate(PredictedMargin = predicted_margins, 
         pr = map_margin_to_outcome(PredictedMargin)) %>% 
  mutate(Winner = ifelse(pr > 0.5, home.team.name, away.team.name)) 

write.csv(fix_data_pred, paste0("predictions2025full/predfull_",rd,".csv"), row.names = F)
# dput(names(fix_data_pred))

pred_clean <- fix_data_pred %>% 
  select("RoundNumber"=round.roundNumber, "HomeTeam" =home.team.name, "AwayTeam"=away.team.name, 
         Winner,"HomeProbability"=pr, "VenueName"=venue.name,  "PredictedMargin")
write.csv(pred_clean, paste0("predictions2025/chakri_round_",rd,".csv"), row.names = F)


pred_bits_opti <- fix_data_pred %>% 
  select("RoundNumber"=round.roundNumber, "HomeTeam" =home.team.name, "AwayTeam"=away.team.name, 
         Winner,"HomeProbability"=pr, "VenueName"=venue.name,  "PredictedMargin") %>% 
  mutate(HomeProbability = case_when(HomeProbability <= 0.44 & HomeProbability >= 0.23  ~ HomeProbability - 0.06,
                                     HomeProbability >= 0.56 & HomeProbability <= 0.77  ~ HomeProbability + 0.06,
                                     TRUE ~  HomeProbability))
write.csv(pred_bits_opti, paste0("predictions2025/chakri_round_optibits",rd,".csv"), row.names = F)

seas_preds <- read.csv(paste0("predictions2025/chakri_2025_allpredsorig.csv"))
seas_preds <- rbind(seas_preds, pred_bits_opti)
write.csv(seas_preds, "predictions2025/chakri_2025_allpredsorig.csv", row.names = F)

# 
# gm_auth_configure(path = "gmailsec.json")
# 
# my_email <- gm_mime() %>% 
#   gm_to("chakri.mamidanna@reece.com.au") %>% 
#   gm_from("chakrimamidanna@gmail.com") %>% 
#   gm_subject(paste0("Round ", rd)) %>% 
#   gm_text_body(paste0("Chakri's round ", rd, " AFL tips")) %>% 
#   gm_attach_file(paste0("predictions2025/chakri_round_",rd,".csv"))
# 
# gm_send_message(my_email)
