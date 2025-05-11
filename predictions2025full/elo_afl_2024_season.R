library(fitzRoy)
library(tidyverse)
library(elo)
library(lubridate)
library(scales)
options(scipen = 99)
# install.packages("gmailr")
library(gmailr)
# 
# a <- c()
# afl_player_stats <- c()
# 
# for(year in 2015:2023){
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
#             hOne.Percenters  = sum(onePercenters), 
#             hcontestedPossessions = sum(contestedPossessions), 
#             hshotsAtGoal = sum(shotsAtGoal), 
#             hclearances = sum(clearances.totalClearances), 
#             hhitouts = sum(extendedStats.hitoutsToAdvantage), 
#             hturnovers = sum(turnovers) ) %>%
#   filter(home.team.name == team.name) %>%
#   select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
#          "Away.team"=  away.team.name, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50, hOne.Percenters, 
#          hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts, hturnovers  )
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
#             aOne.Percenters  = sum(onePercenters),
#             acontestedPossessions = sum(contestedPossessions), 
#             ashotsAtGoal = sum(shotsAtGoal), 
#             aclearances = sum(clearances.totalClearances), 
#             ahitouts = sum(extendedStats.hitoutsToAdvantage), 
#             aturnovers = sum(turnovers)  ) %>%
#   filter(away.team.name == team.name) %>%
#   select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.name,
#          "Away.team"=  away.team.name,aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50, aOne.Percenters, 
#          acontestedPossessions, ashotsAtGoal, aclearances, ahitouts, aturnovers)
# 
# a_clean <- left_join(a_clean_home, a_clean_away, by = c("Season", "Round", "Date", "Venue", "Home.team", "Away.team"))
# 
#   afl_player_stats <- rbind(a_clean,afl_player_stats)
# }
# 
# stats_sum <- afl_player_stats %>%
#   mutate(Home.team = ifelse(Home.team == "Western Bulldogs", "Footscray", Home.team),
#          Away.team = ifelse(Away.team == "Western Bulldogs", "Footscray", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "Gold Coast Suns", "Gold Coast", Home.team),
#          Away.team = ifelse(Away.team == "Gold Coast Suns", "Gold Coast", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "West Coast Eagles", "West Coast", Home.team),
#          Away.team = ifelse(Away.team == "West Coast Eagles", "West Coast", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "Sydney Swans", "Sydney", Home.team),
#          Away.team = ifelse(Away.team == "Sydney Swans", "Sydney", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "Adelaide Crows", "Adelaide", Home.team),
#          Away.team = ifelse(Away.team == "Adelaide Crows", "Adelaide", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "Geelong Cats", "Geelong", Home.team),
#          Away.team = ifelse(Away.team == "Geelong Cats", "Geelong", Away.team)) %>%
#   mutate(Home.team = ifelse(Home.team == "GWS Giants", "GWS", Home.team),
#          Away.team = ifelse(Away.team == "GWS Giants", "GWS", Away.team)) %>%
#   ungroup() %>%
#   select(-Round)
# 
# 
# results_orig <- fitzRoy::fetch_results_afltables(2015:2023)%>%
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

# result_orig_withstats <- read.csv("result_withstats_15to23.csv")
# write.csv(result_orig_withstats, "result_withstats_15to23.csv", row.names = F)
# saveRDS(result_orig_withstats, "result_withstats_15to23.rds")
result_orig_withstats <- readRDS("result_withstats_15to23.rds")

# dput(names(result_orig_withstats24))
add_2024_stats <- fitzRoy::fetch_player_stats_afl(2024)
add_2024_stats_clean_home <- add_2024_stats %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name,home.team.club.name,  away.team.club.name, team.name) %>%
  summarise(hInside.50s      = sum(inside50s),
            hMarks.Inside.50 = sum(marksInside50),
            hTackles         = sum(tackles),
            hTacklesi50         = sum(tacklesInside50),
            hOne.Percenters  = sum(onePercenters), 
            hcontestedPossessions = sum(contestedPossessions), 
            hshotsAtGoal = sum(shotsAtGoal), 
            hclearances = sum(clearances.totalClearances), 
            hhitouts = sum(extendedStats.hitoutsToAdvantage), 
            hturnovers = sum(turnovers)) %>%
  filter(home.team.name == team.name) %>%
  ungroup() %>% 
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.club.name,
         "Away.team"=  away.team.club.name, hInside.50s, hMarks.Inside.50, hTackles,hTacklesi50, hOne.Percenters, 
         hcontestedPossessions, hshotsAtGoal, hclearances, hhitouts , hturnovers )

add_2024_stats_clean_away <- add_2024_stats %>%
  filter(status == "CONCLUDED") %>%
  mutate(Season = year(utcStartTime),
         Date = as.Date(utcStartTime)) %>%
  group_by(Season,  round.roundNumber, Date,  venue.name,  home.team.name, away.team.name, home.team.club.name,  away.team.club.name,  team.name) %>%
  summarise(aInside.50s      = sum(inside50s),
            aMarks.Inside.50 = sum(marksInside50),
            aTackles         = sum(tackles),
            aTacklesi50         = sum(tacklesInside50),
            aOne.Percenters  = sum(onePercenters),
            acontestedPossessions = sum(contestedPossessions), 
            ashotsAtGoal = sum(shotsAtGoal), 
            aclearances = sum(clearances.totalClearances), 
            ahitouts = sum(extendedStats.hitoutsToAdvantage) ,
            aturnovers = sum(turnovers)) %>%
  filter(away.team.name == team.name) %>%
  ungroup() %>% 
  select(Season,"Round"= round.roundNumber, Date,"Venue"= venue.name,"Home.team"= home.team.club.name,
         "Away.team"=  away.team.club.name,aInside.50s, aMarks.Inside.50, aTackles, aTacklesi50, aOne.Percenters, 
         acontestedPossessions, ashotsAtGoal, aclearances, ahitouts, aturnovers)

add_2024_stats_clean <- left_join(add_2024_stats_clean_home, add_2024_stats_clean_away, 
                     by = c("Season", "Round", "Date", "Venue", "Home.team", "Away.team"))%>%
  mutate(Home.team = ifelse(Home.team == "Western Bulldogs", "Footscray", Home.team),
         Away.team = ifelse(Away.team == "Western Bulldogs", "Footscray", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Gold Coast Suns", "Gold Coast", Home.team),
         Away.team = ifelse(Away.team == "Gold Coast Suns", "Gold Coast", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Gold Coast SUNS", "Gold Coast", Home.team),
         Away.team = ifelse(Away.team == "Gold Coast SUNS", "Gold Coast", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "West Coast Eagles", "West Coast", Home.team),
         Away.team = ifelse(Away.team == "West Coast Eagles", "West Coast", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Sydney Swans", "Sydney", Home.team),
         Away.team = ifelse(Away.team == "Sydney Swans", "Sydney", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Adelaide Crows", "Adelaide", Home.team),
         Away.team = ifelse(Away.team == "Adelaide Crows", "Adelaide", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Geelong Cats", "Geelong", Home.team),
         Away.team = ifelse(Away.team == "Geelong Cats", "Geelong", Away.team)) %>%  
  mutate(Home.team = ifelse(Home.team == "GWS GIANTS", "GWS", Home.team),
         Away.team = ifelse(Away.team == "GWS GIANTS", "GWS", Away.team)) %>%
  ungroup() %>%
  select(-Round)


results_24 <- fitzRoy::fetch_results_afltables(2024) %>%
  mutate(Round.Number =ifelse(is.na(Round.Number), sub('.', '', Round) ,Round.Number),
         Round.Number = as.integer(Round.Number),
         Round.Number = ifelse(Round.Number < 10, paste0("0",Round.Number), Round.Number)) %>%
  mutate(seas_rnd =as.numeric(paste0(Season, Round.Number)))

result_24_withstats <- results_24 %>%
  select(-Venue) %>%
  left_join(add_2024_stats_clean, by = c("Date",
                              "Home.Team"= "Home.team",
                              "Season",
                              "Away.Team"="Away.team"))  

result_orig_withstats24 <- rbind(result_orig_withstats, result_24_withstats) 

# nah <- result_orig_withstats24 %>% 
  # summarise(across(everything(), ~ sum(is.na(.x))))
# fixture_footywire <- fitzRoy::fetch_fixture_footywire(2024)
# fix_24 <- fitzRoy::fetch_fixture_squiggle(2024)
f24 <- fitzRoy::fetch_fixture(2024)



away_state <- f24 %>% 
  distinct(home.team.club.name, venue.state)%>% 
  arrange(tolower(home.team.club.name)) %>% 
  select("Away.team" = home.team.club.name,"away.state" =venue.state) %>% 
  mutate(is_home_state = 1) %>%
  filter( away.state != "SA" | Away.team == "Adelaide Crows" | Away.team == "Port Adelaide" ) %>%
  mutate(Away.team = ifelse(Away.team == "Western Bulldogs", "Footscray", Away.team)) %>%
  mutate(Away.team = ifelse(Away.team == "Gold Coast SUNS", "Gold Coast", Away.team)) %>%
  mutate(Away.team = ifelse(Away.team == "West Coast Eagles", "West Coast", Away.team)) %>%
  mutate(Away.team = ifelse(Away.team == "Sydney Swans", "Sydney", Away.team)) %>%
  mutate(Away.team = ifelse(Away.team == "Adelaide Crows", "Adelaide", Away.team)) %>%
  mutate(Away.team = ifelse(Away.team == "Geelong Cats", "Geelong", Away.team)) %>%  
  mutate(Away.team = ifelse(Away.team == "GWS GIANTS", "GWS", Away.team))

home_state <- f24 %>% 
  distinct(home.team.club.name, venue.state) %>% 
  arrange(tolower(home.team.club.name)) %>% 
  select("Home.team" = home.team.club.name,"home.state" =venue.state) %>% 
  mutate(is_home_state_home = 1) %>%
  filter( home.state != "SA" | Home.team == "Adelaide Crows" | Home.team == "Port Adelaide") %>%
  mutate(Home.team = ifelse(Home.team == "Western Bulldogs", "Footscray", Home.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Gold Coast SUNS", "Gold Coast", Home.team)) %>%
  mutate(Home.team = ifelse(Home.team == "West Coast Eagles", "West Coast", Home.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Sydney Swans", "Sydney", Home.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Adelaide Crows", "Adelaide", Home.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Geelong Cats", "Geelong", Home.team)) %>%  
  mutate(Home.team = ifelse(Home.team == "GWS GIANTS", "GWS", Home.team))

fix_24clean <- f24 %>% 
  mutate(year = year(utcStartTime)) %>% 
  select("Date" = utcStartTime, "Season" = year ,  "Round" = round.roundNumber, 
         "Home.team"= home.team.club.name ,"hscore"= home.score.totalScore,    
         "Away.team" = away.team.club.name ,"ascore"= away.score.totalScore, 
         Venue = "venue.name", "state" =venue.state) %>%
  mutate(Home.team = ifelse(Home.team == "Western Bulldogs", "Footscray", Home.team),
         Away.team = ifelse(Away.team == "Western Bulldogs", "Footscray", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Gold Coast SUNS", "Gold Coast", Home.team),
         Away.team = ifelse(Away.team == "Gold Coast SUNS", "Gold Coast", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "West Coast Eagles", "West Coast", Home.team),
         Away.team = ifelse(Away.team == "West Coast Eagles", "West Coast", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Sydney Swans", "Sydney", Home.team),
         Away.team = ifelse(Away.team == "Sydney Swans", "Sydney", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Adelaide Crows", "Adelaide", Home.team),
         Away.team = ifelse(Away.team == "Adelaide Crows", "Adelaide", Away.team)) %>%
  mutate(Home.team = ifelse(Home.team == "Geelong Cats", "Geelong", Home.team),
         Away.team = ifelse(Away.team == "Geelong Cats", "Geelong", Away.team)) %>%  
  mutate(Home.team = ifelse(Home.team == "GWS GIANTS", "GWS", Home.team),
         Away.team = ifelse(Away.team == "GWS GIANTS", "GWS", Away.team)) %>% 
  left_join(home_state, by = c("Home.team", "state" = "home.state")) %>% 
  left_join(away_state, by = c("Away.team", "state" = "away.state")) %>% 
  mutate(is_home_state_home = ifelse(is.na(is_home_state_home), 0 , 1))%>% 
  mutate(is_home_state = ifelse(is.na(is_home_state), 0 , 1)) %>% 
  mutate(is_home_adv = ifelse(is_home_state_home == is_home_state, 0,1))


marg_elo = 80
# map_margin_to_outcome <- function(margin, marg.max = marg_elo, marg.min = -marg_elo){
#   margin <- ifelse(margin > 80, 80, margin)
#   margin <- ifelse(margin < -80, -80, margin)
#   norm <- (margin - marg.min)/(marg.max - marg.min)# ((asin((margin/marg_elo)))*2) + 0.5 #(margin - marg.min)/(marg.max - marg.min)
#   norm %>% pmin(1) %>% pmax(0)
# }

map_margin_to_outcome <- function(score, marg_elo = 80) {
  # Ensure the score is within the bounds of [-80, 80]
  score <- pmin(pmax(score, -80), 80)
  
  # Inverse the score transformation
  elo_perc <- (asin(score / marg_elo) / pi) + 0.5
  
  return(elo_perc)
}
# map_score_to_elo(27.09)
# map_elo_to_score(0.61)

# map_margin_to_outcome(80)
# ((asin((90/marg_elo)))/pi) + 0.5

marg_elo = 80
map_elo_to_score <- function(elo_perc, marg.max = marg_elo, marg.min = -marg_elo){
  # score <- (elo_perc*(marg.max - marg.min)) + marg.min

  score <- (sin((elo_perc - 0.5) * pi)) * marg_elo #(elo_perc - 0.5)^2 * 320 * sign(elo_perc - 0.5)
  score %>% pmin(80) %>% pmax(-80)
}
# y= (x-h)^2 + k
# score = sin((elo_perc - 0.5) * pi) 
# ((asin((12/marg_elo)))*2.5) + 0.5

# map_elo_to_score <- function(elo_perc, marg.max = marg_elo, marg.min = -marg_elo){
#   score <- (elo_perc*(marg.max - marg.min)) + marg.min
#   score %>% pmin(marg_elo) %>% pmax(-marg_elo)
# }
# map_elo_to_score(0.61)
# map_elo_to_score(seq(0,1,0.01))
# plot(map_elo_to_score(seq(0,1,0.01)))
# plot(map_margin_to_outcome(seq(-80,80,1)))




# Getting elo numbers for all

exp_score_lm_homefull <- result_orig_withstats24  %>% 
  mutate(home.scoring.shots = Home.Goals + Home.Behinds) %>% 
  lm(Home.Points ~ hshotsAtGoal  + hMarks.Inside.50   +
      hclearances, data = .)

exp_score_lm_awayfull <- result_orig_withstats24  %>% 
  mutate(away.scoring.shots = Away.Goals + Away.Behinds) %>% 
  lm(Away.Points ~  ashotsAtGoal  + aMarks.Inside.50   +
      aclearances , data = .)

results_withlmfull <- result_orig_withstats24 %>% 
  filter(!is.na(hhitouts)) %>% 
  mutate(newexpscore_home = round(predict(exp_score_lm_homefull, newdata = .),2),
         newexpscore_away =round( predict(exp_score_lm_awayfull, newdata = .),2))%>%
  mutate(homediff = Home.Points - newexpscore_home,
         awaydiff = Away.Points - newexpscore_away, 
         exp_margin = newexpscore_home - newexpscore_away, 
         real_margin = Margin, 
         marg_diff = Margin - exp_margin)


# Set parameters
HGA <- 23# home ground advantage
HGA_same_state <- 3# home ground advantage
carryOver <- 0.13 # season carry over
k_val <- 35# update weighting factor

# map_margin_to_outcome(12)
# Run ELO
# ?elo.run
elo.data_exp_lmfull <- elo.run(
  map_margin_to_outcome(exp_margin) ~ # real_margin # exp_margin
    adjust(Home.Team, HGA) +
    Away.Team +
    regress(Season, 1500, carryOver) +
    group(seas_rnd),
  initial.elos = 1500,
  k = k_val, 
  history = T,
  data = results_withlmfull
)


# abs <- as.data.frame(elo.data_exp_lmfull)
# %>%
# mutate(ar = row_number())#
fullelo <- cbind(results_withlmfull,as.data.frame(elo.data_exp_lmfull))%>% 
  mutate(seas_rnd = as.numeric(seas_rnd)) %>% 
  mutate(rnd_rank = dense_rank(seas_rnd)) %>% 
  mutate(pred_marg = map_elo_to_score(p.A),
         bigdiff = elo.A - elo.B) %>% 
  select("Game", "Date", "Round", "Home.Team", "Home.Goals", "Home.Behinds", 
         "Home.Points", "Away.Team", "Away.Goals", "Away.Behinds", "Away.Points", 
         "Margin", "Season", "Round.Type", "Round.Number", "seas_rnd", 
         "Venue",  "newexpscore_home", 
         "newexpscore_away", "homediff", "awaydiff", "exp_margin", "real_margin", 
         "marg_diff", "team.A", "team.B", "p.A", "wins.A", "update.A", 
         "update.B", "elo.A", "elo.B", "rnd_rank", "pred_marg", "bigdiff"
  )

# dput(names(fullelo))
# b <- as.data.frame(as.matrix(elo.data_exp_lmfull)) #%>% tail()
current_elo_vec <- ( final.elos(elo.data_exp_lmfull))
team_name <- results_withlmfull %>% distinct(Home.Team) %>% arrange(tolower( Home.Team)) %>%  pull(Home.Team)

current_elo_home <- data.frame(team_name, current_elo_vec) %>% rename("home_elo" = current_elo_vec)
current_elo_away <- data.frame(team_name, current_elo_vec) %>% rename("away_elo" = current_elo_vec)


fixture_exp_pred_lm <- fix_24clean %>%
  arrange(Date) %>% 
  left_join(current_elo_home, by = c("Home.team" = "team_name")) %>% 
  left_join(current_elo_away, by = c("Away.team" = "team_name")) %>% 
  mutate(elo_prob_home =ifelse(is_home_adv == 1 | Venue == "GMHBA Stadium",  elo.prob(home_elo+HGA, away_elo), elo.prob(home_elo+HGA_same_state, away_elo) ),
         # elo_prob_home = ifelse(Home.team == "Essendon", elo.prob(home_elo+60, away_elo), elo_prob_home ), 
         elo_prob_home = ifelse(Home.team == "Gold Coast", elo.prob(home_elo+60, away_elo), elo_prob_home ), 
         # elo_prob_home = ifelse(Home.team == "Collingwood", elo.prob(home_elo-18, away_elo), elo_prob_home ), 
         # elo_prob_home = ifelse(Away.team == "Essendon", elo.prob(home_elo, away_elo+60), elo_prob_home ), 
         elo_prob_away = 1 - elo_prob_home, 
         pred_margin = map_elo_to_score(elo_prob_home), 
         winner_name = ifelse(elo_prob_home > 0.5, Home.team, Away.team), 
         winner_prob = ifelse(elo_prob_home > 0.5,percent( elo_prob_home,0.01), percent( elo_prob_away,0.01)),
         winner_margin =ifelse(-1 < pred_margin & pred_margin < 1,1,  round( abs(pred_margin))), 
         actual_winner = ifelse(hscore > ascore, Home.team, Away.team) , 
         actual_margin = abs(hscore - ascore)) 


round <- 28
round_pred_2024 <- fixture_exp_pred_lm %>%
  filter(Round == round) %>%
  select( "Season", "Round", "Home.team", "Away.team",
          "Venue", "state", "winner_name",
          "winner_prob", "winner_margin")
write.csv(round_pred_2024, paste0("results/round",round,"_2024.csv"), row.names = F)
          
          

gm_auth_configure(path = "gmailsec.json")

my_email <- gm_mime() %>% 
  gm_to("chakri.mamidanna@reece.com.au") %>% 
  gm_from("chakrimamidanna@gmail.com") %>% 
  gm_subject(paste0("Round ", round)) %>% 
  gm_text_body(paste0("Chakri's round ", round, " AFL tips")) %>% 
  gm_attach_file(paste0("results/round",round,"_2024.csv"))

gm_send_message(my_email)

# 
# all_preds <- read.csv("round0_2024.csv")
# 
# for (i in 1:9) {
#   temp_preds <- read.csv( paste0("round",i,"_2024.csv"))
#   all_preds <- rbind(all_preds, temp_preds)
# }

# all_preds <- write.csv(all_preds, "season_predictions.csv", row.names = F)

fees = sale * 0.022 + 2500

for (i in c(450000, 455000, 460000, 465000, 470000,500000)) {
  fees = i*0.022 + 2500
  print(i)
  print( fees)
}


library(gmapsdistance)
chakri <- "3 Dent Court, Thomastown VIC"
kirti <- "5 Bickley Ct, Sunshine West VIC"
house <- "13 John Robert Place, Taylors Hill, Vic 3037"
gmapsdistance(origin = c(chakri,kirti), 
              destination = c(house,house), 
              mode = "driving", 
              dep_date = "2024-09-24", 
              dep_time = "19:00:00")

gmapsdistance(origin = kirti, 
              destination = house, 
              mode = "driving")


