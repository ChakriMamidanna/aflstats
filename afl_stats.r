library(dplyr)
library(fitzRoy)

setwd("~/Desktop/rgitstats")

afltables <- get_afltables_stats(start_date = '1990-01-01')

essgames <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score", "Away.score",
         "HQ1G", "HQ1B", "HQ2G", "HQ2B", "HQ3G", "HQ3B", "HQ4G", "HQ4B",  
         "AQ1G", "AQ1B", "AQ2G", "AQ2B", "AQ3G", "AQ3B", "AQ4G", "AQ4B" ) %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  filter((Home.team == 'Essendon' | Away.team == 'Essendon'  )) %>% 
  mutate(ess_score = ifelse(Home.team == 'Essendon', Home.score, Away.score),
         opp_score = ifelse(Home.team == 'Essendon', Away.score, Home.score), 
         diff = ess_score - opp_score)

dput(names(afltables))

most_behinds <- afltables %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score","HQ1G", "HQ1B", "HQ2G", "HQ2B", "HQ3G", "HQ3B", "HQ4G", "HQ4B",  
         "AQ1G", "AQ1B", "AQ2G", "AQ2B", "AQ3G", "AQ3B","HQ4G", "HQ4B", "Away.score",
         "AQ4G", "AQ4B") %>% 
  filter(Season > 2010) %>% 
  mutate(q1v = HQ1B + AQ1B,
         q2b = HQ2B + AQ2B,
         q3b = HQ3B + AQ3B,
           behinds = HQ4B + AQ4B)

accurate  <- afltables %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score","HQ4G", "HQ4B", "Away.score",
          "AQ4G", "AQ4B") %>% 
  filter((Home.score > 50 & HQ4B == 1 ) | (Away.score > 50 &  AQ4B == 1)) 

goals <- afltables %>% 
  filter(Season > 2014) %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance","First.name", 
         "Surname", "Playing.for", "Goals")%>% 
  arrange(-Goals) %>% 
  filter(Goals > 5) %>% 
  mutate(name = paste0(First.name, Surname, " ", Playing.for)) %>% 
  group_by(name) %>% 
  summarise(count = n())
  
carry90 <- afltables %>% 
#  filter(Season > 2014) %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance",
         "Home.team", "Away.team", "Home.score", "Away.score",
         "First.name", "Surname", "Playing.for", "Goals", "Behinds")%>% 
  mutate(playerscore = Goals*6 + Behinds, 
         score = ifelse(Playing.for == Home.team, Home.score, Away.score), 
         carry_perc = ifelse(Playing.for == Home.team, 100*playerscore/Home.score, 100*playerscore/Away.score)) %>% 
  arrange(-carry_perc) %>% 
  filter(score > 68, carry_perc > 40)
  
  
behind <- afltables %>% 
  filter(Season >= 2000) %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance","First.name", 
         "Surname", "Playing.for", "Goals" ,"Behinds") %>% 
  filter(Goals == 0 & Behinds > 0)
  
season_teams <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score", "Away.score") %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  filter(Round == 1) %>% 
  group_by(Season) %>% 
  summarise(total_teams = n() * 2)
  

table_round2 <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score", "Away.score") %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  mutate(Round = as.integer(Round)) %>% 
  filter(Round < 4 ) %>% 
  mutate(points_home = ifelse(Home.score - Away.score > 0, 4,0),
         points_away = ifelse(Home.score - Away.score > 0, 0,4), 
         team = ifelse(points_home == 4, Home.team, Away.team), 
         points = 4) %>%
  group_by(Season, team) %>% 
  summarise(wins = sum(points))%>% 
  ungroup() %>% 
  group_by(Season) %>% 
  summarise(teams = n()) %>% 
  left_join(season_teams, by = c("Season")) %>% 
  mutate(diff = total_teams - teams)


table_round2005 <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance", 
         "Home.team", "Away.team", "Home.score", "Away.score") %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) %>% 
  mutate(Round = as.integer(Round)) %>% 
  filter( Season == 2005, !is.na(Round)) %>% 
  mutate(points_home = ifelse(Home.score - Away.score > 0, 4,0),
         points_away = ifelse(Home.score - Away.score > 0, 0,4), 
         team = ifelse(points_home == 4, Home.team, Away.team), 
         points = 4) %>%
  group_by(Season, team) %>% 
  summarise(wins = sum(points)) %>% 
  arrange(-wins)

%>% 
  ungroup() %>% 
  group_by(Season) %>% 
  summarise(teams = n()) %>% 
  left_join(season_teams, by = c("Season")) %>% 
  mutate(diff = total_teams - teams)


indi_game <- afltables %>% 
 #filter(Surname == "Lucas", First.name == 'Scott') %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance",
         "Home.team", "Away.team", "Home.score", "Away.score",
         "First.name", "Surname", "Playing.for", "Goals", "Behinds")%>% 
  mutate(playerscore = Goals*6 + Behinds, 
         score = ifelse(Playing.for == Home.team, Home.score, Away.score), 
         otherscore = ifelse(Playing.for == Home.team, Away.score, Home.score),
         winner = ifelse(Home.score > Away.score, Home.team, Away.team), 
         loser = ifelse(Home.score < Away.score, Home.team, Away.team), 
         shots = Goals + Behinds) %>% 
  filter(loser == Playing.for, Goals > 4) %>% 
  group_by(First.name, Surname, Playing.for) %>% 
  summarise(count = n(), 
            year = max(Season))


debut <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance",
         "Home.team", "Away.team", "Home.score", "Away.score",
         "First.name", "Surname", "Playing.for", "Goals", "Behinds") %>% 
  arrange(Season) %>% 
  distinct(First.name, Surname,Playing.for , .keep_all = T) %>% 
  select(First.name, Surname,Playing.for, "debut" = Season)
  
goals_season <- afltables %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance",
         "Home.team", "Away.team", "Home.score", "Away.score",
         "First.name", "Surname", "Playing.for", "Goals", "Behinds") %>% 
  group_by(First.name, Surname, Playing.for, Season) %>% 
  summarise(goals = sum(Goals)) %>% 
  left_join(debut, by = c("First.name", "Surname", "Playing.for")) %>% 
  filter(debut > 1990, Season == debut)

misses <- afltables %>% 
  filter(Round == 'GF') %>% 
  select("Season", "Round", "Date",  "Venue", "Attendance",
         "Home.team", "Away.team", "Home.score", "Away.score",
           "HQ4G",  "AQ4G","HQ4B",  "AQ4B") %>% 
  distinct(Season, Round, Home.team, Away.team, .keep_all = T ) 

c("Season", "Round", "Date", "Local.start.time", "Venue", "Attendance", 
  "Home.team", "HQ1G", "HQ1B", "HQ2G", "HQ2B", "HQ3G", "HQ3B", 
  "HQ4G", "HQ4B", "Home.score", "Away.team", "AQ1G", "AQ1B", "AQ2G", 
  "AQ2B", "AQ3G", "AQ3B", "AQ4G", "AQ4B", "Away.score", "First.name", 
  "Surname", "ID", "Jumper.No.", "Playing.for", "Kicks", "Marks", 
  "Handballs", "Goals", "Behinds", "Hit.Outs", "Tackles", "Rebounds", 
  "Inside.50s", "Clearances", "Clangers", "Frees.For", "Frees.Against", 
  "Brownlow.Votes", "Contested.Possessions", "Uncontested.Possessions", 
  "Contested.Marks", "Marks.Inside.50", "One.Percenters", "Bounces", 
  "Goal.Assists", "Time.on.Ground..", "Substitute", "Umpire.1", 
  "Umpire.2", "Umpire.3", "Umpire.4", "group_id")

