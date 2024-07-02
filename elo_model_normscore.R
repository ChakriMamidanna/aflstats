library(fitzRoy)
library(tidyverse)
library(elo)
library(lubridate)




results_orig <- fitzRoy::fetch_results_afltables(2010:2023)
results <- results_orig %>% filter(Date > "2010-01-01", Date < "2023-01-01") %>%
  mutate(seas_rnd = paste0(Season, ".", Round.Number))
# fixture_footywire <- fitzRoy::fetch_fixture_footywire(2024)
fix_24 <- fitzRoy::fetch_fixture_squiggle(2024)

fix_24clean <- fix_24 %>% 
  select("Date" = date, "Season" = year ,  "Round" = round, "Home.Team"= hteam ,     
         "Away.Team" = ateam ,  Venue = "venue") %>%
  filter(Round <= 24) %>% 
  # mutate(Date = ymd(format(Date, "%Y-%m-%d")))%>%
  rename(Round.Number = `Round`) %>% 
  mutate(Home.Team = ifelse(Home.Team == "Western Bulldogs", "Footscray", Home.Team), 
         Away.Team = ifelse(Away.Team == "Western Bulldogs", "Footscray", Away.Team)) %>% 
  mutate(Home.Team = ifelse(Home.Team == "Greater Western Sydney", "GWS", Home.Team), 
         Away.Team = ifelse(Away.Team == "Greater Western Sydney", "GWS", Away.Team))


fixture <- results_orig %>% filter(Date > "2023-01-01")



# Set parameters
HGA <- 10 # home ground advantage
carryOver <- 0.25 # season carry over
k_val <- 15 # update weighting factor

map_margin_to_outcome <- function(margin, marg.max = 80, marg.min = -80){
  norm <- (margin - marg.min)/(marg.max - marg.min)
  norm %>% pmin(1) %>% pmax(0)
}


# Run ELO
elo.data <- elo.run(
  map_margin_to_outcome(Home.Points - Away.Points) ~
    adjust(Home.Team, HGA) +
    Away.Team +
    regress(Season, 1000, carryOver) +
    group(seas_rnd),
  initial.elos = 1000,
  k = k_val,
  data = results
)



a <- cbind(results,as.data.frame(elo.data)) %>% mutate(pred_marg = map_elo_to_score(p.A))  #%>% tail()
b <- as.matrix(elo.data) #%>% tail()
c <- final.elos(elo.data)
# elo::rank.teams(elo.data)

map_elo_to_score <- function(elo_perc, marg.max = 80, marg.min = -80){
  score <- (elo_perc*(marg.max - marg.min)) + marg.min
  score %>% pmin(80) %>% pmax(-80)
}

fixture_pred <- fixture %>%
  # filter(Round.Type == "Regular") %>%
  mutate(Prob = predict(elo.data, newdata = .)) %>% 
  mutate(pred_margin = map_elo_to_score(Prob)) %>% 
  # filter(Round.Type == "Regular") %>%
  mutate(accurate_pred = case_when(Margin < 0 & Prob < 0.5 ~ 1, 
                                   Margin > 0 & Prob > 0.5 ~ 1,
                                   Margin == 0 & Prob == 0.5 ~ 1,
                                   TRUE ~ 0 ))

pred_acc <- fixture_pred %>% 
  group_by(Season) %>% 
  summarise(acc = sum(accurate_pred)/n(), 
            tot_right = sum(accurate_pred), 
            tot_games = n())

# map_elo_to_score(0.5284122)


fixture_pred24 <- fix_24clean %>%
  mutate(Prob = predict(elo.data, newdata = fix_24clean)) %>% 
  mutate(pred_margin = map_elo_to_score(Prob)) 

