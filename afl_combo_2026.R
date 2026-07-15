library(tidyverse)
setwd("~/Documents/r_repos/aflstats")

round <- 19

ai_preds <- read.csv(paste0("test26/chakri_round_optibits", round, ".csv")) %>%
  mutate(type = "ai")

elo_preds <- read.csv(paste0("elo26/round",round,"_2026.csv")) %>%
  mutate(type = "elo")
# # %>%
# #   right_join(s25_res, by = c("RoundNumber", "HomeTeam", "AwayTeam")) %>%
# #   mutate(marg_diff = Margin - PredictedMargin)
#
lm_preds <- read.csv(paste0("lm26/chakri_round_optibits",round,".csv")) %>%
  mutate(type = "lm")


mixed <- rbind( elo_preds, ai_preds)%>%
  mutate(PredictedMargin = round(PredictedMargin, 6), 
         homewin = ifelse(HomeProbability > 0.5, 1, 0)) %>%
  group_by(RoundNumber, HomeTeam) %>%
  mutate(HomeProbability = case_when(mean(homewin) == 1 ~  max(HomeProbability) + ( 0.25*(max(HomeProbability)  - min(HomeProbability))),
                                      mean(homewin) == 0 ~ min(HomeProbability) - ( 0.25*(max(HomeProbability)  - min(HomeProbability))),
                                      TRUE ~ mean(HomeProbability)),
         HomeProbability = case_when(HomeProbability > 1 ~ .099, 
                                     HomeProbability < 0 ~ 0.01, 
                                     TRUE ~ HomeProbability),
         PredictedMargin2 = max((ifelse(type == "elo" & HomeProbability > 0.5, PredictedMargin, 0))),
         PredictedMargin3 = min((ifelse(type == "elo" & HomeProbability < 0.5, PredictedMargin, 0))), 
         PredictedMargin = ifelse(HomeProbability > 0.5, PredictedMargin2, PredictedMargin3)) %>%
  ungroup() %>%
  mutate(Winner = ifelse(HomeProbability > 0.5, HomeTeam, AwayTeam)) %>%
  select(-type) %>%
  distinct()%>%
  select("RoundNumber", "HomeTeam", "AwayTeam", "Winner", "HomeProbability",
         "VenueName", "PredictedMargin")  %>%
  distinct()

write.csv(mixed, paste0("mix26/round",round,"_2026.csv"), row.names = F)





seas_preds <- read.csv(paste0("mix26/mix_2026_allpreds.csv"))
seas_preds <- rbind(seas_preds, mixed)
write.csv(seas_preds, "mix26/mix_2026_allpreds.csv", row.names = F)


seas_preds <- read.csv(paste0("elo26/chakri_2026_allpreds.csv"))
seas_preds <- rbind(seas_preds, mixed)
write.csv(seas_preds, "elo26/chakri_2026_allpreds.csv", row.names = F)


