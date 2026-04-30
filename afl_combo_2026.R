library(tidyverse)

round <- 8

ai_preds <- read.csv(paste0("test26/chakri_round_optibits", round, ".csv"))

elo_preds <- read.csv(paste0("elo26/round",round,"_2026.csv")) %>%
  mutate(type = "elo")
# # %>%
# #   right_join(s25_res, by = c("RoundNumber", "HomeTeam", "AwayTeam")) %>%
# #   mutate(marg_diff = Margin - PredictedMargin)
#
lm_preds <- read.csv(paste0("lm26/chakri_round_optibits",round,".csv")) %>%
  mutate(type = "lm")


mixed <- rbind(lm_preds, elo_preds)%>%
  mutate(PredictedMargin = round(PredictedMargin, 6)) %>%
  group_by(RoundNumber, HomeTeam) %>%
  mutate(HomeProbability = mean(HomeProbability),
         PredictedMargin = mean(PredictedMargin)) %>%
  ungroup() %>%
  mutate(Winner = ifelse(HomeProbability > 0.5, HomeTeam, AwayTeam)) %>%
  select(-type) %>%
  distinct()%>%
  select("RoundNumber", "HomeTeam", "AwayTeam", "Winner", "HomeProbability",
         "VenueName", "PredictedMargin")

write.csv(mixed, paste0("mix26/round",round,"_2026.csv"), row.names = F)

seas_preds <- read.csv(paste0("mix26/mix_2026_allpreds.csv"))
seas_preds <- rbind(seas_preds, mixed)
write.csv(seas_preds, "mix26/mix_2026_allpreds.csv", row.names = F)


seas_preds <- read.csv(paste0("elo26/chakri_2026_allpreds.csv"))
seas_preds <- rbind(seas_preds, mixed)
write.csv(seas_preds, "elo26/chakri_2026_allpreds.csv", row.names = F)


