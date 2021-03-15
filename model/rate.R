library(tidyverse)

summaries = read_csv('eng/summaries.csv')

summaries

probs = read_csv('eng/probabilities.csv')

probs

swings = probs %>% 
  group_by(season, matchid) %>% 
  mutate(
    lagt = abs(probtie - lag(probtie)),
    lagh = abs(prob.h - lag(prob.h)),
    laga = abs(prob.a - lag(prob.a)),
  ) %>% 
  # group_by(season, matchid) %>% 
  summarise(
    swings = sum(lagt, na.rm = TRUE) + sum(lagh, na.rm = TRUE) + sum(laga, na.rm = TRUE)
  )

swings

tension = probs %>% 
  group_by(season, matchid) %>% 
  mutate(
    tenst = abs(probtie - 0.33),
    tensh = abs(prob.h - 0.33),
    tensa = abs(prob.a - 0.33)
  ) %>% 
  summarise(
    tension = sum(tenst) + sum(tensa) + sum(tensh)
  ) %>% 
  arrange(tension)

summaries %>%
  left_join(swings) %>%
  left_join(tension) %>% 
  mutate(
    swingsp = cut_number(swings, 100),
    swingsp = as.numeric(swingsp),
    tensionp = cut_number(tension, 100),
    tensionp = 101 - as.numeric(tensionp),
  ) %>% 
  arrange(tension) %>% 
  view()