library(tidyverse)
library(yaml)
library(tidypredict)
library(glue)

predictors = read_rds('model/files/predictors.rds')

predictors

summary = read_csv('scrape/parse/parsing-summary.csv')

summary

calcs = predictors %>% 
  group_by(clean.minute) %>% 
  nest() %>% 
  mutate(
    model = map(
      clean.minute,
      ~glue('model/files/minute-models/minute-{.x}.yaml') %>% 
        read_yaml() %>% 
        as_parsed_model()
    ),
    predictions = map2(
      data,
      model,
      ~.x %>% 
        tidypredict_to_column(.y)
    )
  )

calcs

predictions = calcs %>% 
  select(clean.minute, predictions) %>% 
  unnest(c(predictions)) %>% 
  arrange(season, matchid, home, clean.minute)

predictions

probabilities = predictions %>% 
  select(season, matchid, clean.minute, event.order, home, goals, pred.goals.left = fit) %>% 
  mutate(home = if_else(home == 1, 'h', 'a')) %>% 
  pivot_wider(names_from = home, values_from = c(goals, pred.goals.left), names_glue = "{.value}.{home}") %>% 
  ungroup() %>% 
  select(-pred.goals.left.a, pred.goals.left.a) %>% 
  mutate(
    dist.h = map(pred.goals.left.h, ~dpois(0:10, .x)),
    dist.a = map(pred.goals.left.a, ~dpois(0:10, .x)),
    dist.h = map2(dist.h, goals.h, ~c(rep(0, .y), .x)),
    dist.a = map2(dist.a, goals.a, ~c(rep(0, .y), .x)),
    
    matrix = map2(dist.h, dist.a, ~.x %o% .y),
    
    prob.h = map_dbl(matrix, ~sum(.x[lower.tri(.x)])),
    prob.t = map_dbl(matrix, ~sum(diag(.x))),
    prob.a = map_dbl(matrix, ~sum(.x[upper.tri(.x)])),
  ) %>% 
  group_by(season, matchid) %>% 
  mutate(
    prob.h = case_when(
      event.order == max(event.order) & goals.h > goals.a ~ 1,
      event.order == max(event.order) & goals.h <= goals.a ~ 0,
      TRUE ~ prob.h
    ),
    prob.t = case_when(
      event.order == max(event.order) & goals.h == goals.a ~ 1,
      event.order == max(event.order) & goals.h != goals.a ~ 0,
      TRUE ~ prob.t
    ),
    prob.a = case_when(
      event.order == max(event.order) & goals.a > goals.h ~ 1,
      event.order == max(event.order) & goals.a <= goals.h ~ 0,
      TRUE ~ prob.a
    )
  ) %>% 
  ungroup()

probabilities

swings = probabilities %>% 
  group_by(season, matchid) %>% 
  mutate(
    lagt = abs(prob.t - lag(prob.t)),
    lagh = abs(prob.h - lag(prob.h)),
    laga = abs(prob.a - lag(prob.a)),
  ) %>% 
  summarise(
    swings = sum(lagt, na.rm = TRUE) + sum(lagh, na.rm = TRUE) + sum(laga, na.rm = TRUE),
    .groups = 'drop'
  )

swings

tension = probabilities %>%
  select(season, matchid, clean.minute, starts_with('prob')) %>% 
  group_by(season, matchid) %>% 
  mutate(
    inith = case_when(clean.minute == 0 ~ prob.h),
    initt = case_when(clean.minute == 0 ~ prob.t),
    inita = case_when(clean.minute == 0 ~ prob.a),
  ) %>% 
  fill(initt, inith, inita, .direction = 'down') %>% 
  mutate(
    tenst = abs(prob.t - inith),
    tensh = abs(prob.h - initt),
    tensa = abs(prob.a - inita)
  ) %>% 
  summarise(
    tension = sum(tenst) + sum(tensa) + sum(tensh),
    .groups = 'drop'
  ) %>% 
  arrange(tension)

tension

min.prob = probabilities %>%
  left_join(summary %>% select(season, matchid, score.h, score.a)) %>% 
  mutate(
    result.prob = case_when(
      score.h > score.a ~ prob.h,
      score.a > score.h ~ prob.a,
      score.a == score.h ~ prob.t,
    )
  ) %>% 
  select(season, matchid, clean.minute, result.prob, starts_with('prob')) %>%
  group_by(season, matchid) %>% 
  summarise(min.prob = min(result.prob), .groups = 'drop')

min.prob

game.calcs = summary %>%
  left_join(swings) %>%
  left_join(tension) %>% 
  left_join(min.prob) %>% 
  mutate(
    swingsp = cut_number(swings, 100),
    swingsp = as.numeric(swingsp),
    tensionp = cut_number(tension, 100),
    tensionp = 101 - as.numeric(tensionp),
  )

game.calcs %>% 
  ggplot(aes(swings, tension)) + 
  geom_point()

game.calcs %>% 
  filter(swings > 2.5, tension > 100) %>% 
  view()

probabilities %>%
  select(season, matchid, clean.minute, event.order, h = prob.h, a = prob.a, t = prob.t) %>%
  pivot_longer(-season:-event.order, names_to = 'side', values_to = 'prob') %>%
  mutate(side = fct_relevel(side, 'h', 't', 'a')) %>%
  group_by(season, matchid) %>%
  nest() %>%
  ungroup() %>%
  left_join(summary) %>%
  filter(matchid == 'fdbbfb30') %>%
  # view() %>%
  # sample_n(1) %>%
  mutate(
    plot = pmap(
      list(data, team.h, team.a, score.h, score.a),
      ~..1 %>%
        # pivot_longer(-clean.minute, names_to = 'side', values_to = 'prob') %>%
        # mutate(side = fct_relevel(side, 'h', 't', 'a')) %>%
        ggplot(aes(x = clean.minute, y = prob, fill = side, color = side)) +
        # geom_line() +
        geom_area() +
        labs(
          title = glue::glue('{..2} {..4}-{..5} {..3}')
        ) +
        theme_minimal()
    )
  ) %>%
  pull(plot)
