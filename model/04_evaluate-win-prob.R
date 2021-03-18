library(tidyverse)
library(yaml)
library(tidypredict)
library(glue)

predictors = read_rds('model/files/predictors.rds')

predictors

summary = read_csv('scrape/parse/parsing-summary.csv')

summary

test = predictors %>% filter(season == 2019)

test

test %>% write_rds('model/files/testing.rds', compress = 'gz')

test.calcs = test %>% 
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

test.calcs

predictions = test.calcs %>% 
  select(clean.minute, predictions) %>% 
  unnest(c(predictions)) %>% 
  arrange(season, matchid, home, clean.minute)

predictions

error.by.minute = predictions %>% 
  group_by(clean.minute) %>% 
  summarise(
    error = mean((fit - goals.left)^2)
  )

plot.error.by.minute = error.by.minute %>% 
  ggplot(aes(clean.minute, error)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = . %>% filter(clean.minute %in% c(45, 90)),
    color = 'red'
  ) + 
  labs(
    title = 'Mean squared error by minute'
  ) +
  theme_minimal()

ggsave(filename = 'model/files/error-by-minute.png', plot = plot.error.by.minute)

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
    
    prob.h = case_when(
      clean.minute == 90 & goals.h > goals.a ~ 1,
      clean.minute == 90 & goals.h <= goals.a ~ 0,
      TRUE ~ prob.h
    ),
    prob.t = case_when(
      clean.minute == 90 & goals.h == goals.a ~ 1,
      clean.minute == 90 & goals.h != goals.a ~ 0,
      TRUE ~ prob.t
    ),
    prob.a = case_when(
      clean.minute == 90 & goals.a > goals.h ~ 1,
      clean.minute == 90 & goals.a <= goals.h ~ 0,
      TRUE ~ prob.a
    )
  )

probabilities

plot.mse.by.hta = probabilities %>% 
  group_by(clean.minute) %>% 
  summarise(
    h = mean(((goals.h > goals.a) - prob.h) ^ 2),
    a = mean(((goals.a > goals.h) - prob.a) ^ 2),
    t = mean(((goals.h == goals.a) - prob.t) ^ 2),
  ) %>% 
  pivot_longer(-clean.minute, values_to = 'side') %>% 
  ggplot(aes(clean.minute, side, color = name)) + 
  geom_line() +
  labs(
    title = 'Mean squared error by prediction type',
    y = 'MSE'
  ) +
  theme_minimal()

plot.mse.by.hta

ggsave(filename = 'model/files/prob-error-by-home-away-tie.png', plot.mse.by.hta)

## plot random game

# probabilities %>% 
#   select(season, matchid, clean.minute, event.order, h = prob.h, a = prob.a, t = prob.t) %>% 
#   pivot_longer(-season:-event.order, names_to = 'side', values_to = 'prob') %>% 
#   mutate(side = fct_relevel(side, 'h', 't', 'a')) %>% 
#   group_by(season, matchid) %>% 
#   nest() %>% 
#   ungroup() %>% 
#   left_join(summary) %>% 
#   filter(matchid == '6defd3a2') %>% 
#   # view() %>% 
#   # sample_n(1) %>% 
#   mutate(
#     plot = pmap(
#       list(data, team.h, team.a, score.h, score.a),
#       ~..1 %>%
#         # pivot_longer(-clean.minute, names_to = 'side', values_to = 'prob') %>%
#         # mutate(side = fct_relevel(side, 'h', 't', 'a')) %>%
#         ggplot(aes(x = clean.minute, y = prob, fill = side, color = side)) +
#         # geom_line() +
#         geom_area() +
#         labs(
#           title = glue::glue('{..2} {..4}-{..5} {..3}')
#         ) +
#         theme_minimal()
#     )
#   ) %>%
#   pull(plot)
