library(tidyverse)

matrix = read_csv('eng/matrix.csv')

matrix

predictors = bind_rows(
  matrix %>% 
    transmute(
      season, matchid, 
      goals.left = hscore - hgoals,
      event.order, clean.minute, stoppage.minute,
      home = 1,
      spi.edge = hspi - aspi,
      goals = hgoals,
      goals.edge = hgoals - agoals,
      men.edge = hmen - amen,
    ),
  matrix %>% 
    transmute(
      season, matchid, 
      goals.left = ascore - agoals,
      event.order, clean.minute, stoppage.minute,
      home = 0,
      spi.edge = aspi - hspi,
      goals = agoals,
      goals.edge = agoals - hgoals,
      men.edge = amen - hmen,
    )
)
collect.matrix.for.side = function(side) {
  
}

pred.matrix = matrix %>% 
  select(
    season, matchid, minute = clean_minute,
    score = hscore, oscore = ascore,
    spi = hspi, ospi = aspi,
    goals = hgoals, ogoals = agoals,
    men = hmen, omen = amen
  ) %>% 
  mutate(home = 1) %>% 
  bind_rows(
    matrix %>% 
      select(
        season, matchid, minute = clean_minute,
        score = ascore, oscore = hscore,
        spi = aspi, ospi = hspi,
        goals = agoals, ogoals = hgoals,
        men = amen, omen = hmen
      ) %>% 
      mutate(home = 0)
  )

pred.matrix

goal.pred = glm(
  score ~ spi + ospi + home + minute + goals + ogoals + men + omen,
  data = pred.matrix,
  family = 'poisson'
)

# goal.pred = lm(
#   log(score) ~ ns(spi, 3) + ns(ospi, 3) + home + ns(minute, 3) + ns(goals, 3) + ns(ogoals, 3) + ns(men, 3) + ns(amen, 3),
#   data = pred.matrix
# )

goal.pred

probs = pred.matrix %>% 
  mutate(exp.goals = predict(goal.pred, newdata = ., type = 'response')) %>% 
  select(season, matchid, minute, exp.goals, home) %>% 
  distinct(season, matchid, minute, home, .keep_all = TRUE) %>% 
  mutate(home = if_else(home == 1, 'hexp', 'aexp')) %>% 
  pivot_wider(names_from = home, values_from = exp.goals) %>% 
  mutate(
    score.matrix = map2(
      hexp,
      aexp,
      ~{
        max.goals = 10
        hgoaldist = dpois(0:max.goals, .x)
        agoaldist = dpois(0:max.goals, .y)
        matrix = hgoaldist %o% agoaldist
        # matrix = matrix / sum(matrix)
        matrix
      }
    ),
    probtie = map_dbl(score.matrix, ~sum(diag(.x))),
    prob1 = map_dbl(score.matrix, ~sum(.x[lower.tri(.x)])),
    prob2 = map_dbl(score.matrix, ~sum(.x[upper.tri(.x)])),
    sumprob = probtie + prob1 + prob2
  )

probs

probs %>% filter(minute == 90)

probs2 = pred.matrix %>% 
  drop_na(minute) %>% 
  group_by(minute) %>% 
  nest() %>% 
  mutate(
    model = map(
      data,
      ~glm(
        score ~ spi + ospi + home + goals + ogoals + men + omen,
        data = .x,
        family = 'poisson'
      )
    ),
    preds = map2(
      data,
      model,
      ~.x %>% 
        mutate(exp.goals = predict(.y, newdata = ., type = 'response'))
    )
  ) %>%  
  select(minute, preds) %>% 
  unnest(c(preds)) %>% 
  select(season, matchid, minute, exp.goals, home) %>% 
  distinct(season, matchid, minute, home, .keep_all = TRUE) %>% 
  mutate(home = if_else(home == 1, 'hexp', 'aexp')) %>% 
  pivot_wider(names_from = home, values_from = exp.goals) %>% 
  mutate(
    score.matrix = map2(
      hexp,
      aexp,
      ~{
        max.goals = 10
        hgoaldist = dpois(0:max.goals, .x)
        agoaldist = dpois(0:max.goals, .y)
        matrix = hgoaldist %o% agoaldist
        # matrix = matrix / sum(matrix)
        matrix
      }
    ),
    probtie = map_dbl(score.matrix, ~sum(diag(.x))),
    prob1 = map_dbl(score.matrix, ~sum(.x[lower.tri(.x)])),
    prob2 = map_dbl(score.matrix, ~sum(.x[upper.tri(.x)])),
    sumprob = probtie + prob1 + prob2
  )
  
  

probs2


probs2 %>% filter(minute == 90)

probs$score.matrix[[1]]
  filter(minute >= 89) %>% 
  arrange(desc(season), matchid)
