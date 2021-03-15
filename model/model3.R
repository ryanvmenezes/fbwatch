library(tidyverse)

summaries = read_csv('eng/summaries.csv')

summaries

matrix = read_csv('eng/matrix.csv')

matrix

predictors = bind_rows(
  matrix %>% 
    transmute(
      season, matchid, 
      goals.left = hscore - hgoals,
      event.order, clean.minute,
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
      event.order, clean.minute,
      home = 0,
      spi.edge = aspi - hspi,
      goals = agoals,
      goals.edge = agoals - hgoals,
      men.edge = amen - hmen,
    )
)

predictors

predictors %>% count(season)

train = predictors %>% filter(season < '2019-2020')
test = predictors %>% filter(season == '2019-2020')

models = train %>% 
  distinct(clean.minute) %>% 
  mutate(
    data = map(
      clean.minute,
      ~{
        if (.x == 0 | .x == 90) {
          filtered = train %>% filter(clean.minute == .x)
        } else {
          filtered = train %>% 
            filter(clean.minute >= .x - 2, clean.minute <= .x + 2)
        }
        
        filtered
      }
    )
  ) %>% 
  mutate(
    model = map(
      data,
      ~glm(
        goals.left ~ goals.edge + spi.edge + men.edge + home,
        data = .x,
        family = 'poisson'
      )
    )
  )

models %>% tail()

calcs = predictors %>% 
  group_by(clean.minute) %>% 
  nest() %>% 
  left_join(
    models %>% select(clean.minute, model)
  ) %>% 
  mutate(
    preds = map2(
      data,
      model,
      ~.x %>% 
        mutate(pred.goals.left = predict(.y, newdata = ., type = 'response'))
    )
  ) %>% 
  select(-data, -model) %>% 
  unnest(c(preds)) %>%
  select(season, matchid, event.order, clean.minute, home, pred.goals.left, goals) %>% 
  arrange(season, matchid, event.order) %>% 
  mutate(home = if_else(home == 1, 'h', 'a')) %>% 
  pivot_wider(names_from = home, values_from = c(pred.goals.left, goals), names_sep = '.') %>% 
  mutate(
    dist.h = map(pred.goals.left.h, ~dpois(0:10, .x)),
    dist.a = map(pred.goals.left.a, ~dpois(0:10, .x)),
    dist.h = map2(dist.h, goals.h, ~c(rep(0, .y), .x)),
    dist.a = map2(dist.a, goals.a, ~c(rep(0, .y), .x)),
    
    matrix = map2(dist.h, dist.a, ~.x %o% .y),
    
    probtie = map_dbl(matrix, ~sum(diag(.x))),
    prob.h = map_dbl(matrix, ~sum(.x[lower.tri(.x)])),
    prob.a = map_dbl(matrix, ~sum(.x[upper.tri(.x)])),
  )

calcs

calcs %>%
  select(-dist.h, -dist.a, -matrix) %>% 
  write_csv('eng/probabilities.csv')

errors = summaries %>% 
  right_join(calcs) %>% 
  select(-dist.h, -dist.a, -matrix) %>% 
  mutate(
    ist = goals.h == goals.a,
    ish = goals.h > goals.a,
    isa = goals.h < goals.a
  ) %>% 
  group_by(clean.minute) %>% 
  summarise(
    errt = mean((probtie - ist)^2),
    errh = mean((prob.h - ish)^2),
    erra = mean((prob.a - isa)^2),
  )

errors

errors %>% 
  pivot_longer(-clean.minute, names_to = 'err', values_to = 'prob') %>% 
  ggplot() +
  geom_line(aes(clean.minute, prob, color = err)) +
  scale_y_continuous(limits = c(0, 0.75))

summaries %>% 
  select(season, matchid, hteam, ateam, hscore, ascore) %>% 
  right_join(
    calcs %>% 
      group_by(season, matchid, clean.minute) %>% 
      filter(event.order == max(event.order)) %>% 
      ungroup() %>% 
      select(season, matchid, clean.minute, tie = probtie, h = prob.h, a = prob.a)
  ) %>%
  group_by(season, matchid, hteam, ateam, hscore, ascore) %>% 
  nest() %>% 
  ungroup() %>% 
  # sample_n(1) %>% 
  filter(matchid == '33b8c50a') %>% 
  mutate(
    plot = pmap(
      list(data, hteam, ateam, hscore, ascore),
      ~..1 %>%
        pivot_longer(-clean.minute, names_to = 'side', values_to = 'prob') %>% 
        mutate(side = fct_relevel(side, 'h', 'tie', 'a')) %>% 
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

