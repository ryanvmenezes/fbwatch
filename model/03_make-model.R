library(tidyverse)
library(tidypredict)
library(glue)
library(yaml)

predictors = read_rds('model/files/predictors.rds')

predictors

predictors %>% count(season)

train = predictors %>% filter(season < 2019)
test = predictors %>% filter(season == 2019)

n_distinct(test$matchid)

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
    model = map2(
      data,
      clean.minute,
      ~{
        formula = goals.left ~ goals.edge + spi.edge + men.edge + home
        if (.y == 0) {
          formula = goals.left ~ spi.edge + home
        }
        glm(
          formula = formula,
          data = .x,
          family = 'poisson'
        )
      }
    )
  )

models %>% head()

models %>% tail()

models %>% 
  select(clean.minute, model) %>%
  pwalk(
    ~..2 %>% 
      parse_model() %>% 
      write_yaml(glue('model/files/minute-models/minute-{..1}.yaml'))
  )

loaded.models = models %>% 
  select(clean.minute) %>% 
  mutate(
    model = map(
      clean.minute,
      ~read_yaml(glue('model/files/minute-models/minute-{.x}.yaml')) %>% 
        as_parsed_model()
    )
  )

loaded.models

str(loaded.models$model[[1]])

tidypredict_fit(loaded.models$model[[1]])

train.predicitions = train %>% 
  group_by(clean.minute) %>% 
  nest() %>% 
  left_join(loaded.models) %>% 
  mutate(
    predictions = map2(
      data,
      model,
      ~.x %>% 
        tidypredict_to_column(.y)
    )
  ) %>% 
  select(clean.minute, predictions) %>% 
  unnest(c(predictions)) %>% 
  arrange(season, matchid, home, clean.minute)

error.by.minute = train.predicitions %>% 
  group_by(clean.minute) %>% 
  summarise(
    error = mean((fit - goals.left)^2)
  )

error.by.minute %>% 
  ggplot(aes(clean.minute, error)) +
  geom_line()

# summary(models$model[[1]])
# 
# butcher::weigh(models$model[[1]])
# 
# models$model[[1]] %>% 
#   butcher::axe_env() %>% 
#   butcher::axe_data() %>%
#   # butcher::
#   butcher::weigh()
#   summarise(sum(size)) %>%
#   identity()
#   write_rds('model/files/minute-models/tmp-trim.rds')
#   # view()
# 
# format(object.size(models$model[[4]]), units = 'MB')
# 
# parse_model(models$model[[91]])
# 
# write_rds(models$model[[4]], 'model/files/minute-models/tmp-c.rds', compress = 'gz')
# beepr::beep()

# models$model[[30]] %>% write_rds('model/files/minute-models/tmp.rds', compress = 'bz')
# beepr::beep()
#   pwalk(~write_rds(..2, glue::glue('model/files/minute-models/{..1}.rds'), compress = 'gz'))

# models %>% write_rds('model/files/models.rds', compress = 'bz')

# calcs = predictors %>%
#   group_by(clean.minute) %>%
#   nest() %>%
#   left_join(
#     models %>% select(clean.minute, model)
#   ) %>%
#   mutate(
#     preds = map2(
#       data,
#       model,
#       ~.x %>%
#         mutate(pred.goals.left = predict(.y, newdata = ., type = 'response'))
#     )
#   ) %>%
#   select(-data, -model) %>%
#   unnest(c(preds)) %>%
#   select(season, matchid, event.order, clean.minute, home, pred.goals.left, goals) %>%
#   arrange(season, matchid, event.order) %>%
#   mutate(home = if_else(home == 1, 'h', 'a')) %>%
#   pivot_wider(names_from = home, values_from = c(pred.goals.left, goals), names_sep = '.') %>%
#   mutate(
#     dist.h = map(pred.goals.left.h, ~dpois(0:10, .x)),
#     dist.a = map(pred.goals.left.a, ~dpois(0:10, .x)),
#     dist.h = map2(dist.h, goals.h, ~c(rep(0, .y), .x)),
#     dist.a = map2(dist.a, goals.a, ~c(rep(0, .y), .x)),
# 
#     matrix = map2(dist.h, dist.a, ~.x %o% .y),
# 
#     probtie = map_dbl(matrix, ~sum(diag(.x))),
#     prob.h = map_dbl(matrix, ~sum(.x[lower.tri(.x)])),
#     prob.a = map_dbl(matrix, ~sum(.x[upper.tri(.x)])),
#   )
# 
# calcs
# # 
# # calcs %>%
# #   select(-dist.h, -dist.a, -matrix) %>% 
# #   write_csv('eng/probabilities.csv')
# # 
# # errors = summaries %>% 
# #   right_join(calcs) %>% 
# #   select(-dist.h, -dist.a, -matrix) %>% 
# #   mutate(
# #     ist = goals.h == goals.a,
# #     ish = goals.h > goals.a,
# #     isa = goals.h < goals.a
# #   ) %>% 
# #   group_by(clean.minute) %>% 
# #   summarise(
# #     errt = mean((probtie - ist)^2),
# #     errh = mean((prob.h - ish)^2),
# #     erra = mean((prob.a - isa)^2),
# #   )
# # 
# # errors
# # 
# # errors %>% 
# #   pivot_longer(-clean.minute, names_to = 'err', values_to = 'prob') %>% 
# #   ggplot() +
# #   geom_line(aes(clean.minute, prob, color = err)) +
# #   scale_y_continuous(limits = c(0, 0.75))
# # 
# # summaries %>% 
# #   select(season, matchid, hteam, ateam, hscore, ascore) %>% 
# #   right_join(
# #     calcs %>% 
# #       group_by(season, matchid, clean.minute) %>% 
# #       filter(event.order == max(event.order)) %>% 
# #       ungroup() %>% 
# #       select(season, matchid, clean.minute, tie = probtie, h = prob.h, a = prob.a)
# #   ) %>%
# #   group_by(season, matchid, hteam, ateam, hscore, ascore) %>% 
# #   nest() %>% 
# #   ungroup() %>% 
# #   # sample_n(1) %>% 
# #   filter(matchid == '33b8c50a') %>% 
# #   mutate(
# #     plot = pmap(
# #       list(data, hteam, ateam, hscore, ascore),
# #       ~..1 %>%
# #         pivot_longer(-clean.minute, names_to = 'side', values_to = 'prob') %>% 
# #         mutate(side = fct_relevel(side, 'h', 'tie', 'a')) %>% 
# #         ggplot(aes(x = clean.minute, y = prob, fill = side, color = side)) +
# #         # geom_line() +
# #         geom_area() +
# #         labs(
# #           title = glue::glue('{..2} {..4}-{..5} {..3}')
# #         ) +
# #         theme_minimal()
# #     )
# #   ) %>% 
# #   pull(plot)
# # 
