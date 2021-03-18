library(tidyverse)
library(tidypredict)
library(glue)
library(yaml)

predictors = read_rds('model/files/predictors.rds')

predictors

predictors %>% count(season)

train = predictors %>% filter(season < 2019)

train %>% write_rds('model/files/training.rds', compress = 'gz')

n_distinct(train$matchid)

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
  select(model, clean.minute) %>%
  pwalk(
    ~..1 %>% 
      parse_model() %>% 
      write_yaml(glue('model/files/minute-models/minute-{..2}.yaml'))
  )
