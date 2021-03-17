library(tidyverse)

minutes = read_rds('model/files/minutes.rds') %>% 
  select(season, matchid, spi.h, spi.a, score.h, score.a, minutes) %>% 
  unnest(c(minutes))

minutes

predictors = bind_rows(
  minutes %>% 
    transmute(
      season, matchid,
      goals.left = score.h - goals.h,
      event.order, clean.minute,
      home = 1,
      spi.edge = spi.h - spi.a,
      goals = goals.h,
      goals.edge = goals.h - goals.a,
      men.edge = men.h - men.a,
    ),
  minutes %>% 
    transmute(
      season, matchid,
      goals.left = score.a - goals.a,
      event.order, clean.minute,
      home = 0,
      spi.edge = spi.a - spi.h,
      goals = goals.a,
      goals.edge = goals.a - goals.h,
      men.edge = men.a - men.h,
    )
)

predictors

predictors %>% write_rds('model/files/predictors.rds', compress = 'gz')
