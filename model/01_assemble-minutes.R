library(tidyverse)

summary = read_csv('scrape/parse/parsing-summary.csv')

summary

fbref.events = read_csv('scrape/parse/fbref-events.csv')

fbref.events

espn.events = read_csv('scrape/parse/espn-events.csv')

espn.events

events = fbref.events %>% 
  transmute(
    matchid, player,
    eventtype = case_when(
      str_detect(eventtype, 'goal') ~ 'goal',
      str_detect(eventtype, 'red') ~ 'red',
    ),
    minute, team,
    scrape.source = 'fbref'
  ) %>% 
  bind_rows(
    espn.events %>% 
      transmute(
        matchid = as.character(matchid),
        player,
        eventtype = case_when(
          str_detect(eventtype, 'goal') ~ 'goal',
          str_detect(eventtype, 'red') ~ 'red',
        ),
        minute, team,
        scrape.source = 'espn'
      )
  )

events

minutes = summary %>% 
  left_join(events) %>% 
  separate(minute, into = c('clean.minute', 'stoppage.minute'), sep = '\\+', fill = 'right', remove = FALSE) %>%
  mutate(
    clean.minute = replace_na(as.numeric(clean.minute), 0),
    stoppage.minute = as.numeric(stoppage.minute),
    stoppage.minute = replace_na(stoppage.minute, 0)
  ) %>% 
  group_by(season, date, league_id, league, team.h, team.a, spi.h, spi.a, matchid, score.h, score.a, scrape.source) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    minutes = map(
      data,
      ~.x %>%
        right_join(tibble(clean.minute = 0:90), by = 'clean.minute') %>%
        arrange(clean.minute, stoppage.minute) %>%
        mutate(
          event.order = 1:nrow(.),
          eventtype = replace_na(eventtype, ''),
          team = replace_na(team, ''),
          goal.h = str_detect(eventtype, 'goal') & team == 'h',
          goal.a = str_detect(eventtype, 'goal') & team == 'a',
          goal.h = replace_na(goal.h, FALSE),
          goal.a = replace_na(goal.a, FALSE),
          goals.h = cumsum(goal.h),
          goals.a = cumsum(goal.a),
          red.h = str_detect(eventtype, 'red') & team == 'h',
          red.a = str_detect(eventtype, 'red') & team == 'a',
          men.h = 11 - cumsum(red.h),
          men.a = 11 - cumsum(red.a),
        )
    )
  )

minutes

minutes %>% write_rds('model/files/minutes.rds', compress = 'gz')
