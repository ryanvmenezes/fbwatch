library(tidyverse)

events = read_csv('eng/events.csv')

events

events %>% count(eventtype)

summaries = read_csv('eng/summaries.csv')

summaries

ratings = read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv')

ratings %>% write_csv('matches538.csv', na = '')

ratings

minute.matrix = events %>%
  separate(minute, into = c('clean.minute', 'stoppage.minute'), sep = '\\+', fill = 'right', remove = FALSE) %>% 
  mutate(
    clean.minute = as.numeric(clean.minute),
    stoppage.minute = as.numeric(stoppage.minute),
    stoppage.minute = replace_na(stoppage.minute, 0)
  ) %>% 
  group_by(season, matchid) %>% 
  nest() %>% 
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
          hgoal = str_detect(eventtype, 'goal') & team == 'h',
          agoal = str_detect(eventtype, 'goal') & team == 'a',
          hgoals = cumsum(hgoal),
          agoals = cumsum(agoal),
          hred = str_detect(eventtype, 'red') & team == 'h',
          ared = str_detect(eventtype, 'red') & team == 'a',
          hmen = 11 - cumsum(hred),
          amen = 11 - cumsum(ared),
        )
    )
  ) %>% 
  select(-data) %>% 
  unnest(c(minutes)) %>% 
  ungroup()
  
beepr::beep()

minute.matrix

# matrix %>% 
#   count(matchid) %>% 
#   filter(n > 91) %>% 
#   left_join(matrix) %>% 
#   drop_na(minute)

# matrix %>% count(matchid) %>% 
#   filter(n > 90) %>% 
#   arrange(-n)
# 
# events %>% count(minute) %>% view()

edit.teams = function(team.name) {
  case_when(
    team.name == 'Wolverhampton' ~ 'Wolverhampton Wanderers',
    team.name == 'Newcastle' ~ 'Newcastle United',
    team.name == 'Brighton and Hove Albion' ~ 'Brighton & Hove Albion',
    team.name == 'AFC Bournemouth' ~ 'Bournemouth',
    TRUE ~ team.name
  )
}

full.matrix = summaries %>% 
  left_join(
    ratings %>% 
      filter(league_id == 2411) %>% 
      select(date, hteam = team1, ateam = team2, hspi = spi1, aspi = spi2) %>% 
      mutate(
        hteam = edit.teams(hteam),
        ateam = edit.teams(ateam),
      )
  ) %>% 
  left_join(minute.matrix) %>%
  select(-hteam, -ateam)

full.matrix

full.matrix %>% write_csv('eng/matrix.csv')
