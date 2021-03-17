library(tidyverse)

source('scrape/utils.R')

fbref.games = read_csv('scrape/parse/fbref-parsing-summary.csv')

fbref.games

espn.games = read_csv('scrape/parse/espn-parsing-summary.csv')

espn.games

ratings538 = read_csv('scrape/matches538.csv')

summary = ratings538 %>% 
  semi_join(leagues, by = c('league_id' = 'code538')) %>% 
  filter(!is.na(score1) & !is.na(score2)) %>%
  select(season, date, league_id, league, team.h = team1, team.a = team2, spi.h = spi1, spi.a = spi2) %>% 
  mutate(
    team.h = edit.teams(team.h),
    team.a = edit.teams(team.a)
  ) %>% 
  left_join(
    fbref.games %>% 
      select(season, matchid, team.h = hteam, team.a = ateam, score.h = hscore, score.a = ascore, date) %>%  
      mutate(scrape.source = 'fbref') %>% 
      bind_rows(
        espn.games %>% 
          select(season, matchid, team.h, team.a, score.h = scoreh, score.a = scorea, date) %>% 
          mutate(
            matchid = as.character(matchid),
            scrape.source = 'espn',
          )
      ) %>% 
      mutate(season = as.numeric(str_sub(season, start = -4)) - 1) %>% 
      arrange(desc(date))
  )

summary

summary %>% write_csv('scrape/parse/parsing-summary.csv')
