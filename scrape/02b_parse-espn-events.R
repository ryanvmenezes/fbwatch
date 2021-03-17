library(tidyverse)
library(rvest)
library(glue)

source('scrape/utils.R')

# matches from espn 

matches = read_csv('scrape/parse/espn-urls.csv')

matches

# previously parsed events by fbref

parsed = read_csv('scrape/parse/fbref-parsing-summary.csv')

parsed

parse.info = function(html, data.type, side) {
  html %>% 
    html_nodes(glue('.team-info.players[data-home-away="{side}"]')) %>% 
    html_nodes(glue('ul[data-event-type="{data.type}"] li')) %>% 
    html_text() %>% 
    str_squish() %>% 
    tibble(goals = .) %>% 
    separate(goals, into = c('goals', 'minute'), sep = ' \\(') %>% 
    mutate(
      minute = str_remove(minute, '\\)'),
      minute = str_remove(minute, ' PEN')
    ) %>% 
    separate_rows(minute, sep = ', ') %>% 
    mutate(minute = str_remove_all(minute, "'")) %>% 
    transmute(
      player = goals,
      playerid = NA_character_,
      eventtype = str_to_lower(data.type),
      minute,
      team = str_sub(side, end = 1)
    )
}

get.events = function(h) {
  bind_rows(
    parse.info(h, 'goal', 'home'),
    parse.info(h, 'goal', 'away'),
    parse.info(h, 'redCard', 'home'),
    parse.info(h, 'redCard', 'away')
  )
}

matchhtml = matches %>% 
  # has this already been parsed as an fbref game? 
  anti_join(parsed, by = c('team.h' = 'hteam', 'team.a' = 'ateam', 'date')) %>% 
  mutate(
    matchid = str_sub(matchurl, start = 39),
    rawhtml = map(fpath, read_html),
    events = map(rawhtml, get.events)
  )

matchhtml

summary = matchhtml %>% 
  select(season, matchid, team.h, team.a, scoreh, scorea, date)

summary

summary %>% write_csv('scrape/parse/espn-parsing-summary.csv')

events = matchhtml %>% 
  select(season, matchid, events) %>% 
  unnest(c(events))

events

events %>% write_csv('scrape/parse/espn-events.csv')

