library(tidyverse)
library(rvest)
library(glue)
library(progressr)

source('scrape/utils.R')

seasons = leagues %>%
  mutate(
    rawhtml = map2(country, fbref, getorretrieve.fbref),
    table = map(
      rawhtml,
      ~.x %>%
        html_node('table.stats_table') %>% 
        html_table()
    ),
    seasonurl = map(
      rawhtml,
      ~.x %>%
        html_nodes('th[data-stat="season"][scope="row"]') %>%
        html_nodes('a') %>%
        html_attr('href') %>%
        str_c('https://fbref.com', .)
    )
  ) %>% 
  select(-code538, -fbref, -rawhtml) %>% 
  unnest(c(table, seasonurl)) %>% 
  filter(Season >= firstseason)

seasons

seasonshtml = seasons %>%
  mutate(
    iscurrentszn = Season == current_szn,
  ) %>% 
  transmute(
    country, name,
    season = Season,
    seasonurl = str_replace(seasonurl, '\\/[a-zA-Z0-9-]*$', '/schedule\\0'),
    seasonurl = str_replace(seasonurl, '-Stats', '-Scores-and-Fixtures'),
    rawhtml = pmap(
      list(country, seasonurl, iscurrentszn),
      ~getorretrieve.fbref(..1, ..2, override = ..3)
    ),
  )

seasonshtml

fixtures = seasonshtml %>% 
  mutate(
    fixtures = map(
      rawhtml,
      ~.x %>% 
        html_node('table.stats_table') %>% 
        html_table() %>% 
        as_tibble(.name_repair = 'unique') %>% 
        select(Wk, Day, Date, Time, Home, Score, Away)
    ),
    matchurl = map(
      rawhtml,
      ~.x %>% 
        html_nodes('td[data-stat="match_report"]') %>% 
        html_node('a') %>% 
        html_attr('href')
    )
  ) %>% 
  select(-rawhtml, -seasonurl) %>% 
  unnest(c(fixtures, matchurl)) %>% 
  filter(!(Home == '' & Away == ''))

fixtures

fixtures %>% count()
fixtures %>% count(season)

played = fixtures %>% 
  filter(str_detect(matchurl, 'matches')) %>% 
  mutate(matchurl = map_chr(matchurl, ~str_c('https://fbref.com', .x)))

played

played %>%  count(season)

played %>% write_csv('scrape/fbref-urls.csv')

with_progress({
  p = progressor(along = played$matchurl)
  
  walk2(
    played$country,
    played$matchurl,
    ~{
      getorretrieve.fbref(.x, .y)
      p()
    }
  )
})

beepr::beep()
