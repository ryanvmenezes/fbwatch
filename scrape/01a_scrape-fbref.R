library(tidyverse)
library(rvest)
library(glue)
library(progressr)

source('scrape/utils.R')

seasons = leagues %>%
  mutate(
    fpath = map2_chr(country, fbref, make.path.fbref),
    rawhtml = map2(country, fpath, get.or.retrieve),
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
    fpath = map2_chr(country, seasonurl, make.path.fbref),
    rawhtml = pmap(
      list(fpath, seasonurl, iscurrentszn),
      ~get.or.retrieve(..1, ..2, override = ..3)
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
  select(-fpath) %>% 
  filter(str_detect(matchurl, 'matches')) %>% 
  mutate(
    matchurl = map_chr(matchurl, ~str_c('https://fbref.com', .x)),
    fpath = map2_chr(country, matchurl, make.path.fbref)
  )

played

played %>%  count(season)

played %>% write_csv('scrape/parse/fbref-urls.csv')

with_progress({
  p = progressor(along = played$fpath)
  
  walk2(
    played$fpath,
    played$matchurl,
    ~{
      get.new(.x, .y)
      p()
    }
  )
})

beepr::beep()
