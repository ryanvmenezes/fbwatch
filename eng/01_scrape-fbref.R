library(tidyverse)
library(rvest)
library(glue)
library(progressr)

getorretrieve = function(url, override = FALSE) {
  fname = url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`(length(.)) %>% 
    str_c('.html')
  
  fpath = glue('eng/html/{fname}')
  
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  
  return(h)
}

indexpage = tibble(url = 'https://fbref.com/en/comps/9/history/Premier-League-Seasons')

indexpage

seasons = indexpage %>%
  mutate(
    rawhtml = map(url, getorretrieve),
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
  select(-url, -rawhtml) %>% 
  unnest(c(table, seasonurl))

seasons

seasonshtml = seasons %>% 
  transmute(
    season = Season,
    seasonurl = str_replace(seasonurl, '\\/[a-zA-Z0-9-]*$', '/schedule\\0'),
    seasonurl = str_replace(seasonurl, '-Stats', '-Scores-and-Fixtures'),
    rawhtml = map(seasonurl, getorretrieve),
  )

seasonshtml

fixtures = seasonshtml %>% 
  mutate(
    matchreporturl = map(
      rawhtml,
      ~.x %>% 
        html_nodes('td[data-stat="match_report"]') %>% 
        html_nodes('a') %>%
        html_attr('href') %>%
        str_c('https://fbref.com', .)
    )
  ) %>% 
  select(season, matchreporturl) %>% 
  unnest(c(matchreporturl)) %>%
  filter(str_detect(matchreporturl, 'matches'))
  
fixtures

fixtures %>% count()
fixtures %>% count(season)

fixtures %>% write_csv('eng/match-urls.csv')

urls = fixtures %>% 
  filter(season >= '2016-2017') %>%
  pull(matchreporturl)

with_progress({
  p = progressor(along = urls)
  
  walk(
    urls,
    ~{
      getorretrieve(.x)
      p()
    }
  )
})

beepr::beep()
