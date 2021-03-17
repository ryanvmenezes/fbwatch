library(tidyverse)
library(rvest)
library(glue)
library(lubridate)

source('scrape/utils.R')

# date = str_remove_all(today(), '-')
date = '2021-03-15' # do removal
# url = glue('https://www.espn.com/soccer/fixtures/_/date/{date}')
# page = read_html(url)
# page %>% write_html(glue('scrape/espn/sched/{date}.html'))
page = read_html('scrape/espn/sched/20210315.html')

nodes = page %>%
  html_nodes('#sched-container > *')

nodes

parsed = tibble(
  class = map_chr(
    nodes,
    ~.x %>% 
      html_attr('class')
  ),
  league = map2_chr(
    nodes,
    class,
    ~{
      league = NA
      if(.y == 'table-caption') {
        league = .x %>%
          html_text()
      }
      league
    }
  ),
  table = map2(
    nodes,
    class,
    ~{
      table = NULL
      if(.y == 'responsive-table-wrap') {
        table = .x %>%
          html_node('table') %>% 
          html_table()
      }
      table
    }
  ),
  matchurl = map2(
    nodes,
    class,
    ~{
      links = NULL
      if (.y == 'responsive-table-wrap') {
        links = .x %>% 
          html_nodes('table span.record a') %>% 
          html_attr('href')
      }
      links
    }
  )
) %>% 
  fill(league, .direction = 'down') %>%
  filter(map_lgl(table, ~!is.null(.x))) %>% 
  group_by(league) %>% 
  filter(row_number() == max(row_number())) %>%
  semi_join(leagues, leagues, by = c('league' = 'name')) %>% 
  # check if this table has the header "result"
  filter(map_lgl(table, ~'result' %in% names(.x))) %>% 
  mutate(
    table = map(
      table,
      ~.x %>% 
        as_tibble(.name_repair = 'unique') %>% 
        mutate(
          score = str_sub(match, start = -5),
          team.h = str_sub(match, end = -10),
          team.a = str_sub(...2, end = -5),
          date = date,
          score
        ) %>% 
        select(team.h, team.a, date, score) %>% 
        separate(score, sep = ' - ', into = c('scoreh', 'scorea'))
    )
  ) %>% 
  select(-class)

parsed

links = leagues %>%
  transmute(country, league = name, season = current_szn) %>% 
  left_join(
    parsed %>%
      unnest(c(table, matchurl)) %>% 
      ungroup() %>% 
      mutate(
        matchurl = map_chr(matchurl, ~str_c('https://espn.com', .x)),
        fpath = map_chr(matchurl, make.path.espn)
      )  
  )

links

links %>% write_csv('scrape/parse/espn-urls.csv')

walk2(
  links$fpath,
  links$matchurl,
  get.new,
)
