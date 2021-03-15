library(tidyverse)
library(rvest)
library(glue)
library(lubridate)

source('scrape/utils.R')

date = str_remove_all(today(), '-')
# time = str_replace_all(now(), '[-\\s:]', '_')

url = glue('https://www.espn.com/soccer/fixtures/_/date/{date}')
page = read_html(url)
page %>% write_html(glue('scrape/espn/sched/{date}.html'))

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
  links = map2(
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
          teamh = str_sub(match, end = -10),
          teama = str_sub(...2, end = -5),
          date = today(),
          score
        ) %>% 
        select(teamh, teama, date, score) %>% 
        separate(score, sep = ' - ', into = c('scoreh', 'scorea'))
    )
  ) %>% 
  select(-class)

parsed

links = parsed %>%
  unnest(c(table, links)) %>% 
  ungroup() %>% 
  mutate(
    links = map_chr(links, ~str_c('https://espn.com', .x))
  )

links

walk(
  links$links,
  getorretrieve.espn
)
