library(tidyverse)
library(rvest)

matches = read_csv('eng/match-urls.csv')

matches

matchhtml = matches %>% 
  filter(season >= '2016-2017') %>% 
  mutate(
    rawhtml = map(
      matchreporturl,
      ~.x %>% 
        str_split('/') %>% 
        `[[`(1) %>% 
        `[`(length(.)) %>% 
        str_c('eng/html/', ., '.html') %>% 
        read_html()
    )
  )

matchhtml  

beepr::beep()

parseevents = function(tm) {
  player = tm %>%
    html_node('a') %>%
    html_text()
  
  playerid = tm %>%
    html_node('a') %>%
    html_attr('href') %>%
    str_replace('/en/players/','') %>%
    str_sub(end = 8)
  
  eventtype = tm %>%
    html_node('div') %>%
    html_attr('class') %>%
    str_replace('event_icon ', '')
  
  minute = tm %>%
    html_text() %>%
    str_trim() %>%
    str_replace('&rsquor;', '') %>%
    str_split(' · ') %>%
    map_chr(`[`, 2)
  
  return(tibble(player, playerid, eventtype, minute))
}

getevents = function(h) {
  aevents = h %>% 
    html_node('div#a.event') %>% 
    html_children() %>% 
    parseevents() %>% 
    mutate(team = 'h')
  
  bevents = h %>% 
    html_node('div#b.event') %>% 
    html_children() %>% 
    parseevents() %>% 
    mutate(team = 'a')
  
  return(bind_rows(aevents, bevents))
}


parsed = matchhtml %>% 
  # sample_n(20) %>% 
  transmute(
    season,
    matchid = str_sub(matchreporturl, start = 30, end = 37),
    # rawhtml,
    hteam = map_chr(
      rawhtml,
      ~.x %>% 
        html_nodes('div[itemprop="performer"]') %>% 
        `[`(1) %>% 
        html_node('a[itemprop="name"]') %>% 
        html_text()
    ),
    ateam = map_chr(
      rawhtml,
      ~.x %>% 
        html_nodes('div[itemprop="performer"]') %>% 
        `[`(2) %>% 
        html_node('a[itemprop="name"]') %>% 
        html_text()
    ),
    hscore = map_chr(
      rawhtml,
      ~.x %>% 
        html_nodes('div.score') %>% 
        `[`(1) %>% 
        html_text()
    ),
    ascore = map_chr(
      rawhtml,
      ~.x %>% 
        html_nodes('div.score') %>% 
        `[`(2) %>% 
        html_text()
    ),
    date = map_chr(
      rawhtml,
      ~.x %>% 
        html_node('span.venuetime') %>% 
        html_attr('data-venue-date')
    ),
    time = map_chr(
      rawhtml,
      ~.x %>% 
        html_node('span.venuetime') %>% 
        html_attr('data-venue-time')
    ),
    events = map(
      rawhtml,
      getevents
    )
  )

parsed

beepr::beep()

parsed %>%
  select(-events) %>% 
  write_csv('eng/summaries.csv')

parsed %>% 
  select(season, matchid, events) %>% 
  unnest(c(events)) %>% 
  write_csv('eng/events.csv')

parsed
