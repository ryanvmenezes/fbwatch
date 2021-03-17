library(tidyverse)
library(rvest)

# matches with fbref urls that have been downloaded

matches = read_csv('scrape/fbref-urls.csv')

matches

# previously parsed events

parsed = read_csv('scrape/parse/fbref-parsing-summary.csv')

parsed

matchhtml = matches %>% 
  mutate(matchid = str_sub(matchurl, start = 30, end = 37)) %>% 
  anti_join(parsed) %>% 
  mutate(
    rawhtml = map2(
      matchurl,
      country,
      ~.x %>% 
        str_split('/') %>% 
        `[[`(1) %>% 
        `[`(length(.)) %>% 
        str_c('scrape/', .y, '/html/', ., '.html') %>% 
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
    matchid = str_sub(matchurl, start = 30, end = 37),
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
    events = map(
      rawhtml,
      getevents
    )
  )

parsed

beepr::beep()

if (nrow(parsed) > 0) {
  old.summary = read_csv('scrape/parse/fbref-parsing-summary.csv', col_types = c(.default = 'c'))
  
  old.summary
  
  summary = parsed %>%
    select(-events) %>% 
    bind_rows(old.summary) %>% 
    arrange(desc(date), desc(time))
  
  summary
  
  summary %>% write_csv('scrape/parse/fbref-parsing-summary.csv', na = '')
  
  old.events = read_csv('scrape/parse/fbref-events.csv', col_types = c(.default = 'c'))
  
  old.events
  
  events = parsed %>% 
    select(season, matchid, events) %>% 
    unnest(c(events)) %>% 
    bind_rows(old.events)
  
  events
  
  events %>% write_csv('scrape/parse/fbref-events.csv')
}
