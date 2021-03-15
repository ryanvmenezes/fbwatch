current_szn = '2020-2021'

getorretrieve = function(fpath, url, override = FALSE) {
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  return (h)
}

getorretrieve.fbref = function(country, url, override = FALSE) {
  fname = url %>% 
    str_split('/') %>% 
    `[[`(1) %>% 
    `[`(length(.)) %>% 
    str_c('.html')
  
  fpath = glue('scrape/{country}/html/{fname}')
  
  return (getorretrieve(fpath, url, override))
}

getorretrieve.espn = function(url, override = FALSE) {
  fname = str_sub(url, start = -6) %>% 
    str_c('.html')
  
  fpath = glue('scrape/espn/matches/{fname}')
  
  return (getorretrieve(fpath, url, override))
}

leagues = read_csv('scrape/leagues.csv')

leagues
