current_szn = '2020-2021'

get.or.retrieve = function(fpath, url, override = FALSE) {
  if (!override & file.exists(fpath)) {
    h = read_html(fpath)
  } else {
    h = read_html(url)
    write_html(h, fpath)
  }
  return (h)
}

get.new = function(fpath, url, override = FALSE) {
  if (!file.exists(fpath) | override) {
    h = read_html(url)
    write_html(h, fpath)
  }
}

make.path.fbref = function(country, url) {
  fname = url %>%
    str_split('/') %>%
    `[[`(1) %>%
    `[`(length(.)) %>%
    str_c('.html')

  glue('scrape/{country}/html/{fname}')
}

make.path.espn = function(url) {
  fname = str_sub(url, start = -6) %>%
    str_c('.html')

  glue('scrape/espn/matches/{fname}')
}

edit.teams = function(team.name) {
  # from 538 names to fbref names
  case_when(
    team.name == 'Wolverhampton' ~ 'Wolverhampton Wanderers',
    team.name == 'Newcastle' ~ 'Newcastle United',
    team.name == 'Brighton and Hove Albion' ~ 'Brighton & Hove Albion',
    team.name == 'AFC Bournemouth' ~ 'Bournemouth',

    team.name == 'Sevilla FC' ~ 'Sevilla',
    team.name == 'Athletic Bilbao' ~ 'Athletic Club',
    team.name == 'Leganes' ~ 'Leganés',
    team.name == 'Atletico Madrid' ~ 'Atlético Madrid',
    team.name == 'Girona FC' ~ 'Girona',
    team.name == 'Real Valladolid' ~ 'Valladolid',
    team.name == 'SD Huesca' ~ 'Huesca',
    team.name == 'Cadiz' ~ 'Cádiz',

    TRUE ~ team.name
  )
}

leagues = read_csv('scrape/leagues.csv')

leagues
