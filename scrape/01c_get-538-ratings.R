ratings = readr::read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv')

readr::write_csv(ratings, 'scrape/matches538.csv', na = '')
