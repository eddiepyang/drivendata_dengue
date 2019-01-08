key <- jsonlite::read_json('~/projects/credentials/shinyapps.json')
rsconnect::setAccountInfo(name=key$name,
                          token=key$token,
                          secret=key$secret)

rsconnect::deployApp('~/projects/investing/src/shiny/dashboard.Rmd')

