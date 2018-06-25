library(httr)
library(jsonlite)

query <- GET('http://www.ttss.krakow.pl/internetservice/geoserviceDispatcher/services/stopinfo/stops?left=-648000000&bottom=-324000000&right=648000000&top=324000000')
stop_for_status(query)

stop_names <- fromJSON(content(query, 'text'))
stop_names <- stop_names[[1]]

stop_names <- stop_names[c('name', 'shortName')]
stop_names$shortName <- as.numeric(stop_names$shortName)

saveRDS(stop_names, 'stop-name-db.RDS')
