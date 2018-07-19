library(httr)
library(jsonlite)


# Locations of stops
query <- GET('http://www.ttss.krakow.pl/internetservice/geoserviceDispatcher/services/stopinfo/stops?left=-648000000&bottom=-324000000&right=648000000&top=324000000')
stop_for_status(query)

stop_names <- fromJSON(content(query, 'text'))
stop_names <- stop_names[[1]]

stop_names <- stop_names[c('name', 'shortName', 'latitude', 'longitude')]
stop_names$shortName <- as.numeric(stop_names$shortName)
stop_names$latitude <- stop_names$latitude/3600000
stop_names$longitude <- stop_names$longitude/3600000

saveRDS(stop_names, 'stop-name-db.RDS')

# Map between dtops and lines
library(rvest)

lines <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 13, 14, 16, 18, 19, 20, 21, 22, 24, 44, 50, 52)

linestops <- list()

url <- 'http://rozklady.mpk.krakow.pl/?lang=PL&akcja=index&rozklad=20180626&linia='

for(i in lines){
  numeral <- paste(i, 1, sep = '__')
  
  linestops[[numeral]] <-  GET(paste0(url,numeral)) %>% 
    content('text') %>% read_html() %>% 
    html_node( css = '.main table table table td:nth-child(1) tr:nth-child(2) td') %>% 
    html_node(xpath = 'table') %>% html_table(fill = TRUE) %>% `[`(1)
  
  numeral <- paste(i, 2, sep = '__')
  
  linestops[[numeral]] <-  GET(paste0(url,numeral)) %>% 
    content('text') %>% read_html() %>% 
    html_node( css = '.main table table table td:nth-child(1) tr:nth-child(2) td') %>% 
    html_node(xpath = 'table') %>% html_table(fill = TRUE) %>% `[`(1)
}

linestops <- lapply(linestops, unique)

for(i in seq_along(linestops)){
  linestops[[i]]$seq_num <- seq(dim(linestops[[i]])[1])
  linestops[[i]]$number <- rep(lines[ceiling(i/2)], dim(linestops[[i]])[1])
  linestops[[i]]$direction <- unlist(rep(tail(linestops[[i]][1], 1), dim(linestops[[i]])[1]))
  
  names(linestops[[i]]) <- c('name', 'seq_num', 'number', 'direction')
}

linestops <- do.call(rbind, linestops)

linestops$direction <- gsub('Wzgórza Krzesławickie', 'Wzgórza K.', linestops$direction)
linestops$direction <- gsub('Cmentarz Rakowicki', 'Cm. Rakowicki', linestops$direction)
linestops$direction <- gsub('Dworzec Towarowy', 'Dworzec Tow.', linestops$direction)

saveRDS(linestops, 'linestops.RDS')
