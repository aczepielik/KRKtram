library(httr)
library(jsonlite)


# Locations of stops
query <- GET('http://www.ttss.krakow.pl/internetservice/geoserviceDispatcher/services/stopinfo/stops?left=-648000000&bottom=-324000000&right=648000000&top=324000000')
stop_for_status(query)

stop_names <- fromJSON(content(query, 'text'))
stop_names <- stop_names[[1]]

stop_names <- stop_names[c('name', 'shortName', 'latitude', 'longitude')]
stop_names$shortName <- as.numeric(stop_names$shortName)
stop_names$latitude <- stop_names$latitude/3600000 #notation correction
stop_names$longitude <- stop_names$longitude/3600000

stop_names[stop_names$name == 'Podgórze SKA', ]$longitude <- 19.960779 #position correction
stop_names[stop_names$name == 'Podgórze SKA', ]$latitude <- 50.042421


saveRDS(stop_names, 'stop-name-db.RDS')

# Map between stops and lines
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
  linestops[[i]]$seq_num <- seq(nrow(linestops[[i]]))
  linestops[[i]]$number <- rep(lines[ceiling(i/2)], nrow(linestops[[i]]))
  linestops[[i]]$direction <- unlist(rep(tail(linestops[[i]][1], 1), nrow(linestops[[i]])))
  
  names(linestops[[i]]) <- c('name', 'seq_num', 'number', 'direction')
}

linestops <- do.call(rbind, linestops)

linestops$direction <- gsub('Wzgórza Krzesławickie', 'Wzgórza K.', linestops$direction)
linestops$direction <- gsub('Cmentarz Rakowicki', 'Cm. Rakowicki', linestops$direction)
linestops$direction <- gsub('Dworzec Towarowy', 'Dworzec Tow.', linestops$direction)

saveRDS(linestops, 'linestops.RDS')

# Routes

### Consists of records like:
# name | from | to | from.longitude | from.latitude | to.longitude | to.latitude | lines (number of)
# Agencja - Blokowa | Agencja | Blokowa | 20.07 | 50.08 | 20.07 | 50.08534 | 1

routes <- linestops %>% select(number, direction, name) %>% 
  left_join(stop_names, by = 'name') %>% select(-shortName) %>% 
  group_by(number, direction) %>% 
  rename(from = name, from.longitude = longitude, from.latitude = latitude) %>% 
  mutate(to = lead(from), to.longitude = lead(from.longitude), to.latitude = lead(from.latitude)) %>%
  mutate(name = paste(from, to, sep = " - ")) %>% ungroup() %>% 
  select(name, from, to, from.longitude, from.latitude, to.longitude, to.latitude) %>% 
  filter(!is.na(to)) %>% 
  group_by(name, from, to, from.longitude, from.latitude, to.longitude, to.latitude) %>% 
  summarise(lines = n())

saveRDS(routes, 'routes.RDS')

line_routes <- linestops %>% select(number, direction, name) %>% 
  left_join(stop_names, by = 'name') %>% select(-shortName) %>% 
  group_by(number, direction) %>% 
  rename(from = name, from.longitude = longitude, from.latitude = latitude) %>% 
  mutate(to = lead(from), to.longitude = lead(from.longitude), to.latitude = lead(from.latitude)) %>%
  mutate(name = paste(from, to, sep = " - ")) %>%
  select(name, from, to, from.longitude, from.latitude, to.longitude, to.latitude) %>% 
  filter(!is.na(to)) %>% ungroup()

saveRDS(line_routes, 'line-routes.RDS')

line_routes_directed <- linestops %>% select(number, direction, name) %>% 
  left_join(stop_names, by = 'name') %>% select(-shortName) %>% 
  group_by(number, direction) %>% 
  rename(by = name) %>% 
  mutate(from = lag(by), to = lead(by)) %>%
  mutate(name = paste(from, by, to, sep = " -> ")) %>%
  select(number, direction, from, by, to, name, longitude, latitude) %>% 
  ungroup()

saveRDS(line_routes_directed, 'line-routes-directed.RDS')

just_directions <- line_routes_directed %>% 
  select(from, by, to, name, longitude, latitude) %>% unique()
