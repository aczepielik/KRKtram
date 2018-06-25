library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

stop_report <- function(stop_id){
  tstamp <- Sys.time()
  
  resp <- GET('http://www.ttss.krakow.pl/internetservice/services/passageInfo/stopPassages/stop',
      query = list(stop = stop_id)) %>% try()
  
  stop_for_status(resp, "Query unsucessful")
  
  resp <- fromJSON(content(resp, 'text')) %>% `$`(actual)
  resp$time_stamp <- tstamp
  resp$stop <- stop_id
  
  resp <- as.data.frame(resp)
  
  desirable_names <- c('time_stamp', 'stop', 'patternText', 'direction', 'plannedTime', 'tripId')
  for(i in seq_along(desirable_names)){
    if(desirable_names[i] %in% names(resp) == FALSE){
        resp[, desirable_names[i]]<- NA
  }}
  
  resp <- select(resp, time_stamp, stop, patternText, direction, plannedTime, status)
  names(resp)[3] <- 'number'

  resp$plannedTime <- parse_date_time(
    paste(year(resp$time_stamp), month(resp$time_stamp), day(resp$time_stamp), resp$plannedTime),
    orders = 'ymd HM', tz = 'Europe/Warsaw')

  filter(resp, difftime(floor_date(time_stamp, 'minute'), plannedTime) >= 0)
}

all_stops <- function(stops){
  city <- lapply(stops, stop_report)
  do.call(rbind, city)
}

periodic_report <- function(end){
  i <- 1
  report <- all_stops(stop_names$shortName)
  report <- cbind(index = i, report)
  
  while(Sys.time() < end){
    i <- i + 1
    report <- all_stops(stop_names$shortName) %>% cbind(index = i, .) %>% rbind(report, .)
    report <- report[sapply(seq(1, dim(report)[1]), function(i) to_save(report[i, ], report)), ]
  }
  
  report$delay <- difftime(floor_date(report$time_stamp, 'minute'), report$plannedTime, units = 'min')
  report$stop <- sapply(report$stop, function(x) stop_names[stop_names$shortName == x, 1])
  
}
  