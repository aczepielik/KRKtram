library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

to_save <- function(row, df){
  n <- nrow(df[df$index == row$index + 1 &
                 df$number == row$number &
                 df$direction == row$direction &
                 df$plannedTime == row$plannedTime
               , ])
  if(n > 0){
    FALSE} else{
      TRUE}
}

stop_report <- function(stop_id){
  tstamp <- Sys.time()
  
  resp <- GET('http://www.ttss.krakow.pl/internetservice/services/passageInfo/stopPassages/stop',
      query = list(stop = stop_id)) %>% try()
  
  stop_for_status(resp, "Query unsucessful")
  
  resp <- fromJSON(content(resp, 'text')) %>% `$`(actual)
  resp$time_stamp <- tstamp
  resp$stop <- stop_id
  
  resp <- as.data.frame(resp)
  
  desirable_names <- c('time_stamp', 'stop', 'patternText', 'direction', 'plannedTime', 'vehicleId', 'trackId')
  for(i in seq_along(desirable_names)){
    if(desirable_names[i] %in% names(resp) == FALSE){
        resp[, desirable_names[i]]<- NA
  }}
  
  resp <- select(resp, time_stamp, stop, patternText, direction, plannedTime, vehicleId, trackId)
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
    # if(dim(report)[1] > 15000){
    #   filter(report, index < i - 15) %>% write.csv('report.csv', append = TRUE)
    #   report <- filter(report, index >= i - 15)
    # }
    i <- i + 1
    report <- all_stops(stop_names$shortName) %>% cbind(index = i, .) %>% rbind(report, .)
    report <- report[sapply(seq(1, dim(report)[1]), function(i) to_save(report[i, ], report)), ]
  }
  # report %>% write.csv('report.csv', append = TRUE)
  
  # report <- read.csv('report.csv')
  report$delay <- difftime(floor_date(report$time_stamp, 'minute'), report$plannedTime, units = 'min')
  report$stopName <- sapply(report$stop, function(x) stop_names[stop_names$shortName == x, 1])
  
  report <- report[c('time_stamp', 'stop', 'stopName', 'number', 'direction', 'plannedTime', 'vehicleId', 'trackId', 'delay')]
  report %>% write.csv('report.csv', row.names = FALSE, fileEncoding = "UTF-8")
}
  