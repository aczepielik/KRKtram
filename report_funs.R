suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(readr))


stop_report <- function(stop_id){
  tstamp <- Sys.time()
  
  resp <- tryCatch(GET('http://www.ttss.krakow.pl/internetservice/services/passageInfo/stopPassages/stop',
      query = list(stop = stop_id), timeout(15)),
      error = function(err){ #To handle connection errors
        message(paste('Problem occured at', tstamp))
        message(err)
        return(404L)
      })
  
  if(http_error(resp)){
    return(data.frame(time_stamp = tstamp,
               stop = NA, number = NA, direction = NA,
               plannedTime = NA, vehicleId = NA, tripId = NA, status = NA))
  }
    
  resp <- fromJSON(content(resp, 'text')) %>% `$`(actual)
  resp$time_stamp <- tstamp
  resp$stop <- stop_id
  
  resp <- as.data.frame(resp)
  
  desirable_names <- c('time_stamp', 'stop', 'patternText', 'direction', 'plannedTime', 'vehicleId', 'tripId', 'status')
  for(i in seq_along(desirable_names)){# queries on some stops don't generate all the features
    if(desirable_names[i] %in% names(resp) == FALSE){
        resp[, desirable_names[i]] <- NA
  }}
  
  resp <- select(resp, time_stamp, stop, patternText, direction, plannedTime, vehicleId, tripId, status)
  names(resp)[3] <- 'number'

  resp$plannedTime <- fast_strptime(
    paste(year(resp$time_stamp), month(resp$time_stamp), day(resp$time_stamp), resp$plannedTime),
    format = '%Y %m %d %H:%M', tz = 'Europe/Warsaw') %>% as.POSIXct()
  
  #As dates are parsed from timestamps it may happen, that todays date from seconds before midnight,
  #was parsed to plannedTime from tommorow
  if(hour(tstamp) == 23){
  resp$plannedTime[hour(resp$plannedTime) == 0] <- resp$plannedTime[hour(resp$plannedTime) == 0] + days(1)
  }

  filter(resp, difftime(floor_date(time_stamp, 'minute'), plannedTime) >= -2)
}

many_stops <- function(stops){ #takes about 20 sec
  city <- lapply(stops, stop_report)
  do.call(rbind, city)
  Sys.sleep(2)
}

to_save <- function(row, df){
  df %>% 
    filter(index > as.numeric(row[1]) & # Check for duplicated record with bigger index
             stop == as.numeric(row[3]) & 
             number == as.numeric(row[4]) &
             direction == as.character(row[5]) & 
             tripId == as.character(row[8])) %>% 
    nrow() %>% `-`(1, .) %>% as.logical() #returns TRUE iff there are no duplicated records
}

report_completion <- function(report){
  report$delay <- difftime(floor_date(report$time_stamp, 'minute'), report$plannedTime, units = 'min')
  report$delay[report$delay < 0] <- 0
  report$stopName <- sapply(report$stop, function(x) stop_names[stop_names$shortName == x, 1])
  
  report %>% 
    select(index, time_stamp, stop, stopName, number, direction, plannedTime, vehicleId, tripId, status, delay) %>%
    left_join(linestops, by = c('number', 'direction', 'stopName' = 'name'))
}

periodic_report <- function(end, stops = stop_names$shortName){
  #Every 15 minutes report will be cleared and saved to csv
  saving_times <- seq(Sys.time(), end, by = '15 min') 
  
  save_index <- 2 #to iterate over saving times
  i <- 1 #to iterate over queries
  first_success <- FALSE
  first_dump <- TRUE
  
  while(first_success == FALSE & Sys.time() < end){
  report <- many_stops(stops)

  if(nrow(report) > 0){ #report could be empty, it happens ussually at early morning and late evening
    report <-  cbind(index = i, report) # index can only be added to non-empty report
    first_success <- TRUE
  }}
  
  stopifnot(first_success) #if all execution of above loop were ineffective
  
  while(Sys.time() < end){
    i <- i + 1
    report_tmp <- many_stops(stops)
    
    if(nrow(report_tmp) == 0){ #if report is empty we look forward to the next report unless timeout is reached
      if(Sys.time() < end){
        i <- i - 1
        next()
        } else {
        break()
        }
    }
    report <- report_tmp %>% cbind(index = i, .) %>% bind_rows(report, .)
    
    if(Sys.time() > saving_times[save_index]){# Cleraing report variable and writing it to external file
      report <- report[apply(report, 1, to_save, report), ]
      write.table(report, 'report_tmp.csv', sep = ',', row.names = FALSE, 
                  col.names = first_dump, fileEncoding = 'UTF-8', append = !first_dump)
      
      report <- report_tmp %>% cbind(index = i, .)
      save_index <- save_index + 1
      first_dump <- FALSE
      }
  }
  # saving what hasn't been saved yet
  write.table(report, 'report_tmp.csv', sep = ',', row.names = FALSE, 
              col.names = first_dump, fileEncoding = 'UTF-8', append = !first_dump)
  
  report_final <- read_csv('report_tmp.csv', col_types = cols(
    index = col_integer(),
    time_stamp = col_datetime(format = ""),
    stop = col_integer(),
    number = col_integer(),
    direction = col_character(),
    plannedTime = col_datetime(format = ""),
    vehicleId = col_character(),
    tripId = col_character(),
    status = col_character()
  )) %>% unique()
  
  report_final <- report_final[apply(report_final, 1, to_save, report_final), ] #Final clering
  
  report_completion(report_final) %>% 
  write.csv(report, 'report.csv', row.names = FALSE, fileEncoding = 'UTF-8')
  
}

line_periodic_report  <- function(end, line){
  stops_extr <- filter(linestops, number == line) %>% select(name) %>% unique() %>%
    left_join(stop_names, by = 'name') %>% select(shortName) %>% `[[`(1)
  
  i <- 1
  first_success <- FALSE
  
  while(first_success == FALSE & Sys.time() < end){
    report <- many_stops(stops)
    
    if(dim(report)[1] > 0){ #report could be empty, it happens at early morning and late evening
      report <-  cbind(index = i, report) # index can only be added to non-empty report
      first_success <- TRUE
    }}
  
  stopifnot(first_success) #if all execution of above loop were ineffective
  
  while(Sys.time() < end){
    i <- i + 1
    report_tmp <- many_stops(stops)
    
    if(dim(report_tmp)[1] == 0){ #if report is empty we look forward to the next report unless timeout is reached
      if(Sys.time() < end){
        i <- i - 1
        next()
      } else {
        break()
      }
    }
    
    report <- report_tmp %>% cbind(index = i, .) %>% rbind(report, .)
    report <- report[apply(report, 1, to_save, report), ]
    write.csv(report, 'report_tmp.csv', row.names = FALSE, fileEncoding = "UTF-8")
  }
  report$delay <- difftime(floor_date(report$time_stamp, 'minute'), report$plannedTime, units = 'min')
  report$stopName <- sapply(report$stop, function(x) stop_names[stop_names$shortName == x, 1])
  
  report %>% 
    select(index, time_stamp, stop, stopName, number, direction, plannedTime, vehicleId, tripId, status, delay) %>%
    left_join(linestops, by = c('number', 'direction' = 'directions', 'stopName' = 'name')) %>%
    write.csv('report.csv', row.names = FALSE, fileEncoding = "UTF-8")
}
