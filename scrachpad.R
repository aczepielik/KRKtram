df1 <- data.frame(a = rnorm(5), b = rnorm(5))
df2 <- data.frame(a = rnorm(5), b = rnorm(5))
df3 <- data.frame(a = rnorm(5), b = rnorm(5))

cluster <- list(df1, df2, df3)

df.new <- data.frame(a = rnorm(5), b = rnorm(5))
lapply(cluster, function(x) rbind(x, df.new))

report <- read.csv('report.csv')
report <- left_join(report, linestops, by = c('number', 'direction' = 'directions', 'stopName' = 'name'))

report$vehicleId <- as.character(report$vehicleId) %>% substr(15, 19) #NO!!!!
report$tripId <- as.character(report$tripId) %>% substr(15, 19)

reportdf %>% write.csv('reportt2.csv', row.names = FALSE, fileEncoding = "UTF-8")

reportdf <- report[c('index', 'time_stamp', 'stop', 'number', 'direction', 'plannedTime', 'vehicleId', 'tripId')]

reportdf$to_save <- apply(reportdf, 1, to_save, reportdf)

test <- function(){
for(i in stop_names$shortName){
  stop_report(i)
}}

library(ggplot2)
library(ggmap)
krk_map <- get_map(c(20, 50), source = 'stamen')
ggmap(krk_map) + 
  #geom_point(data = stop_names, aes(longitude/3600000, latitude/3600000), color =  'red', alpha = 0.5)
  geom_text(data = stop_names, aes(longitude/3600000, latitude/3600000, label = name), size = 2)

qmplot(longitude, latitude, data = stop_names, geom = 'point')

test <- function(x){
  if(x == 0){
    print('Failed')
    return(NULL)
    print('Should not appear')
  }
  print('Success')
}

up <- function(x){
  test(x)
  x + 1
}

library(ggmap)
krk_bbox <- make_bbox(longitude, latitude, data = stop_names)
krk_view = c(lon = mean(krk_bbox[c(1, 3)]), lat = mean(krk_bbox[c(2, 4)]), zoom = 12)
krk_map <- get_map(krk_bbox, source = 'stamen', maptype = 'terrain-lines'); 


friday_map <- left_join(friday, stop_names, by = c('stop' = 'shortName', 'stopName' = 'name')) %>%
  filter(delay < 20) %>% 
  group_by(stop, stopName, longitude, latitude) %>% dplyr::summarize(avg = as.numeric(mean(delay, na.rm = TRUE)))

ggmap(krk_map, base_layer = ggplot(friday_map, aes(longitude, latitude))) + 
  geom_point(aes(size = avg, col = avg))

report2 <- report %>% filter(!is.na(seq_num)) %>% arrange(tripId)
check_vector <- rep_len(0, nrow(report2))

for(line in 1:(nrow(report2)-1)){
  if(report2$seq_num[line + 1] != report2$seq_num[line] + 1 &
     report2$tripId[line + 1] == report2$tripId[line]){
    check_vector[line] <-  1
  }
}

qplot(seq_along(check_vector), check_vector, geom = 'col')
qplot(report2$time_stamp[which(check_vector == 1)], geom = 'histogram', bins = 50)

end <- as.POSIXct('2018-07-20 00:30')
periodic_report(end)

write_test <- function(i){
  x <- data.frame(value = rep(i, 10))
  write.table(x, 'csv_test.csv', sep = ',', row.names = FALSE, fileEncoding = 'UTF-8', append = TRUE)
}


newroutes <- routes[routes$number == 3 & routes$direction == 'Krowodrza Górka', ]

ggmap(krk_map, base_layer = ggplot(routes, aes(x = from.longitude, y = from.latitude))) + 
  geom_segment(aes(x = from.longitude, y = from.latitude, 
                   xend = to.longitude, yend = to.latitude),
               size = 1) + theme(legend.position = 'none')

#arrow = arrow(length = unit(0.15, 'cm')), position = position_jitter(0.0006, 0.0006)

podgorze <- stop_names %>% filter(name == 'Podgórze SKA')
ggmap(krk_map, base_layer = ggplot(data = podgorze, aes(longitude, latitude))) + geom_point(size = 3)

library(leaflet)
library(RColorBrewer)
library(htmltools)

pal <- colorNumeric('RdYlGn', domain = c(0.06, 2.5), reverse = TRUE)

friday_map <- left_join(friday, stop_names, by = c('stop' = 'shortName', 'stopName' = 'name')) %>% 
  group_by(stop, stopName, longitude, latitude) %>% 
  summarise(avg = as.numeric(mean(delay, na.rm = TRUE)))

leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(minzoom = 8)) %>% 
  setView(lng = 19.9388, lat = 50.0617, zoom = 12) %>% 
  setMaxBounds(krk_bbox[[1]] - 0.03, krk_bbox[[2]] - 0.03, krk_bbox[[3]] + 0.03, krk_bbox[[4]] + 0.03) %>% 
  addCircleMarkers(data = friday_map, 
                   color = ~pal(avg), opacity = 1, fillOpacity = 0.7,
                   label = ~stopName,
                   popup = ~paste0('<b>', htmlEscape(stopName), '</b>',
                                   '</br>',
                                   'Średnie opóźnienie:', ' ', round(avg, 2), ' min'))

friday %>% group_by(tripId) %>% arrange(tripId, seq_num) %>% 
  mutate(test = seq_along(index) + min(seq_num) - 1) %>% 
  filter(seq_num != test) %>% View()

(end <- Sys.time() + minutes(2))
(saving_times <- seq(Sys.time(), end + seconds(20), by = '20 sec'))
save_index <- 2
while(Sys.time() < end){
  if(Sys.time() > saving_times[save_index]){
    print(paste(Sys.time(), 'Time up', save_index, saving_times[save_index]))
    save_index <- save_index + 1
  } else{
    print(paste(Sys.time(), 'Not yet', save_index, saving_times[save_index]))
    Sys.sleep(10)
  }
}

report %>% arrange(desc(delay)) %>% head(30) %>% View()

report <- filter(report, (tripId != '6351558574044965125' | delay < 10) &
                         tripId != '6351558574044641547' &
                         (tripId != '6351558574045034760' | delay < 10) &
                         tripId != '6351558574044551426')

write.csv(report, 'report_07-23.csv', row.names = FALSE, fileEncoding = 'UTF-8')

report22 <- report %>% filter(tripId == '6351558574044883459')

report22 %>% group_by(tripId) %>% arrange(tripId, seq_num) %>% 
  rename(by = stopName) %>% 
  mutate(from = lag(by), to = lead(by)) %>%
  ungroup() %>% 
  select(index, number, direction, time_stamp, from, by, delay, seq_num) %>%  
  inner_join(line_routes_directed, by = c('number', 'direction', 'from', 'by')) %>% View()



#####

report_directions <- report %>% 
           group_by(tripId) %>% arrange(tripId, seq_num) %>% 
  rename(by = stopName) %>% 
  mutate(from = lag(by), to = lead(by)) %>%
  filter(by %in% c("TAURON Arena Kraków Wieczysta", "AWF", "Muzeum Lotnictwa")) %>% 
  ungroup() %>% 
  select(index, number, direction, time_stamp, from, by, delay, seq_num) %>%  
  inner_join(line_routes_directed, by = c('number', 'direction', 'from', 'by'))

only_stops <- report_directions %>% group_by(by) %>% summarise(mean(delay))

directions_summary <- report_directions %>% group_by(by, name) %>%
  summarise(avg = as.numeric(mean(delay, na.rm = TRUE))) %>% ungroup()

stop_dir_summary <- function(stopName){
  filter(directions_summary, by == stopName) %>%
    select(name, avg) %>% kable(col.names = c("Trasa", "Średnie opóźnienie"), digits = 2)
}

all_dir_summaries <- lapply(stop_names$name, stop_dir_summary)
names(all_dir_summaries) <- stop_names$name

stops_map <- report %>% 
  filter(stopName %in% c("TAURON Arena Kraków Wieczysta", "AWF", "Muzeum Lotnictwa")) %>% 
  left_join(stop_names, by = c('stop' = 'shortName', 'stopName' = 'name')) %>%
  group_by(stop, stopName, longitude, latitude) %>%
  summarise(avg = as.numeric(mean(delay, na.rm = TRUE)))


report_routes <- report %>%
  group_by(tripId) %>% arrange(tripId, seq_num) %>% 
  rename(from = stopName) %>% 
  mutate(to = lead(from)) %>%
  filter(from %in% c("TAURON Arena Kraków Wieczysta", "AWF", "Muzeum Lotnictwa") |
           to %in% c("TAURON Arena Kraków Wieczysta", "AWF", "Muzeum Lotnictwa")) %>%
  filter(!is.na(to)) %>% 
  mutate(name = paste(from, to, sep = " - "), delay_increment = c(diff(delay), 0)) %>% 
  ungroup() %>% 
  select(index, time_stamp, number, direction, name, delay, delay_increment, seq_num) %>% 
  inner_join(routes, by = 'name') 

routes_stops <- report_routes %>% group_by(from, to) %>% summarise(mean(delay))

report_routes_map <- report_routes %>% group_by(name) %>%
  summarise(avg_increment = mean(delay_increment)) %>% 
  arrange(desc(avg_increment)) %>% inner_join(routes, by = 'name') %>% 
  select(-from, -to)

errorous <- read_csv('~/R/kraktram/tmp_reports/report_tmp_07-25.csv', col_types = cols(
  index = col_integer(),
  time_stamp = col_datetime(format = ""),
  stop = col_integer(),
  number = col_integer(),
  direction = col_character(),
  plannedTime = col_datetime(format = ""),
  vehicleId = col_character(),
  tripId = col_character(),
  status = col_character()
)) %>% filter(tripId == '6351558574044474122')

errorous <- errorous[!to_remove(errorous), ]
errorous <- report_completion(errorous)

errorous %>% select(index, seq_num, stopName, time_stamp, plannedTime, status, delay) %>% 
  saveRDS(file = 'error_example.RDS')

x <- rnorm(100000)
assgn <- sample(c(0, 1), 100000, prob = c(0.8, 0.2), replace = TRUE)
assgn <- as.logical(assgn)
median(x)

median(x[assgn])
mean(abs(x[assgn] - median(x[assgn])))

median(x[!assgn])
mean(abs(x[!assgn] - median(x[assgn])))

mean(x)

mean(x[assgn])
mean((x[assgn] - mean(x[assgn]))^2)

mean(x[!assgn])
mean((x[!assgn] - mean(x[assgn]))^2)
mean((x[!assgn] - mean(x[!assgn]))^2)
