#### 07-23, Monday ####
# Probably would not be used due to changes in timeplans and routes #

#report generator broke down when doing final clearing because of bugs. It must be done by hand

report <- read_csv('tmp_reports/report_tmp_07-23.csv', col_types = cols(
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

report <- report[!to_remove(report), ] #Last clering from repeated observations
report <- report_completion(report) #Merging with other data sets

#Removing strange observations
report <- filter(report, (tripId != '6351558574044965125' | delay < 10) &
                   tripId != '6351558574044641547' &
                   (tripId != '6351558574045034760' | delay < 10) &
                   tripId != '6351558574044551426')
#Saving
write.csv(report, 'report_07-23.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#### 07-24, Tuesday ####

#report generator broke down when doing final clearing because of bugs. It must be done by hand

report <- read_csv('tmp_reports/report_tmp_07-24.csv', col_types = cols(
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

report <- report[!to_remove(report), ] #Last clering from repeated observations
report <- report_completion(report) #Merging with other data sets

#Cleaning erroneous records
report <- filter(report, (tripId != '6351558574044891653' | delay < 15) &
                   (tripId != '6351558574044891652' | delay < 15) &
                   (tripId != '6351558574044768776' | delay < 15) &
                   (tripId != '6351558574044891654' | delay < 15) &
                   (tripId != '6351558574044592644') &
                   (tripId != '6351558574044543493' | delay < 15) &
                   (tripId != '6351558574044637702' | delay < 15) &
                   tripId != '6351558574044940806' &
                   (tripId != '6351558574044592645' | delay < 5) &
                   (tripId != '6351558574044637703' | delay < 10) &
                   (tripId != '6351558574044531204' | delay < 10) &
                   (tripId != '6351558574044637698' | delay < 10) &
                   (tripId != '6351558574044637700' | delay < 10) &
                   (tripId != '6351558574044531203' | delay < 4) &
                   (tripId != '6351558574044891655' | delay < 15) &
                   (tripId != '6351558574044592647' | delay < 15) &
                   #(tripId != '6351558574044510731' | delay < 15) & # to be checked
                   (tripId != '6351558574044625418' | delay < 15) &
                   (tripId != '6351558574044510732') &
                   (tripId != '6351558574044625419' | delay < 3) &
                   (tripId != '6351558574044363272') &
                   (tripId != '6351558574044363273') &
                   (tripId != '6351558574044363274') &
                   (tripId != '6351558574044891657' | delay < 15) &
                   (tripId != '6351558574044363276' | delay < 15) &
                   (tripId != '6351558574045018632') &
                   (tripId != '6351558574044891658' | delay < 15) &
                   (tripId != '6351558574044805648') &
                   (tripId != '6351558574044895755') &
                   (hour(time_stamp) < 16 | delay < 15) &
                   delay < 12)

#Saving
write.csv(report, 'report_07-24.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#### 07-25, Wednesday ####

#report generator broke down when doing final clearing because of bugs. It must be done by hand

report <- read_csv('tmp_reports/report_tmp_07-25.csv', col_types = cols(
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

report <- report[!to_remove(report), ] #Last clering from repeated observations
report <- report_completion(report) #Merging with other data sets

#Cleaning erroneous records
report <- filter(report, (tripId != '6351558574044654338') &#
(tripId != '6351558574044613378' | delay < 15) &#
(tripId != '6351558574044891906' | delay < 2) &#
(tripId != '6351558574044601090') & #GPS problem
(tripId != '6351558574044527375' | delay < 3) &#
(tripId != '6351558574044945164') &#
(tripId != '6351558574044527375') &#
tripId != '6351558574044531468' &#
(tripId != '6351558574044879626' | delay < 8) & #
(tripId != '6351558574044474122' | delay < 7) & # good example
(tripId != '6351558574044543756' | delay < 6) &#
(tripId != '6351558574044535567' | delay < 10) &#
(tripId != '6351558574044945164') &#
(tripId != '6351558574044527375') &#
(tripId != '6351558574044474122' | delay < 7) &#
(tripId != '6351558574044519184') &#
(tripId != '6351558574044531468') &#
(tripId != '6351558574044519184') &#
(tripId != '6351558574044543756' | delay < 3) &
(tripId != '6351558574044924686') &
(!(number %in% c(6, 24)) | hour(time_stamp) < 16 | hour(time_stamp) > 17) &#
(tripId != '6351558574044691210') &#
(tripId != '6351558574044855057') &#
(tripId != '6351558574044842771') &#
(tripId != '6351558574044855057') &
(delay < 15 | hour(time_stamp) < 18) &
(!(number %in% c(6, 24)) | hour(time_stamp) < 18 | delay < 5) &
(tripId != '6351558574044584722' | delay < 7) &
(tripId != '6351558574044449556') &
(tripId != '6351558574044998426' | delay < 10) &
(tripId != '6351558574044449556' | delay < 20) &
(tripId != '6351558574044584722') &
(tripId != '6351558574045006620' ) &
(tripId != '6351558574044982045') &
(tripId != '6351558574044470029') &
(tripId != '6351558574044449556') &
(number != 50 | hour(time_stamp) < 22) &
(tripId != '6351558574044691211'))

                   
#Saving
write.csv(report, 'report_07-25.csv', row.names = FALSE, fileEncoding = 'UTF-8')


#### 07-26, Thursday ####

#Everything was ok with autocompleting and cleaning from duplicates
report <-  read_csv('~/R/kraktram/reports/report_07-26.csv', col_types = cols(
  index = col_integer(),
  time_stamp = col_datetime(format = ""),
  stop = col_integer(),
  stopName = col_character(),
  number = col_integer(),
  direction = col_character(),
  plannedTime = col_datetime(format = ""),
  vehicleId = col_character(),
  tripId = col_character(),
  status = col_character(),
  delay = col_double(),
  seq_num = col_integer()
))

ggplot(report, aes(time_stamp, delay)) + geom_jitter(alpha = 0.3)

#Let automate some cleaning

broken_and_repeated <- function(report){
  df <- report %>% select(index, tripId) 
    rep_forward <- duplicated(df)
    rep_backward <- duplicated(df, fromLast = TRUE)
    
    rep <- !(rep_forward | rep_backward)
    rep
}
report <- report[broken_and_repeated(report), ]

# ggplot(report %>% filter(hour(time_stamp) > 19), aes(time_stamp, delay)) + 
#   geom_jitter(alpha = 0.3)
# 
# report %>% filter(hour(time_stamp) > 19) %>% arrange(desc(delay)) %>% head(100) %>% 
#   View()

report <- filter(report, (tripId != '6351558574047575042') &
                   (tripId != '6351558574047280130') &
                   (tripId != '6351558574047472642') &
                   (tripId != '6351558574047157250') &
                   (tripId != '6351558574047013891') &
                   (tripId != '6351558574047632386') &
                   (tripId != '6351558574047284232' | status == 'STOPPING') &
                   (tripId != '6351558574047284232' | status == 'STOPPING') &
                   number < 60)

write.csv(report, 'reports/report_07-26.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#### 07-27, Friday ####

#Internet conncetion was broken and report is splitted into two files

report <- read_csv('tmp_reports/report_tmp_07-27_part1.csv', col_types = cols(
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

report <- rbind(report,
                read_csv('tmp_reports/report_tmp__07-27_part2.csv', col_types = cols(
                  index = col_integer(),
                  time_stamp = col_datetime(format = ""),
                  stop = col_integer(),
                  number = col_integer(),
                  direction = col_character(),
                  plannedTime = col_datetime(format = ""),
                  vehicleId = col_character(),
                  tripId = col_character(),
                  status = col_character()
                )) %>% unique())

report <- report[!to_remove(report), ] #Last clering from repeated observations
report <- report_completion(report) #Merging with other data sets

report <- report[broken_and_repeated(report), ]

# ggplot(report %>% filter(hour(time_stamp) > 19), aes(time_stamp, delay)) + 
#   geom_jitter(alpha = 0.3)
# 
# report %>% filter(hour(time_stamp) > 19) %>% arrange(desc(delay)) %>% head(100) %>% 
#   View()

report <- filter(report, (tripId != '6351558574046485763') &#
                   (tripId != '6351558574046485764' | delay < 5) &#
                   (tripId != '6351558574046485767') &#
                   (tripId != '6351558574046485765') &#
                   (tripId != '6351558574046485766') &#
                   (tripId != '6351558574046485768') &#
                   (tripId != '6351558574046485769') &#
                   (tripId != '6351558574046485770') &#
                   (tripId != '6351558574046485771') &#
                   (tripId != '6351558574046452999') &#
                   (tripId != '6351558574046305546') &#
                   (tripId != '6351558574046784780') &#
                   (tripId != '6351558574046747918') &#
                   number < 60)

write.csv(report, 'reports/report_07-27.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#### 07-30, Monday ####

#Everything was fine with downloading

report <-  read_csv('~/R/kraktram/reports/report_07-30.csv', col_types = cols(
  index = col_integer(),
  time_stamp = col_datetime(format = ""),
  stop = col_integer(),
  stopName = col_character(),
  number = col_integer(),
  direction = col_character(),
  plannedTime = col_datetime(format = ""),
  vehicleId = col_character(),
  tripId = col_character(),
  status = col_character(),
  delay = col_double(),
  seq_num = col_integer()
))

report <- report[broken_and_repeated(report), ]

# ggplot(report %>% filter(hour(time_stamp) > 19), aes(time_stamp, delay)) +
#   geom_jitter(alpha = 0.3)
# 
# report %>% filter(hour(time_stamp) > 19) %>% arrange(desc(delay)) %>% head(100) %>%
#   View()

report <- filter(report, (tripId != '6351558574044610562' | delay < 10) &
                   (tripId != '6351558574044774402' | delay < 7) &
                   (tripId != '6351558574044454917') &
                   (tripId != '6351558574045020165' | seq_num > 13) &
                   (tripId != '6351558574045024263') &
                   (tripId != '6351558574045024262') &
                   (tripId != '6351558574044786700') &
                   (tripId != '6351558574046485770') &
                   (tripId != '6351558574044790795') &
                   (tripId != '6351558574044708873') &
                   (tripId != '6351558574044991503') &
                   (tripId != '6351558574044422156') &
                   (tripId != '6351558574044708873') &
                   (tripId != '6351558574044803086') &#
                   (tripId != '6351558574044930060') &#
                   (tripId != '6351558574044626956' | seq_num < 12 ) &#
                   (tripId != '6351558574044573707') &#
                   (tripId != '6351558574044778510' | delay < 5) &#
                   (tripId != '6351558574044631054' | delay < 5) & #
                   (tripId != '6351558574044774414' | delay < 5) &#
                   (tripId != '6351558574045069327' | delay < 5) &#
                   (tripId != '6351558574044504078' | delay < 5) & #
                   (tripId != '6351558574044762124' | delay < 5) & #
                   (tripId != '6351558574044930060') &#
                   (tripId != '6351558574044766222' | delay < 5) &#
                   (tripId != '6351558574044770316' | delay < 5) &
                   (tripId != '6351558574044573707') &#
                   (tripId != '6351558574044930060') &#
                   (tripId != '6351558574044758030' | delay < 5) &#
                   (tripId != '6351558574044925966') & #
                   (tripId != '6351558574044758030' | delay < 5) &#
                   (tripId != '6351558574044782606' | delay < 5) &#
                   (tripId != '6351558574044786703' | delay < 5) &#
                   (tripId != '6351558574044504085') &#
                   (tripId != '6351558574044807190') &#
                   (tripId != '6351558574044504085') &#
                   number < 60)

write.csv(report, 'reports/report_07-30.csv', row.names = FALSE, fileEncoding = 'UTF-8')

#### 07-31, Tuesday ####

#Everything was fine with downloading

report <-  read_csv('~/R/kraktram/reports/report_07-31.csv', col_types = cols(
  index = col_integer(),
  time_stamp = col_datetime(format = ""),
  stop = col_integer(),
  stopName = col_character(),
  number = col_integer(),
  direction = col_character(),
  plannedTime = col_datetime(format = ""),
  vehicleId = col_character(),
  tripId = col_character(),
  status = col_character(),
  delay = col_double(),
  seq_num = col_integer()
))

report <- report[broken_and_repeated(report), ]

ggplot(report %>% filter(hour(time_stamp) >  20), aes(time_stamp, delay)) +
  geom_jitter(alpha = 0.3)

report %>% filter(hour(time_stamp) >  20) %>% arrange(desc(delay)) %>% head(100) %>%
  View()

report <- filter(report, (tripId != '6351558574044639493' | delay < 10) &#
                   (tripId != '6351558574044487949' | delay < 7) &#
                   (tripId != '6351558574044492034') &#
                   (tripId != '6351558574045016324' | seq_num > 17) &#
                   (tripId != '6351558574044393737') &#
                   (tripId != '6351558574044365058') &#
                   (tripId != '6351558574044467458') &#
                   (tripId != '6351558574044528899') &#
                   (tripId != '6351558574044872966') &#
                   (tripId != '6351558574044504330') &#
                   (tripId != '6351558574044487948') &#
                   (tripId != '6351558574044393738') &#
                   (tripId != '6351558574044872968') &#
                   (tripId != '6351558574044676366') &#
                   (tripId != '6351558574044483850') &#
                   (tripId != '6351558574044885259' | seq_num < 19 ) &#
                   (tripId != '6351558574044410134') &#
                   (tripId != '6351558574045044997' | delay < 5) &#
                   (tripId != '6351558574044885257' | delay < 10) & #
                   (tripId != '6351558574044631310' | delay < 15) &#
                   (tripId != '6351558574044885257' | delay < 10) &#
                   (tripId != '6351558574044410134' | delay < 5) & #
                   (tripId != '6351558574044885257' | delay < 5) & #
                   (tripId != '6351558574044455183') &#
                   (tripId != '6351558574044508430' | delay < 5) &#
                   (tripId != '6351558574044725519' | delay < 5) &#
                   (tripId != '6351558574044909841') &#
                   (tripId != '6351558574044410147') &#
                   (tripId != '6351558574044639507' | delay < 5) &#
                   (tripId != '6351558574044418325') & #
                   (tripId != '6351558574044631314' | delay < 5) &#
                   (tripId != '6351558574044483854' | delay < 5) &#
                   (tripId != '6351558574044627219' | delay < 5) &#
                   (tripId != '6351558574044483854' | delay < 5) &#
                   (tripId != '6351558574044463375' | delay < 5) &#
                   (tripId != '6351558574045012239' | delay < 5) &#
                   (tripId != '6351558574044492055') &#
                   (tripId != '6351558574044737808' | delay < 7) &#
                   (tripId != '6351558574045036815' | delay < 7) &#
                   (tripId != '6351558574044627220') &#
                   (tripId != '6351558574044737808' | delay < 5) &#
                   (tripId != '6351558574044836120' | delay < 8) &#
                   (tripId != '6351558574044446997') &#
                   (tripId != '6351558574044639506' | delay < 5) &#
                   (tripId != '6351558574045065487') &#
                   (tripId != '6351558574044844311' | delay < 5) &#
                   (tripId != '6351558574044639506' | delay < 8) &#
                   number < 60)

write.csv(report, 'reports/report_07-31.csv', row.names = FALSE, fileEncoding = 'UTF-8')
