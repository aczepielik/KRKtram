#! /usr/bin/env Rscript

source(file = 'report_funs.R')
stop_names <- readRDS(file = 'stop-name-db.RDS')
linestops <- readRDS(file = 'linestops.RDS')

end <- lubridate::ceiling_date(Sys.time(), 'day') + lubridate::minutes(30)
periodic_report(end)