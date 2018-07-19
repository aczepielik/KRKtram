#! /usr/bin/env Rscript

source(file = 'report_funs.R')
stop_names <- readRDS(file = 'stop-name-db.RDS')
linestops <- readRDS(file = 'linestops.RDS')

cmd_args <- commandArgs(trailingOnly = TRUE)
end <- ymd_hm(cmd_args[1], tz = 'Europe/Warsaw')

if(length(cmd_args) == 1){
  periodic_report(end)
} else{
  line <- as.numeric(cmd_args[2])
  line_periodic_report(end, line)
}
