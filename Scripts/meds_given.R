'Usage: 
  meds_given.R  [--project_name=<project_name>] [--debug=<debug>]

  
  Options:
  -h --help
  --project_name=<project_name> label for analysis. example might "picu" or "IPF" [default: temp_case]
  --debug=<debug> [default: FALSE]
  

' -> doc
library(tidyverse)
library(data.table)
library(here)
library(docopt)
library(R.utils)
"%!in%" <- Negate("%in%")
arguments <- docopt(doc, version = 'meds_given.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  arguments$project_name <- "PGX"
  arguments$visit_departmentName <- "MSCH 9 TOWER,MSCH 11 CENTRAL,MSCH 9 CENTRAL PICU"
}

#' Initialize log file
#' @param time_case_prefix time stamp and case group combo to append to beginning of logfile
#' @param function_name name of function being logged
initialize_logfile<- function(time_case_prefix, function_name){
  dir.create(here("Intermediate"))
  dir.create(here("Intermediate","Logs"))
  dir.create(here("Intermediate","Logs",function_name))
  tryCatch(
    {
      logr::log_open(here("Intermediate","Logs",function_name, paste0(time_case_prefix,function_name,"_logfile.log")))
      return(TRUE)
    },
    error=function(e) {
      message('An Error Occurred in creating the logfile')
      print(e)
      stop("error")
      return(NA)
    },
    warning=function(w) {
      message('A Warning Occurred in creating the logfile')
      print(w)
    }
  )
}

# main-----
# logfile creation
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",Sys.time()))), "_",arguments$project_name, "_")
initialize_logfile(time_case_prefix, "meds_given")
logr::log_print(arguments)

logr::log_print("Loading in deidentified data")
tryCatch(
  {
    deidentified_data <- fread(here("Intermediate","deidentified_data.tsv.gz"))
  }, 
  error=function(e){
    message("error loading in deidentified data")
    logr::log_print(e)
  },
  warning=function(w) {
    message('A Warning Occurred loading in deidentified data')
    logr::log_print(w)
  }
)

logr::log_print("Filtering and creating med base")
tryCatch(
  {
    visit_departmentName_vector <- unlist(strsplit(arguments$visit_departmentName,","))
    deidentified_data_departmentName_filtered <- deidentified_data %>% filter(ADT_departmentName %in% visit_departmentName_vector, mar_actionName == "Given")
    
    med_base <- strsplit(deidentified_data_departmentName_filtered$marOrder_descrption, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
    med_base <- strsplit(deidentified_data_departmentName_filtered$marOrder_descrption, "(?<!-)[0-9]", perl=TRUE)
    deidentified_data_departmentName_filtered$med_base <- sapply(med_base, function(x) x[1])
  }, 
  error=function(e){
    message("error filtering and creating med bases")
    logr::log_print(e)
    stop("stoped during filtering and creating med basess")
  }
  # ,
  # warning=function(w) {
  #   message('A Warning occurred writing deidentified data')
  #   logr::log_print(w)
  #   return(NA)
  # }
)

logr::log_print("writing results")
tryCatch(
  {
    write.table(x = deidentified_data_departmentName_filtered %>% select(deidentified_key,med_base ), file = gzfile(here("Intermediate","meds_given.tsv.gz")), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
  }, 
  error=function(e){
    message("error writing meds given")
    logr::log_print(e)
    stop("stoped during writing meds given")
  }
  # ,
  # warning=function(w) {
  #   message('A Warning occurred writing deidentified data')
  #   logr::log_print(w)
  #   return(NA)
  # }
)

logr::log_print("Finished")
logr::log_close()

# string <- "abc123def-456"
# split_string <- unlist(strsplit(gsub("(.*[^-0-9])?([^-0-9]*)([0-9].*)", "\\2\\3", deidentified_data_departmentName_filtered$marOrder_descrption[1]), "[^0-9]+"))
# 
# string <- "abc123def456"
# split_string <- strsplit(string, "(?<!-)[0-9]", perl=TRUE)
# 
# strsplit(deidentified_data_departmentName_filtered$marOrder_descrption, "(?<![-]{1,})[0-9]", perl=TRUE)
# strsplit(deidentified_data_departmentName_filtered$marOrder_descrption, "(?<=^[^-]*[^-0-9])|(?<=[^-]*[^-0-9])", perl=TRUE)
# split_string <- (strsplit(gsub("([^-]*)-[^-0-9]*([0-9].*)", "\\1\\2", deidentified_data_departmentName_filtered$marOrder_descrption), "[^0-9]+"))
# 
# deidentified_data_departmentName_filtered$med_base <- sapply(med_base, function(x) x[1])
# 
# sort(unique(deidentified_data_departmentName_filtered$med_base))
# 
# temp1 <- do.call(rbind,strsplit(sub('([[:alpha:]]+)\\s*([[:digit:]]+)', '\\1$\\2', deidentified_data_departmentName_filtered$marOrder_descrption), split='\\$'))
# deidentified_data_departmentName_filtered$med_base <- temp1[,1]
# sort(unique(deidentified_data_departmentName_filtered$med_base))
# 
# temp_str <- "VITAMIN B-1 100 MG OR TABS"
# 
# strsplit(temp_str, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
# strsplit(temp_str, split = "*(0|[1-9][0-9])$", perl = TRUE)
# result <- strsplit(temp_str, "[0-9]")
# 
# library(stringr)
# ind <- str_locate(deidentified_data_departmentName_filtered$marOrder_descrption, '[0-9]+')[,1]
# setNames(data.frame(do.call(rbind, Map(function(x, y) 
#   trimws(substring(x, seq(1, nchar(x), y-1), seq(y-1, nchar(x), nchar(x)-y+1))), 
#   d, ind)))[,1:2]), c('X', 'Y'))
# 
# # deidentified_data_departmentName_filtered <- deidentified_data %>% filter(visit_specialtyDepartmentName %in% "Pediatric Intensive Care")
# 
# temp <- fread(here("Input","RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"))
# 
# temp_meds_given <- temp %>% filter( mar_actionName == "Given")
# 
# temp_departmentName_filtered <- temp 
