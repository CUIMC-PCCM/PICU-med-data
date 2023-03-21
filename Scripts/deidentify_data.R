# deidentify_data.R: loads in identified data and substitutes new ID.

'Usage: 
  deidentify_data.R --path_to_data=<path_to_data> --identity_header=<identity_header> [--project_name=<project_name>] [--debug=<debug>]

  
  Options:
  -h --help
  --path_to_data=<path_to_data> path to identified data
  --identity_header=<identity_header> column header with identity in identified data
  --project_name=<project_name> label for analysis. example might "picu" or "IPF" [default: temp_case]
  --debug=<debug> [default: FALSE]
  

' -> doc
library(tidyverse)
library(data.table)
library(here)
library(docopt)
arguments <- docopt(doc, version = 'deidentify_data.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  arguments$path_to_data <- here("Input","RITM0429582_V1_epicVisitAdtMar_halfRows.txt")
  arguments$identity_header
}

#' Initialize log file
#' @param time_case_prefix time stamp and case group combo to append to beginning of logfile
#' @param function_name name of function being logged
initialize_logfile<- function(time_case_prefix, function_name){
  tryCatch(
    {
      dir.create(here("Intermediate"))
      dir.create(here("Intermediate","Logs"))
      dir.create(here("Intermediate","Logs",function_name))
      logr::log_open(here("Intermediate","Logs",function_name, paste0(time_case_prefix,function_name,"_logfile.log")))
      return(TRUE)
    },
    error=function(e) {
      message('An Error Occurred in creating the logfile')
      print(e)
      stop("error")
    },
    warning=function(w) {
      message('A Warning Occurred in creating the logfile')
      print(w)
      return(NA)
    }
  )
}

# main-----
# logfile creation
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",Sys.time()))), "_",arguments$project_name, "_")
initialize_logfile(time_case_prefix, "deidentify_data")
logr::log_print(arguments)

tryCatch(
  {
    identified_data <- fread(arguments$path_to_data)
  }, 
  error=function(e){
    message("error loading in identified data")
    logr::log_print(e)
  },
  warning=function(w) {
    message('A Warning Occurred loading in identified data')
    logr::log_print(w)
    return(NA)
  }
)

key_data <- fread("c:/Users/jm4279/OneDrive - cumc.columbia.edu/Research/redcap/IGMGoldsteinPatientT-Jemneurodiagseq_DATA_LABELS_2023-01-10_1740.csv", ) 
names(key_data) <- make.unique(names(key_data))

key_data_w_id <- key_data %>% mutate(igm_id = case_when(!is.na(`What is the DiagSeq Study ID?`) ~ paste0("Diagseq",`What is the DiagSeq Study ID?`,"f",`What is the DiagSeq Family ID?`), !is.na(`What is the Neuro Study ID?`) ~ paste0("Neuro",`What is the Neuro Study ID?`,"f",`What is the Neuro Family ID?`),  TRUE ~ "NONE"))

fwrite(x = key_data_w_id %>% filter(`Epic MRN` != "") %>% select(identified_key = `Epic MRN`,deidentified_key = igm_id), file = here("Input","identify_key.csv"), quote = FALSE, row.names = FALSE, col.names = TRUE)
