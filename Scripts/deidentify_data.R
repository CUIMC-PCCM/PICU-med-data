# deidentify_data.R: loads in identified data and substitutes new ID.

'Usage: 
  deidentify_data.R --path_to_data=<path_to_data> --identity_header=<identity_header> --path_to_key_data=<path_to_key_data> [--project_name=<project_name>] [--debug=<debug>]

  
  Options:
  -h --help
  --path_to_data=<path_to_data> path to identified data
  --identity_header=<identity_header> column header with identity in identified data. This should have the same values as "identified_key" column in key_data
  --path_to_key_data=<path_to_key_data> csv file with two columns. headers should be identified_key and deidentified_key
  --project_name=<project_name> label for analysis. example might "picu" or "IPF" [default: temp_case]
  --debug=<debug> [default: FALSE]
  

' -> doc
library(tidyverse)
library(data.table)
library(here)
library(docopt)
"%!in%" <- Negate("%in%")
arguments <- docopt(doc, version = 'deidentify_data.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  arguments$path_to_data <- here("Input","RITM0429582_V1_epicVisitAdtMar_halfRows.txt")
  arguments$path_to_key_data <- here("Input","identify_key.csv")
  arguments$identity_header <- "EMPI"
  arguments$project_name <- "PGX"
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
initialize_logfile(time_case_prefix, "deidentify_data")
logr::log_print(arguments)

# load in identified data
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

# load in key data
tryCatch(
  {
    key_data <- fread(arguments$path_to_key_data)
  }, 
  error=function(e){
    message("error loading in key data")
    logr::log_print(e)
  },
  warning=function(w) {
    message('A Warning Occurred loading in key data')
    logr::log_print(w)
  }
)

# remvte duplicate keys
tryCatch(
  {
    duplicate_keys <-  key_data$identified_key[duplicated(key_data$identified_key)]
    location_unique_keys <- as.character(identified_data[[c(arguments$identity_header)]]) %!in% as.character(duplicate_keys)
    identified_data_unique <- identified_data[location_unique_keys , ]
    nrow(identified_data)
    nrow(identified_data_unique)
  }, 
  error=function(e){
    message("error creating deidentified key")
    logr::log_print(e)
  },
  warning=function(w) {
    message('A Warning occurred creating deidentified key')
    logr::log_print(w)
  }
)

tryCatch(
  {
    merge_df <- merge(identified_data_unique,key_data, by.x = arguments$identity_header, by.y = "identified_key", all.x = TRUE)
    # identified_data$deidentified_id <- sapply(identified_data[[c(arguments$identity_header)]], function(x) key_data$deidentified_key[key_data$identified_key == x])
  }, 
  error=function(e){
    message("error creating deidentified key")
    logr::log_print(e)
  },
  warning=function(w) {
    message('A Warning occurred creating deidentified key')
    logr::log_print(w)
  }
)


tryCatch(
  {
    new_names <- names(merge_df)
    new_names <- new_names[new_names != arguments$identity_header]
    write_df = subset(merge_df, select = c(new_names) )
    write.table(x = write_df, file = gzfile(here("Intermediate","deidentified_data.tsv.gz")), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
    # write.csv(x = write_df, file = here("Intermediate","deidentified_data.csv"), quote = FALSE, row.names = FALSE, col.names = TRUE)
    # write.table()
  }, 
  error=function(e){
    message("error writing deidentified data")
    logr::log_print(e)
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
