# deidentify_data.R: loads in identified data and substitutes new ID.

'Usage: 
  deidentify_data.R --data_file=<data_file> --epic_cdw=<epic_cdw> --key_data_file=<key_data_file> --identity_header=<identity_header> [--project_name=<project_name>] [--debug=<debug>]

  
  Options:
  -h --help
  --data_file=<data_file> name identified data. Should be located in Input
  --epic_cdw=<epic_cdw> epic or xdw
  --key_data_file=<key_data_file> csv file with two columns. headers should be identified_key and deidentified_key. name of file. should be located in Input
  --identity_header=<identity_header> column header of mrn
  --project_name=<project_name> label for analysis. example might "picu" or "IPF" [default: temp_case]
  --debug=<debug> [default: FALSE]

' -> doc
# --identity_header=<identity_header> column header with identity in identified data. This should have the same values as identified_key column in key_data
#    

# --key_data_file=<key_data_file> csv file with two columns. headers should be identified_key and deidentified_key. name of file. should be located in Input
#
library(tidyverse)
library(data.table)
library(here)
library(docopt)
"%!in%" <- Negate("%in%")
arguments <- docopt(doc, version = 'deidentify_data.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  # arguments$path_to_data <- here("Input","RITM0429582_V1_epicVisitAdtMar_complete.txt.gz")
  arguments$data_file <- "RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"
  arguments$epic_cdw <- "epic"
  arguments$key_data_file <- "identify_key.csv"
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
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",Sys.time()))), "_",arguments$project_name, "_",arguments$epic_cdw,"_")
initialize_logfile(time_case_prefix, "deidentify_data")
logr::log_print(arguments)

logr::log_print("loading in identified data")
tryCatch(
  {
    identified_data <- read.table(here("Input",arguments$data_file), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE)
    identified_data[[paste0(arguments$identity_header,"_char")]] <- as.character(identified_data$EMPI)
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

logr::log_print("loading in key data")
tryCatch(
  {
    key_data <- fread(here("Input", arguments$key_data_file))
    key_data$identified_key_char <- as.character(key_data$identified_key)
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

logr::log_print("removing duplicate keys")
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

logr::log_print("identifying mrns missing between identify code and data")
tryCatch(
  {
    logr::log_print("in MAR and key")
    logr::log_print(length(mrn_in_key_and_mar <- unique(intersect(key_data$identified_key_char,identified_data_unique$EMPI_char))))
    logr::log_print("in MAR but not in key")
    logr::log_print(length(MAR_mrn_wo_deidentified_key <- unique(identified_data_unique$EMPI_char[identified_data_unique$EMPI_char %!in% key_data$identified_key_char ])))
    logr::log_print("in key but not in mrn")
    logr::log_print(length(deidentified_key_mrn_wo_MAR <- unique(key_data$identified_key_char[key_data$identified_key_char %!in%  identified_data_unique$EMPI_char])))
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



logr::log_print("adding deidentified code")
tryCatch(
  {
    nrow(merge_df <- merge(identified_data_unique,key_data, by.x = paste0(arguments$identity_header,"_char"), by.y = "identified_key_char", all.x = TRUE))
    logr::log_print(length(unique(merge_df$EMPI_char)))
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

logr::log_print("writing results")
tryCatch(
  {
    new_names <- names(merge_df)
    new_names <- new_names[new_names != arguments$identity_header]
    write_df = subset(merge_df, select = c(new_names) )
    write_df$EMPI_char <- NULL
    write_df$identified_key <- NULL
    write.table(x = write_df %>% filter(!is.na(deidentified_key)), file = gzfile(here("Intermediate",paste0(time_case_prefix,"deidentified_data.tsv.gz"))), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
    # write.csv(x = write_df, file = here("Intermediate","deidentified_data.csv"), quote = FALSE, row.names = FALSE, col.names = TRUE)
    # write.table()
  }, 
  error=function(e){
    message("error writing deidentified data")
    logr::log_print(e)
    stop("stoped during writing deidentified data")
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
