# deidentify_data.R: loads in identified data and substitutes new ID.

'Usage: 
  deidentify_data.R --data_file_epic=<data_file_epic> --data_file_cdw=<data_file_cdw> --key_data_file=<key_data_file> --identity_header=<identity_header> [--project_name=<project_name>] [--data_file_cdw_loc=<data_file_cdw_loc>] [--debug=<debug>]

  
  Options:
  -h --help
  --data_file_epic=<data_file_epic> name identified epic data. Should be located in Input
  --data_file_cdw=<data_file_cdw> name identified epic data. Should be located in Input
  --key_data_file=<key_data_file> csv file with two columns. headers should be identified_key and deidentified_key. name of file. should be located in Input
  --identity_header=<identity_header> column header of mrn
  --data_file_cdw_loc=<data_file_cdw_loc> name of location data for CDW [default: NA]
  --project_name=<project_name> label for analysis. example might "picu" or "IPF" [default: temp_case]
  --debug=<debug> [default: FALSE]

' -> doc
# --identity_header=<identity_header> column header with identity in identified data. This should have the same values as identified_key column in key_data
#    

# --key_data_file=<key_data_file> csv file with two columns. headers should be identified_key and deidentified_key. name of file. should be located in Input
#
tryCatch({
  
  library(tidyverse)
  library(data.table)
  here::i_am("Scripts/deidentify_data.R")
  library(here)
  library(docopt)
  library(parallel)
  library(DescTools)
  library(logr)
  library(lubridate)
  source(here("Scripts","useful_functions.R"))
  source(here("Scripts","deidentify_data_functions.R"))
}, error = function(e) {
  message("An error occurred loading libraries: ", e$message)
  quit("no", status = 10)
})
"%!in%" <- Negate("%in%")
# parse command line arguments
arguments <- docopt(doc, version = 'deidentify_data.R')

# parse command line arguments
if(arguments$debug == "TRUE"){
  arguments<-list()
  arguments$data_file_epic <- "RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"
  arguments$data_file_cdw <- "RITM0429582_V1_cdwRx.txt.gz"
  arguments$data_file_cdw_loc <- "RITM0429582_V1_cdwVisitDetailForRx.txt.gz"
  arguments$key_data_file <- "2023_09_14_17_29_34.859167_identity_key.csv"
  arguments$identity_header <- "EMPI"
  arguments$project_name <- "PGX"
}



# Create time_stamp
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",round_date(Sys.time(), unit = "minutes")))), "_",arguments$project_name, "_")

# Creating log file
tryCatch({
  initialize_logfile(time_case_prefix, "deidentify_data")
  logr::log_print(arguments)
}, error = function(e) {
  message("An error occurred opening the log file: ", e$message)
  quit("no", status = 10)
})

logr::log_print("loading in deidentified data")
tryCatch({
  if(arguments$data_file_epic != "NA"){
    logr::log_print("load in epic data")
    identified_data_epic <- read.table(here("Input",arguments$data_file_epic), 
                                       header = TRUE, 
                                       sep = "\t", quote = "", 
                                       as.is = TRUE, 
                                       fill = TRUE)
    identified_data_epic[[paste0(arguments$identity_header,"_char")]] <- as.character(identified_data_epic$EMPI)
  } 
  if(arguments$data_file_cdw != "NA"){
    logr::log_print("load in cdw data")
    identified_data_cdw <- read.table(here("Input",arguments$data_file_cdw), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE) %>% 
      mutate(year = substring(PRIMARY_TIME, 1, 4), month = substring(PRIMARY_TIME, 6, 7), day = substr(PRIMARY_TIME, 9, 10), hr = substring(PRIMARY_TIME, 12, 13), min = substring(PRIMARY_TIME, 15, 16), sec = paste0(substring(PRIMARY_TIME, 18, 19),
                                                                                                                                                                                                                       substring(PRIMARY_TIME, 21, 26))) %>% mutate(time_stamp=paste0(year,month,day,hr,min,sec)) %>% 
      filter(PRIMARY_TIME != "",CODED_VALUE_desc %like% "Cerner Drug:%") %>% arrange(time_stamp)
    identified_data_cdw[[paste0(arguments$identity_header,"_char")]] <- as.character(identified_data_cdw$EMPI)
  }
  logr::log_print("successfully loaded in deidentified data")
}, error=function(e){
    message("An error occurred loading in identified data: ", e$message)
    quit("no", status = 10)
})

# if CDW data loaded in, add room number
tryCatch({
  if(arguments$data_file_cdw_loc != "NA"){
    logr::log_print("adding room number for cdw data")
    cdw_loc <- read.table(here("Input",arguments$data_file_cdw_loc), 
                          header = TRUE, 
                          sep = "\t", 
                          quote = "", 
                          as.is = TRUE, 
                          fill = TRUE)  %>% 
      mutate(year = substring(LOC__DATE, 1, 4), 
             month = substring(LOC__DATE, 6, 7), 
             day = substr(LOC__DATE, 9, 10), 
             hr = substring(LOC__DATE, 12, 13), 
             min = substring(LOC__DATE, 15, 16),
             sec = paste0(substring(LOC__DATE, 18, 19),
                          substring(LOC__DATE, 21, 26))) %>%
      mutate(time_stamp=paste0(year,month,day,hr,min,sec)) %>% 
      filter(LOC__DATE != "") %>% arrange(time_stamp)
    cdw_loc[[paste0(arguments$identity_header,"_char")]] <- as.character(cdw_loc$EMPI)
      
      room_list <- mclapply(1:nrow(identified_data_cdw), 
                            function(y) return_room_location(y,identified_data_cdw,cdw_loc), 
                            mc.cores = detectCores(logical = FALSE)-1 )
      identified_data_cdw$LOC__ROOM <- unlist(room_list)
      logr::log_print("finished room number for cdw data")
  }
}, error=function(e){
  message("An error occurred adding room number for cdw data: ", e$message)
  quit("no", status = 10)
})

logr::log_print("loading in key data")
tryCatch({
  key_data <- fread(here("Input", arguments$key_data_file))
  key_data$identified_key_char <- as.character(key_data$identified_key)
  logr::log_print("success loading in key data")
}, error = function(e) {
  message("An error occurred loading in key data: ", e$message)
  quit("no", status = 10)
})

logr::log_print("removing duplicate keys")
tryCatch({
  key_data_unique <- key_data %>% filter(!is.na(identified_key), !is.na(deidentified_key)) %>% distinct(identified_key, .keep_all = TRUE)
  nrow(key_data_unique)
  # nrow(identified_data_unique)
}, error = function(e) {
  message("An error occurred removing duplicate keys: ", e$message)
  quit("no", status = 10)
})


logr::log_print("identifying mrns missing between identify code and data")
tryCatch({
  if(arguments$data_file_epic != "NA"){
    logr::log_print("checking epic data")
    check_mar_vs_identity_key(identified_data_epic,key_data_unique)
  }
  if(arguments$data_file_cdw != "NA"){
    logr::log_print("checking cdw data")
    check_mar_vs_identity_key(identified_data_cdw,key_data_unique)
  }
  if(arguments$data_file_epic != "NA" && arguments$data_file_cdw != "NA"){
    logr::log_print("checking union of epic and cdw data")
    check_mar_vs_identity_key(rbind(identified_data_epic %>% select(EMPI_char),identified_data_cdw %>% select(EMPI_char)),key_data_unique)
  }
}, error = function(e) {
  message("An error identifying mrns missing between identify code and data: ", e$message)
  quit("no", status = 10)
})

logr::log_print("adding deidentified code")
tryCatch({
  if(arguments$data_file_epic != "NA"){
    logr::log_print("adding deidentified code to epic data")
    nrow(merge_df_epic <- merge(identified_data_epic,key_data_unique, by.x = paste0(arguments$identity_header,"_char"), by.y = "identified_key_char", all.x = TRUE))
  }
  if(arguments$data_file_cdw != "NA"){
    logr::log_print("adding deidentified code to cdw data")
    nrow(merge_df_cdw <- merge(identified_data_cdw,key_data_unique, by.x = paste0(arguments$identity_header,"_char"), by.y = "identified_key_char", all.x = TRUE))
  }
}, error = function(e) {
  message("An error adding deidentified code: ", e$message)
  quit("no", status = 10)
})


logr::log_print("writing results")
tryCatch({
  if(arguments$data_file_epic != "NA"){
    logr::log_print("writing epic results")
    writing_deidentified_data(merge_df_epic, "epic",arguments)
  } 
  if(arguments$data_file_epic != "NA"){
    logr::log_print("writing cdw results")
    writing_deidentified_data(merge_df_cdw, "cdw",arguments)
  } 
}, error = function(e) {
  message("An error writing results: ", e$message)
  quit("no", status = 10)
})

tryCatch({
  logr::log_print("Finished")
  logr::log_close()
}, error = function(e) {
  message("Caught an error closing the log file: ", e$message)
  quit("no", status = 10)
})