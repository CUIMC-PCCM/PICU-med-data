# deidentify_data.R: loads in identified data and substitutes new ID.

'Usage: 
  deidentify_data.R --data_file=<data_file> --epic_cdw=<epic_cdw> --key_data_file=<key_data_file> --identity_header=<identity_header> [--project_name=<project_name>] [--data_file_cdw_loc=<data_file_cdw_loc>] [--debug=<debug>]

  
  Options:
  -h --help
  --data_file=<data_file> name identified data. Should be located in Input
  --epic_cdw=<epic_cdw> epic or cdw
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
library(tidyverse)
library(data.table)
library(here)
library(docopt)
library(parallel)
library(DescTools)
library(logr)
"%!in%" <- Negate("%in%")
arguments <- docopt(doc, version = 'deidentify_data.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  # arguments$path_to_data <- here("Input","RITM0429582_V1_epicVisitAdtMar_complete.txt.gz")
  # arguments$data_file <- "RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"
  arguments$data_file <- "RITM0429582_V1_cdwRx.txt.gz"
  arguments$data_file_cdw_loc <- "RITM0429582_V1_cdwVisitDetailForRx.txt.gz"
  arguments$epic_cdw <- "cdw"
  arguments$key_data_file <- "identify_key.csv"
  arguments$identity_header <- "EMPI"
  arguments$project_name <- "PGX"
}

#' Initialize a Logfile
#'
#' This function creates the necessary directories and initializes a log file with the specified prefix and function name.
#'
#' @param time_case_prefix Character. Prefix for the log filename which may often represent the time or case.
#' @param function_name Character. Name of the function for which the log file is being created. This helps categorize logs by function.
#'
#' @return TRUE if the log file was successfully created, NA otherwise.
#' @details If there's any error in creating the logfile, the function stops with a message, and if there's a warning, it will be printed out.
#'
#' @examples
#' \dontrun{
#' initialize_logfile("2023_09_13_", "myFunctionName")
#' }
#'
#' @importFrom logr log_open
#' @importFrom here here
#' @export
initialize_logfile <- function(time_case_prefix, function_name){
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
    if(arguments$epic_cdw == "epic"){
      identified_data <- read.table(here("Input",arguments$data_file), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE)
    } else {
      identified_data <- read.table(here("Input",arguments$data_file), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE) %>% 
        mutate(year = substring(PRIMARY_TIME, 1, 4), month = substring(PRIMARY_TIME, 6, 7), day = substr(PRIMARY_TIME, 9, 10), hr = substring(PRIMARY_TIME, 12, 13), min = substring(PRIMARY_TIME, 15, 16), sec = paste0(substring(PRIMARY_TIME, 18, 19),
                                                                                                                                                                                                                         substring(PRIMARY_TIME, 21, 26))) %>% mutate(time_stamp=paste0(year,month,day,hr,min,sec)) %>% 
        filter(PRIMARY_TIME != "",CODED_VALUE_desc %like% "Cerner Drug:%") %>% arrange(time_stamp)
    }
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

if(arguments$epic_cdw == "cdw"){
  logr::log_print("adding room number for cdw data")
  tryCatch(
    {
      cdw_loc <- read.table(here("Input",arguments$data_file_cdw_loc), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE)  %>% 
        mutate(year = substring(LOC__DATE, 1, 4), month = substring(LOC__DATE, 6, 7), day = substr(LOC__DATE, 9, 10), hr = substring(LOC__DATE, 12, 13), min = substring(LOC__DATE, 15, 16),  sec = paste0(substring(LOC__DATE, 18, 19),substring(LOC__DATE, 21, 26))) %>% 
        mutate(time_stamp=paste0(year,month,day,hr,min,sec)) %>% filter(LOC__DATE != "") %>% arrange(time_stamp)
      cdw_loc[[paste0(arguments$identity_header,"_char")]] <- as.character(cdw_loc$EMPI)
      # unique_empi_var <- sort(unique(cdw_loc$EMPI_char))
      
      x <- function(i,data_df, loc_df){
        index_gt <- (data_df$time_stamp[i] >= loc_df$time_stamp) & data_df$EMPI_char[i] == loc_df$EMPI_char
        max_gt <- max(which(index_gt == TRUE))
        return(loc_df$LOC__ROOM[max_gt])
      }
      
      # room_list <- mclapply(1:nrow(identified_data), function(y) x(y,identified_data,cdw_loc), mc.cores = 20 )
      # Create a cluster with the desired number of cores
      # For example, use detectCores() to use all available cores
      cl <- makeCluster(detectCores()-2)
      
      # Export the necessary objects to all the cluster nodes
      clusterExport(cl, list("x", "identified_data", "cdw_loc"))
      
      # Use parLapply for parallel processing
      room_list <- parLapply(cl, 1:nrow(identified_data), function(y) x(y, identified_data, cdw_loc))
      
      # Stop the cluster when done
      stopCluster(cl)
      identified_data$LOC__ROOM <- unlist(room_list)
      # for(i in 1:nrow(identified_data)){
      #   index_gt <- (identified_data$time_stamp[i] >= cdw_loc$time_stamp) & identified_data$EMPI_char[i] == cdw_loc$EMPI_char
      #   max_gt <- max(which(index_gt == TRUE))
      #   identified_data$LOC__ROOM[i] <- cdw_loc$LOC__ROOM[max_gt]
      # }
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
}

tryCatch(
  {
    key_data <- fread(here("Input", arguments$key_data_file))
    key_data$identified_key_char <- as.character(key_data$identified_key)
    logr::log_print("success loading in key data")
  }, error = function(e) {
    message("An error occurred loading in key data: ", e$message)
    quit("no", status = 10)
})

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
