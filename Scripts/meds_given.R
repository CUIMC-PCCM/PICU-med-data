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
library(DescTools)
library(lubridate)
source(here("Scripts","useful_functions.R"))
"%!in%" <- Negate("%in%")
arguments <- docopt(doc, version = 'meds_given.R')

if(arguments$debug == "TRUE"){
  arguments<-list()
  arguments$project_name <- "PGX"
  arguments$epic_visit_departmentName <- "MSCH 9 TOWER,MSCH 11 CENTRAL,MSCH 9 CENTRAL PICU"
  arguments$epic_filename <- "2023_05_12_14_21_19_picu_wes_pgx_deidentified_data.tsv.gz"
  arguments$cdw_visit_departmentName <- "MSCH 9 TOWER,MSCH 11 CENTRAL,MSCH 9 CENTRAL PICU"
  arguments$cdw_filename <- "2023_05_23_15_38_39.872748_PGX_cdw_deidentified_data.tsv.gz"
}

# Create time_stamp
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",round_date(Sys.time(), unit = "minutes")))), "_",arguments$project_name, "_")

# Creating log file
tryCatch({
  initialize_logfile(time_case_prefix, "meds_given")
  logr::log_print(arguments)
}, error = function(e) {
  message("An error occurred opening the log file: ", e$message)
  quit("no", status = 10)
})

logr::log_print("Loading in deidentified data")
tryCatch(
  {
    deidentified_data_epic <- fread(here("Intermediate",arguments$epic_filename))
    deidentified_data_cdw <- read.table(here("Intermediate",arguments$cdw_filename), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE)
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

logr::log_print("Filtering and creating med base for epic data")
tryCatch(
  {
    visit_departmentName_vector <- unlist(strsplit(arguments$epic_visit_departmentName,","))
    deidentified_data_departmentName_filtered_epic <- deidentified_data_epic %>% filter(ADT_departmentName %in% visit_departmentName_vector, mar_actionName == "Given")
  
    # med_base <- strsplit(deidentified_data_departmentName_filtered_epic$marOrder_descrption, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
    med_base <- strsplit(deidentified_data_departmentName_filtered_epic$marOrder_descrption, "(?<!-)[0-9]", perl=TRUE)
    deidentified_data_departmentName_filtered_epic$med_base <- sapply(med_base, function(x) x[1])
    deidentified_data_departmentName_filtered_epic$med <- deidentified_data_departmentName_filtered_epic$marOrder_descrption
    
    # deidentified_data_departmentName_filtered_non_PICU <- deidentified_data %>% filter(ADT_departmentName %!in% visit_departmentName_vector)
    # deidentified_data_departmentName_filtered_PICU <- deidentified_data %>% filter(ADT_departmentName %in% visit_departmentName_vector)
    # logr::log_print(sprintf("%i picu patients in this data set", length(unique(deidentified_data_departmentName_filtered_PICU$EMPI_char))))
    # sort(unique(deidentified_data_departmentName_filtered_non_PICU$ADT_departmentName))
    
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

logr::log_print("Filtering and creating med base for cdw data")
tryCatch(
  {

    deidentified_data_cdw_drug_rows <- deidentified_data_cdw %>% filter(CODED_VALUE_desc %like% "Cerner Drug:%", EVENT_name == "Completed Pharmacy Order")
    drug_parse <- strsplit(deidentified_data_cdw_drug_rows$CODED_VALUE_desc, ":")
    deidentified_data_cdw_drug_rows$drug <- sapply(drug_parse, function(x) trimws(x[2]))
    # visit_departmentName_vector <- unlist(strsplit(arguments$visit_departmentName,","))
    # deidentified_data_departmentName_filtered <- deidentified_data_epic %>% filter(ADT_departmentName %in% visit_departmentName_vector, mar_actionName == "Given")
    
    med_base <- strsplit(deidentified_data_cdw_drug_rows$drug, split = " ")
    # med_base <- strsplit(deidentified_data_departmentName_filtered$marOrder_descrption, "(?<!-)[0-9]", perl=TRUE)
    deidentified_data_cdw_drug_rows$med_base <- sapply(med_base, function(x) x[1])
    deidentified_data_cdw_drug_rows$med <- deidentified_data_cdw_drug_rows$CODED_VALUE_desc

    deidentified_data_cdw_drug_rows_picu <- deidentified_data_cdw_drug_rows %>% filter(LOCATION_DESC %like any% c("CHILDREN%","CHONY%"), LOC__ROOM %like any% c("91%","90%","11%"))
    
    # temp <- deidentified_data_cdw_drug_rows %>% group_by(LOCATION_DESC) %>% summarize(countVar = n())
    # 
    # 
    # temp_loc_des <- temp_filtered %>% group_by(LOCATION_DESC) %>% summarize(countVar = n())
    # temp_loc_room <- temp_filtered %>% group_by(LOC__ROOM) %>% summarize(countVar = n())
    
    
    # deidentified_data_departmentName_filtered_non_PICU <- deidentified_data %>% filter(ADT_departmentName %!in% visit_departmentName_vector)
    # deidentified_data_departmentName_filtered_PICU <- deidentified_data %>% filter(ADT_departmentName %in% visit_departmentName_vector)
    # logr::log_print(sprintf("%i picu patients in this data set", length(unique(deidentified_data_departmentName_filtered_PICU$EMPI_char))))
    # sort(unique(deidentified_data_departmentName_filtered_non_PICU$ADT_departmentName))
    
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
    combo_med_given <- rbind(deidentified_data_departmentName_filtered_epic %>% select(deidentified_key,med_base, med ), deidentified_data_cdw_drug_rows_picu %>% select(deidentified_key,med_base, med ))
    write.table(x = combo_med_given, file = gzfile(here("Intermediate",paste0(time_case_prefix,"meds_given.tsv.gz"))), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
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

tryCatch({
  logr::log_print("Finished")
  logr::log_close()
}, error = function(e) {
  message("Caught an error closing the log file: ", e$message)
  quit("no", status = 10)
})
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
