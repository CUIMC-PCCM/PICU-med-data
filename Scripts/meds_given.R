'Usage: 
  meds_given.R  [--cdw_filename=<cdw_filename>] [--epic_filename=<epic_filename>] [--project_name=<project_name>] [--debug=<debug>]

  
  Options:
  -h --help
  --cdw_filename=<cdw_filename> [default: NA]
  --epic_filename=<epic_filename> [default: NA]
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
  arguments$epic_filename <- "2023_10_06_14_43_00_PGX_epic_deidentified_data.tsv.gz"
  arguments$cdw_visit_departmentName <- "MSCH 9 TOWER,MSCH 11 CENTRAL,MSCH 9 CENTRAL PICU" #
  arguments$cdw_filename <- "2023_10_06_14_43_00_PGX_cdw_deidentified_data.tsv.gz"
  arguments$star_allele_deidentify_key_list <- "list_of_star_alleles.txt"
  arguments$key_data_file <- "2023_09_14_17_29_34.859167_identity_key.csv"
}

# Create time_stamp
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",round_date(Sys.time(), unit = "minutes")))), 
                           "_",
                           arguments$project_name, 
                           "_")

# Creating log file
tryCatch({
  initialize_logfile(time_case_prefix, "meds_given")
  logr::log_print(arguments)
}, error = function(e) {
  message("An error occurred opening the log file: ", e$message)
  quit("no", status = 10)
})

logr::log_print("Loading in deidentified data")
tryCatch({
  if(arguments$epic_filename != "NA"){
    logr::log_print("Loading in epic data")
    deidentified_data_epic <- fread(here("Intermediate",arguments$epic_filename))
  }
  if(arguments$cdw_filename != "NA"){
    logr::log_print("Loading in cdw data")
    deidentified_data_cdw <- read.table(here("Intermediate",arguments$cdw_filename), 
                                        header = TRUE, 
                                        sep = "\t", 
                                        quote = "", 
                                        as.is = TRUE, 
                                        fill = TRUE)
  }
}, error = function(e) {
  message("An error occurred Loading in deidentified data: ", e$message)
  quit("no", status = 10)
})

logr::log_print("Filtering and creating med base")
tryCatch({
  if(arguments$epic_filename != "NA"){
    logr::log_print("Filtering and creating med base for epic data")
    visit_departmentName_vector <- unlist(strsplit(arguments$epic_visit_departmentName,","))
    logr::log_print("Filtering for meds that were actually given and in the units specific")
    deidentified_data_departmentName_filtered_epic <- deidentified_data_epic %>% 
      filter(ADT_departmentName %in% visit_departmentName_vector, mar_actionName == "Given")
    logr::log_print("Creating a med base to simplify med naming. this likely needs work as it is not currently useful. A master conversion list would be better")
    med_base <- strsplit(deidentified_data_departmentName_filtered_epic$marOrder_descrption, "(?<!-)[0-9]", perl=TRUE)
    deidentified_data_departmentName_filtered_epic$med_base <- sapply(med_base, function(x) x[1])
    deidentified_data_departmentName_filtered_epic$med <- deidentified_data_departmentName_filtered_epic$marOrder_descrption
  }
  if(arguments$cdw_filename != "NA"){
    logr::log_print("Filtering and creating med base for cdw data")
    logr::log_print("Filtering for meds that were actually given and in the units specific")
    deidentified_data_cdw %>% filter(deidentified_key == "Diagseq2002f727",CODED_VALUE_desc %like% "Cerner Drug:%",EVENT_name %in% c("Completed Pharmacy Order","New Pharmacy Order"))
    deidentified_data_departmentName_filtered_cdw <- deidentified_data_cdw %>% 
      filter(CODED_VALUE_desc %like% "Cerner Drug:%", EVENT_name %in% c("Completed Pharmacy Order","New Pharmacy Order","Discontinued Pharmacy Order"), LOC__ROOM %like any% c("91%","90%","11%","0831")) #,LOCATION_DESC %like any% c("CHILDREN%","CHONY%")
    # deidentified_data_departmentName_filtered_cdw %>% filter(deidentified_key == "Diagseq1902f681")
    logr::log_print("Parsing out drug name")
    drug_parse <- strsplit(deidentified_data_departmentName_filtered_cdw$CODED_VALUE_desc, ":")
    deidentified_data_departmentName_filtered_cdw$drug <- sapply(drug_parse, function(x) trimws(x[2]))
    logr::log_print("Creating a med base to simplify med naming. this likely needs work as it is not currently useful. A master conversion list would be better")
    med_base <- strsplit(deidentified_data_departmentName_filtered_cdw$drug, split = " ")
    deidentified_data_departmentName_filtered_cdw$med_base <- sapply(med_base, function(x) x[1])
    deidentified_data_departmentName_filtered_cdw$med <- deidentified_data_departmentName_filtered_cdw$CODED_VALUE_desc
  }
}, error = function(e) {
  message("An error occurred filtering and creating med base for epic data: ", e$message)
  quit("no", status = 10)
})

logr::log_print("writing results")
tryCatch(
  {
    combo_med_given <- rbind(deidentified_data_departmentName_filtered_epic %>% select(deidentified_key,med_base, med ), deidentified_data_departmentName_filtered_cdw %>% select(deidentified_key,med_base, med ))
    write.table(x = combo_med_given, 
                file = gzfile(here("Intermediate",paste0(time_case_prefix,"meds_given.tsv.gz"))), 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = TRUE, 
                sep = "\t")
  }, error = function(e) {
    message("An error occurred writing results: ", e$message)
    quit("no", status = 10)
  })

tryCatch({
  logr::log_print("in original but not in filtered")
  setdiff(c(unique(deidentified_data_epic$deidentified_key), unique(deidentified_data_cdw$deidentified_key)), unique(combo_med_given$deidentified_key))
}, error = function(e) {
  message("Caught an error closing the log file: ", e$message)
  quit("no", status = 10)
})  

tryCatch({
  logr::log_print("Finished")
  logr::log_close()
  quit()
}, error = function(e) {
  message("Caught an error closing the log file: ", e$message)
  quit("no", status = 10)
})

#debug
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

tryCatch({
  logr::log_print("checking MAR against pgx data")
  if(arguments$star_allele_deidentify_key != "NA"){
    pgx_sample_internal_names_df <- read.table(file = here("Input", arguments$star_allele_deidentify_key_list), header = FALSE)
    length(pgx_sample_internal_names <- (pgx_sample_internal_names_df %>% filter(V1 %in% key_data_unique$deidentified_key))$V1)
    
    setdiff(unique(combo_med_given$deidentified_key), pgx_sample_internal_names)
    setdiff( pgx_sample_internal_names, unique(combo_med_given$deidentified_key))
    setdiff(c(unique(deidentified_data_epic$deidentified_key), unique(deidentified_data_cdw$deidentified_key)), pgx_sample_internal_names)
  }
  
}, error = function(e) {
  message("Caught an error closing the log file: ", e$message)
  quit("no", status = 10)
})

View(deidentified_data_cdw %>% filter(deidentified_key == "Diagseq2813f1063"))
View(deidentified_data_epic %>% filter(deidentified_key == "Diagseq2813f1063"))
