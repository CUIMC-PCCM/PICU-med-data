# create_deidentify_key.R: creates key

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
time_case_prefix <- paste0(gsub(":","_",  gsub(" ","_", gsub("-","_",Sys.time()))), "_")

igm_redcap <- fread(here("Input","IGMGoldsteinPatientT-CHONYPICUmanual_DATA_LABELS_2023-09-13_2048.csv"))
diagseq <- sprintf("Diagseq%sf%s",igm_redcap$`What is the DiagSeq Study ID?`,igm_redcap$`What is the DiagSeq Family ID?`)
neuro <- sprintf("Neuro%sf%s",igm_redcap$`What is the Neuro Study ID?`,igm_redcap$`What is the Neuro Family ID?`)
igm_redcap$sample_internal_name <- ifelse(neuro != "NeuroNAfNA",neuro,diagseq)
write.csv(igm_redcap %>% select(identified_key = `Epic MRN`, deidentified_key = sample_internal_name), file = here("Intermediate",paste0(time_case_prefix, "identity_key.csv")), quote = FALSE, row.names = FALSE)
