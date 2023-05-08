library(tidyverse)
library(data.table)
library(here)
library(docopt)
"%!in%" <- Negate("%in%")

epic_data <- fread(here("Input","RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"))
epic_data$EMPI_char <- as.character(epic_data$EMPI)
cdw_data_1 <- fread(here("Input","RITM0429582_V1_cdwRx.txt.gz"))
cdw_data_1$EMPI_char <- as.character(cdw_data_1$EMPI)
cdw_data_2 <- fread(here("Input","RITM0429582_V1_cdwVisitDetailForRx.txt.gz"))
cdw_data_2$EMPI_char <- as.character(cdw_data_2$EMPI)

sort(unique(cdw_data_1$LOCATION_DESC))
sort(unique(cdw_data_1$LOCATION_CODE))
sort(unique(cdw_data_2$LOCATION_DESC))

cdw_data_2_picu <- cdw_data_2 %>% 
  filter(LOCATION_DESC %in% c("CHONY 9 PEDIATRIC ICU", "CHONY 9 TOWER","CHIDRENS 11 (BH9 PICU SATELLIT","CHONY 9 CENTRAL FORMER B09S"))


nrow(picu_ids <- as_tibble(fread(here("Input","picu_all_list.txt"), header = FALSE)))

deidentified_data <- as_tibble(fread(here("Intermediate","2023_05_08_10_07_24_PGX_deidentified_data.tsv.gz")))

meds_given <- as_tibble(fread(here("Intermediate","2023_05_08_09_27_14_PGX_meds_given.tsv.gz")))

length(in_picu_but_not_deidentified_data <- picu_ids$V1[tolower(picu_ids$V1) %!in% tolower(deidentified_data$deidentified_key)])
length(in_picu_and_deidentified_data <- picu_ids$V1[tolower(picu_ids$V1) %in% tolower(deidentified_data$deidentified_key)])
length(in_picu_deidentified_data_meds_given <- in_picu_and_deidentified_data[tolower(in_picu_and_deidentified_data) %in% tolower(meds_given$deidentified_key)])
length(in_picu_deidentified_data_not_meds_given <- in_picu_and_deidentified_data[tolower(in_picu_and_deidentified_data) %!in% tolower(meds_given$deidentified_key)])

nrow(deidentified_data_picu_but_not_given <- deidentified_data %>% filter(tolower(deidentified_key) %in% tolower(in_picu_deidentified_data_not_meds_given)))
sort(unique(deidentified_data_picu_but_not_given$visit_departmentName))
sort(unique(deidentified_data_picu_but_not_given$ADT_departmentName))

nrow(pre_epic_example_1 <- cdw_data_1 %>% filter(EMPI == "1102606022"))
nrow(pre_epic_example_2 <- cdw_data_2 %>% filter(EMPI == "1102606022"))


identify_key <- fread(here("Input","identify_key.csv"))
nrow(identify_key)
identify_key$identified_key_char <- as.character(identify_key$identified_key)
nrow(keys_in_epic_data <- identify_key %>% filter(identified_key_char %in% epic_data$EMPI_char))
nrow(keys_in_cdw_1 <- identify_key %>% filter(identified_key_char %in% cdw_data_1$EMPI_char))
nrow(keys_in_cdw_2 <- identify_key %>% filter(identified_key_char %in% cdw_data_2$EMPI_char))
length(mrns_in_cdw_1_not_cdw_2 <- unique(cdw_data_1$EMPI_char[cdw_data_1$EMPI_char %!in% cdw_data_2$EMPI_char]))
length(mrns_in_cdw_2_not_cdw_1 <- unique(cdw_data_2$EMPI_char[cdw_data_2$EMPI_char %!in% cdw_data_1$EMPI_char]))


in_patient_from_cdw_2 <- cdw_data_2 %>% filter(PATIENT_CLASS_DESCRIPTION == "In-patient")
length(in_patient_from_cdw_2_ids <- sort(unique(in_patient_from_cdw_2$EMPI)))
length(mrns_in_cdw_2_not_cdw_1_inpatient <- unique(in_patient_from_cdw_2_ids[in_patient_from_cdw_2_ids %!in% cdw_data_1$EMPI_char]))
