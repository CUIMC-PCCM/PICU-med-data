library(tidyverse)
library(data.table)
library(here)
library(docopt)
"%!in%" <- Negate("%in%")

epic_data <- fread(here("Input","RITM0429582_V1_epicVisitAdtMar_complete.txt.gz"),quote = "")
epic_data$EMPI_char <- as.character(epic_data$EMPI)
cdw_data_1 <- fread(here("Input","RITM0429582_V1_cdwRx.txt.gz"), fill = TRUE, sep = "\t")
cdw_data_1_csv <- read.csv(here("Input","RITM0429582_V1_cdwRx.txt.gz"), blank.lines.skip = TRUE)
cdw_data_12 <- read_csv(here("Input","RITM0429582_V1_cdwRx.txt.gz"))
cdw_data_13 <- read.delim(here("Input","RITM0429582_V1_cdwRx.txt.gz"), stringsAsFactor = FALSE)
temp <- cdw_data_13[900000:910000,]
cdw_data_1$EMPI_char <- as.character(cdw_data_1$EMPI)
cdw_data_11$EMPI_char <- as.character(cdw_data_11$EMPI)
cdw_data_2 <- fread(here("Input","RITM0429582_V1_cdwVisitDetailForRx.txt.gz"), fill = TRUE)
# cdw_data_2 <- read.csv(here("Input","RITM0429582_V1_cdwVisitDetailForRx.txt.gz"))
cdw_data_2$EMPI_char <- as.character(cdw_data_2$EMPI)
cdw_data_14 <- read.delim(here("Input","RITM0429582_V1_cdwRx.txt.gz"), stringsAsFactor = FALSE, quote = "")
cdw_data_15 <- readLines(here("Input","RITM0429582_V1_cdwRx.txt.gz"))

cdw_data_15[17]

cdw_data_12$`EMPI	PRIMARY_TIME	ORGANIZATION	ORGANIZATION_desc	EVENT_CODE	EVENT_name	EVENT_STATUS	LOCATION	COMP_NUMBER	PARENT	COMP_CODE	COMP_name	VALUE_TYPE	NUM_VALUE	CODED_VALUE	CODED_VALUE_desc	CHAR_VALUE	ACCOUNT	ADMIT_DATE	DISCHARGE_DATE	HOSPITAL_ID	HOSPITAL_DESC	LOCATION_CODE	LOCATION_DESC`[906049]
cdw_data_12$`EMPI	PRIMARY_TIME	ORGANIZATION	ORGANIZATION_desc	EVENT_CODE	EVENT_name	EVENT_STATUS	LOCATION	COMP_NUMBER	PARENT	COMP_CODE	COMP_name	VALUE_TYPE	NUM_VALUE	CODED_VALUE	CODED_VALUE_desc	CHAR_VALUE	ACCOUNT	ADMIT_DATE	DISCHARGE_DATE	HOSPITAL_ID	HOSPITAL_DESC	LOCATION_CODE	LOCATION_DESC`[906051]
cdw_data_12$`EMPI	PRIMARY_TIME	ORGANIZATION	ORGANIZATION_desc	EVENT_CODE	EVENT_name	EVENT_STATUS	LOCATION	COMP_NUMBER	PARENT	COMP_CODE	COMP_name	VALUE_TYPE	NUM_VALUE	CODED_VALUE	CODED_VALUE_desc	CHAR_VALUE	ACCOUNT	ADMIT_DATE	DISCHARGE_DATE	HOSPITAL_ID	HOSPITAL_DESC	LOCATION_CODE	LOCATION_DESC`[906052]
cdw_data_12$`EMPI	PRIMARY_TIME	ORGANIZATION	ORGANIZATION_desc	EVENT_CODE	EVENT_name	EVENT_STATUS	LOCATION	COMP_NUMBER	PARENT	COMP_CODE	COMP_name	VALUE_TYPE	NUM_VALUE	CODED_VALUE	CODED_VALUE_desc	CHAR_VALUE	ACCOUNT	ADMIT_DATE	DISCHARGE_DATE	HOSPITAL_ID	HOSPITAL_DESC	LOCATION_CODE	LOCATION_DESC`[906053]
cdw_data_12$`EMPI	PRIMARY_TIME	ORGANIZATION	ORGANIZATION_desc	EVENT_CODE	EVENT_name	EVENT_STATUS	LOCATION	COMP_NUMBER	PARENT	COMP_CODE	COMP_name	VALUE_TYPE	NUM_VALUE	CODED_VALUE	CODED_VALUE_desc	CHAR_VALUE	ACCOUNT	ADMIT_DATE	DISCHARGE_DATE	HOSPITAL_ID	HOSPITAL_DESC	LOCATION_CODE	LOCATION_DESC`[906054]
cdw_data_11[906051,24]
cdw_data_11[906052,24]
cdw_data_11[906053,24]
cdw_data_11[906054,24]
cdw_data_11[906055,24]

sort(unique(cdw_data_1$LOCATION_DESC))
sort(unique(cdw_data_1$LOCATION_CODE))
sort(unique(cdw_data_2$LOCATION_DESC))

temp_list <-lapply(cdw_data_15, function(x) strsplit(x, "\t"))
numbers_tab <- sapply(temp_list,function(x) length(x[[1]]))
temp_df <- do.call(rbind, temp_list)

cdw_data_2_picu <- cdw_data_2 %>% 
  filter(LOCATION_DESC %in% c("CHONY 9 PEDIATRIC ICU", "CHONY 9 TOWER","CHIDRENS 11 (BH9 PICU SATELLIT","CHONY 9 CENTRAL FORMER B09S"))


nrow(picu_ids <- as_tibble(fread(here("Input","picu_all_list.txt"), header = FALSE)))

deidentified_data <- as_tibble(fread(here("Intermediate","2023_05_08_10_07_24_PGX_deidentified_data.tsv.gz")))

meds_given <- as_tibble(fread(here("Intermediate","2023_05_08_09_27_14_PGX_meds_given.tsv.gz"))) %>% arrange(med_base)

med_counts <- meds_given %>% group_by(med_base) %>% summarize(count_var = n()) %>% arrange(desc(count_var))

write.csv(x = med_counts, file = here("Intermediate","2023_05_08_09_27_14_PGx_meds_given_counts.csv"),quote = FALSE, row.names = FALSE)

length(in_picu_but_not_deidentified_data <- picu_ids$V1[tolower(picu_ids$V1) %!in% tolower(deidentified_data$deidentified_key)])
length(in_picu_and_deidentified_data <- picu_ids$V1[tolower(picu_ids$V1) %in% tolower(deidentified_data$deidentified_key)])
length(in_picu_deidentified_data_meds_given <- in_picu_and_deidentified_data[tolower(in_picu_and_deidentified_data) %in% tolower(meds_given$deidentified_key)])
length(in_picu_deidentified_data_not_meds_given <- in_picu_and_deidentified_data[tolower(in_picu_and_deidentified_data) %!in% tolower(meds_given$deidentified_key)])

nrow(deidentified_data_picu_but_not_given <- deidentified_data %>% filter(tolower(deidentified_key) %in% tolower(in_picu_deidentified_data_not_meds_given)))
sort(unique(deidentified_data_picu_but_not_given$visit_departmentName))
sort(unique(deidentified_data_picu_but_not_given$ADT_departmentName))

nrow(pre_epic_example_1 <- cdw_data_1 %>% filter(EMPI_char == "1102606022"))
nrow(pre_epic_example_1 <- cdw_data_11 %>% filter(EMPI_char == "1102606022"))
nrow(pre_epic_example_2 <- cdw_data_2 %>% filter(EMPI_char == "1102606022"))


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

epic_data_1 <- fread(here("Input","RITM0429582_V2","RITM0429582_V2_epicVisitAdtMar_complete.txt"), fill = TRUE, sep = "|")
cdw_data_1 <- fread(here("Input","RITM0429582_V2","RITM0429582_V2_cdwRx.txt"), fill = TRUE, sep = "|")
cdw_data_111 <- readLines(here("Input","RITM0429582_V2","RITM0429582_V2_cdwRx.txt"))
cdw_data_11 <- read.csv(here("Input","RITM0429582_V2","RITM0429582_V2_cdwRx.txt"))
cdw_data_1111 <- read.table(here("Input","RITM0429582_V2","RITM0429582_V2_cdwRx.txt"), header = TRUE, sep = "|", quote = "", as.is = TRUE, fill = TRUE)
cdw_data_1 <- read.table(here("Input","RITM0429582_V1_cdwRx.txt.gz"), header = TRUE, sep = "\t", quote = "", as.is = TRUE, fill = TRUE)

cdw_data_1_picu <- cdw_data_1 %>% filter(LOCATION_DESC %in% c("CHONY 9 PEDIATRIC ICU", "CHONY 9 TOWER","CHIDRENS 11 (BH9 PICU SATELLIT","CHONY 9 CENTRAL FORMER B09S","CHILDRENS TRANSFER UNIT"))

cdw_data_1 %>% group_by(LOCATION_DESC) %>% summarize(countVar = n())
cdw_data_1_picu %>% group_by(LOCATION_DESC) %>% summarize(countVar = n())
nrow(cdw_data_1)

cdw_data_11[1]
cdw_data_11[2]
cdw_data_11[290385]
cdw_data_11[290386]
cdw_data_11[290387]

temp_list <-lapply(cdw_data_11, function(x) strsplit(paste0(x, " "), "\\|"))
numbers_tab <- sapply(temp_list[[1]],function(x) length(x))
data_df <- do.call(rbind,temp_list)
sort(unique(numbers_tab))

nrow(temp <- cdw_data_1112 %>% filter(EMPI == 1102606022))
sort(unique(temp$LOCATION_DESC))
sort(unique(cdw_data_1112$LOCATION_DESC))



parsed_data_index <- 1:length(cdw_data_11)

parsed_data_list <- lapply(parsed_data_index[parsed_data_index %!in% bad_rows], function(x) strsplit(paste0(cdw_data_11[x], " "), "\\|"))

cdw_data_2b <- read.table(here("Input","RITM0429582_V2B","RITM0429582_V2B_cdwRx.txt"), header = TRUE, sep = "|", quote = "", as.is = TRUE, fill = TRUE)
cdw_data_2b <- read.table(here("Input","RITM0429582_V2B","RITM0429582_V2B_cdwRx.txt"), header = TRUE, sep = "|", quote = "", fill = TRUE)
cdw_data_2c <- readLines(here("Input","RITM0429582_V2B","RITM0429582_V2B_cdwRx.txt"))
nrow(temp <- cdw_data_2b %>% filter(EMPI == 1102606022))
