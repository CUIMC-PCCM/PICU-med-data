# *****************************************************************************
# Libraries -------------------------------------------------------------------
# *****************************************************************************
library(tidyverse)
library(janitor)
library(ggplot2)
library(stringr)
library(lubridate)
library(forcats)
library(readxl)
library(writexl)
library(gtsummary)
library(icd)
library(skimr)

# *****************************************************************************
# Definitions -----------------------------------------------------------------
# *****************************************************************************

data_loc <- here('data', 'post_epic_2023_02_13//')

# Max patients to load
max_load <- Inf

fname_demograph <- 'patient_demograph_20230213.txt'
fname_meds <- 'medication_20230213.txt'
fname_race <- 'patient_race_20230213.txt'
fname_insurance <- 'patient_insurance_20230213.txt'
fname_address <- 'patient_address_20230213.txt'
fname_vent <- 'mech_vent_flowsheet_20230213.txt'
fname_labs <- 'lab_values_20230213.txt'
fname_vitals <- 'vitals_CAPD_RASS_20230213.txt'
fname_adt <- 'ADT_events_20230213.txt'
fname_labs <- 'lab results_20230131.txt'
fname_icd <- 'ICD10_codes_20230213.txt'
fname_cpt <- 'CPT_codes_20230213.txt'


# *****************************************************************************
# Load Epic data -------------------------------------------------------------------
# *****************************************************************************

## ****************************************************************************
## Load encounters ------------------------------------------------------------
## ****************************************************************************

# MRN|PAT_ENC_CSN_ID|PAT_NAME|DOB|AGE_AT_ADMIT|SEX|ETHNICITY|ZIP|ADMISSION_DATE|DISCHARGE_DATE

coltypes_enc <- list(
  col_character(),      # MRN
  col_character(),      # PAT_ENC_CSN_ID
  col_skip(),           # PAT_NAME
  col_datetime(),       # DOB
  col_double(),         # AGE_AT_ADMIT
  col_character(),      # SEX
  col_character(),      # ETHNICITY
  col_character(),      # ZIP
  col_datetime(),       # ADMISSION_DATE
  col_datetime()        # DISCHARGE_DATE
)

suppressWarnings(
  {
    df_encounters <- read_delim(paste0(data_loc, fname_demograph), 
                                delim = '|',
                                col_types = coltypes_enc,
                                n_max = max_load
    ) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>%   
      mutate(
        dob = as_date(dob),
        sex = factor(sex, levels = c('male', 'female', 'other')),
        zip = str_pad(zip, 5, side = 'left', pad = '0'),
        ethnicity = factor(ethnicity),
        hospital_admission_datetime = admission_date,
        hospital_admission_date = as_date(admission_date),
        hospital_discharge_datetime = discharge_date,
        hospital_discharge_date = as_date(discharge_date),
        admission_interval = interval(hospital_admission_date, hospital_discharge_date),
        agemonth_admit = floor((hospital_admission_date - dob)/dmonths(1)),
        ageyear_admit = round((hospital_admission_date - dob)/dyears(1), 1)) %>% 
      select(-age_at_admit, -admission_date, -discharge_date) %>% 
      rename(enc_id = pat_enc_csn_id)
  }
)

# Remove last line (sometimes it is a raw text printout)
df_encounters <- df_encounters %>% 
  filter(!str_detect(mrn, 'completion'))

# Create intervals to ensure all events, etc. occurred during the hospitalization
# Note that we need to add 23:59:59 to the discharge time to ensure it doesn't cut off things that
# occurred on the last day
hospital_intervals <- df_encounters %>% select(mrn, enc_id, hospital_admission_date, hospital_discharge_date) %>% 
  distinct() %>% 
  mutate(hosp_interval = interval(hospital_admission_date, hospital_discharge_date + dhours(24)-dseconds(1))) %>% 
  select(-hospital_admission_date, -hospital_discharge_date)

# How many unique patients?
df_encounters %>% distinct(mrn) %>% nrow()

# How many unique encounters?
df_encounters %>% distinct(enc_id) %>% nrow()

## ****************************************************************************
## Load race ------------------------------------------------------------------
## ****************************************************************************

# MRN|ETHNIC_GROUP|PATIENT_RACE|LINE

cols_race <- list(
  col_character(),  # MRN
  col_character(),  # ETHNIC_GROUP
  col_character(),  # PATIENT_RACE
  col_integer()     # LINE
)

suppressWarnings(
  {
    df_race <- read_delim(paste0(data_loc, fname_race), 
                          n_max = max_load,
                          col_types = cols_race
                          ) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>%
      filter(!str_detect(mrn, 'completion')) %>% 
      mutate(
        ethnic_group = factor(ethnic_group),
        patient_race = factor(patient_race)) 
  }
)

# Keep only distinct values
df_race <- df_race %>% distinct()

# Convert to wide format
df_race <- df_race %>% 
  pivot_wider(id_cols = c('mrn', 'ethnic_group'),
              names_from = 'line',
              values_from = 'patient_race',
            names_prefix = 'race')

# Ensure each race designation came from a valid encounter ID
df_race %>% pivot_longer(cols = starts_with('race'),
                         names_to = 'race',
                         values_to = 'value',
                         values_drop_na = TRUE) %>% 
  select(-race) %>% rename(race = value) %>% 
  mutate(ethnic_group = fct_recode(ethnic_group, 'Hispanic/Latino/Spanish' = 'hispanic or latino or spanish origin',
                                   'Not Hispanic/Latino/Spanish' = 'not hispanic or latino or spanish origin',
                                   'Unknown/Declined' = 'unknown',
                                   'Unknown/Declined' = 'declined'),
         race = fct_recode(race, 'AI/AN' = 'american indian or alaska nation',
                           'White' = 'white',
                           'Other' = 'other combinations not described',
                           'Declined' = 'declined',
                           'Black or African-American' = 'black or african american',
                           'Asian' = 'asian',
                           'Hawaiian/Pacific Islander' = 'nat.hawaiian/oth.pacific island')) %>% 
  ggplot(aes(y = race, group = ethnic_group, fill = ethnic_group)) +
  geom_bar(position = position_dodge2(preserve = 'single', padding = 0.2)) +
  theme(legend.position = 'top') +
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'Racial/ethnic demographics',
       y = NULL,
       x = 'Proportion',
       fill = 'Ethnicity') + 
  scale_fill_discrete(breaks=c('Hispanic/Latino/Spanish', 'Not Hispanic/Latino/Spanish', 'Unknown/Declined'))




## ****************************************************************************
## Load address ---------------------------------------------------------------
## ****************************************************************************

suppressWarnings(
  {
    df_address <- read_excel(paste0(data_loc, fname_data), 
                             guess_max = 10000,
                             n_max = max_load, 
                             sheet = sheet_address) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(
        zip = str_pad(zip, 5, side = 'left', pad = '0'),
      ) %>% 
      pivot_wider(id_cols = c('mrn', 'city', 'state', 'zip', 'country'),
                  names_from = 'line',
                  values_from = 'address',
                  names_prefix = 'address_') %>% 
      unite(address, address_1, address_2, address_3, sep = ', ', na.rm = TRUE)
  }
)

df_address %>% tabyl(state) %>% arrange(desc(n)) %>% adorn_pct_formatting()

## ****************************************************************************
## Load insurance -------------------------------------------------------------
## ****************************************************************************

suppressWarnings(
  {
    df_insurance <- read_excel(paste0(data_loc, fname_data), 
                               guess_max = 10000,
                               n_max = max_load, 
                               sheet = sheet_insurance) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(medicaid = if_else(str_detect(payor_name, 'medicaid'), TRUE, FALSE)) %>% 
      group_by(mrn, pat_enc_csn_id) %>% 
      mutate(medicaid = if_else(any(medicaid), TRUE, FALSE)) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = c('mrn', 'pat_enc_csn_id', 'medicaid'),
                  names_from = 'line',
                  values_from = 'payor_name',
                  names_prefix = 'insurance_')  %>% 
      rename(enc_id = pat_enc_csn_id) 
  }
)

df_insurance %>% tabyl(medicaid)
inner_join(df_insurance, df_address) %>% 
  filter(state == 'new york') %>% tabyl(medicaid) 

## ****************************************************************************
## Load ADT -------------------------------------------------------------------
## ****************************************************************************

# Define different location types.
floor_locations <- c('msch 6 tower', 
                     'msch 4 tower', 
                     'msch 5 tower')
step_down_locations <- c('msch 8 central')
nicu_locations <- c('msch 9 north nicu', 
                    'msch 7t nicu', 
                    'zzmsch 7 nicu c', 
                    'zzmsch 7 nicu b',
                    'msch 8 intr icu nursery')
picu_locations <- c('msch 9 central picu',
                    'msch 11 central',
                    'msch 9 tower',
                    'msch 8 intr icu surge')
or_locations <- c('msch operating room',
                  'msch invasive cardiology',
                  'msch endoscopy', 
                  'mil operating room',
                  'mil cardiac cath',
                  'mil neuro ir imaging',
                  'msch 4 north preop pacu')
er_locations <- c('msch emergency',
                  'aln emergency',
                  'nypw emergency')
adult_locations <- c('mil 4 sicu',
                     'mil 5 cticu',
                     'mil 9 hudson')
virtual_locations <- c('msch xray imaging')

#' Load the dataset
#' They key here is to define the department names, and the
#' event types. Department names are classified above.
#' Event types have already been filtered. I believe they have the following numeric types:
#'    1     Admission
#'    2     Discharge
#'    3     Transfer in
#'    4     Transfer out
#'    5/6   "Virtual" events that don't necessitate patient movement, such as for radiology studies
suppressWarnings(
  {
    df_adt <- read_excel(paste0(data_loc, fname_adt), 
                         col_types = c('text',   # mrn
                                       'text',   # pat_enc_csn_id  
                                       'text',   # event_id
                                       'text',   # event_type
                                       'text',   # effective_time
                                       'text',   # department_id
                                       'text',   # department_name
                                       'text',   # pat_class
                                       'text'    # appt_status
                         ),
                         guess_max = 10000,
                         n_max = max_load) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(event_type = case_when(event_type == 'admission' ~ 'admit',
                                    event_type == 'discharge' ~ 'discharge',
                                    event_type == 'transfer in' ~ 'transfer_in',
                                    event_type == 'transfer out' ~ 'transfer_out'),
             department_name = factor(str_to_lower(department_name)),
             appt_status = factor(appt_status),
             adt_date = as_datetime(effective_time)) %>% 
      select(-pat_class, -department_id, -effective_time) %>% 
      rename(enc_id = pat_enc_csn_id, location = department_name) %>% 
      relocate(adt_date, .after = event_id)
  }
)

# Define different levels based on arrays above
df_adt <- df_adt %>% 
  mutate(level_of_care = case_when(location %in% floor_locations ~ 'floor',
                                   location %in% step_down_locations ~ 'stepdown',
                                   location %in% nicu_locations ~ 'nicu',
                                   location %in% picu_locations ~ 'picu',
                                   location %in% or_locations ~ 'or',
                                   location %in% er_locations ~ 'ed',
                                   location %in% adult_locations ~ 'adult',
                                   location %in% virtual_locations ~ 'virtual',
                                   TRUE ~ 'OTHER'),
         level_of_care = factor(level_of_care),
         picu = if_else(level_of_care == 'picu', TRUE, FALSE, FALSE))

# Only keep transfers to or from the ED, floor, PICU locations
# Drop any "virtual" locations, and also any procedural locations like the 
# operating room or endoscopy suite because patients never "stay" in those spots after
# the procedure
df_adt <- df_adt %>% filter(!(location %in% c(virtual_locations, 
                                              or_locations))) %>% 
  mutate(location = fct_drop(location))

# For all ADT, ensure they were taken from within a valid COVID hospitalization
hospital_intervals <- hospital_intervals %>% 
  mutate(enc_id = as.character(enc_id))
df_adt <- inner_join(df_adt, hospital_intervals) %>% 
  filter(adt_date %within% hosp_interval) %>% 
  select(-hosp_interval) %>% 
  distinct()

# Sometimes the first event that occurred isn't listed as an admission. The last
# event is sometimes not a discharge. Re-define the 1st event and last event
# as admit or discharge event.
df_adt <- df_adt %>%
  group_by(mrn, enc_id) %>%
  arrange(adt_date) %>%
  mutate(event_type = if_else(row_number() == 1, 'admit', event_type),
         event_type = if_else(row_number() == n(), 'discharge', event_type),
         event_type = factor(event_type)
  ) %>%
  ungroup()

# Group patients by admission date. Determine when patients changed locations.
# We only need to use admits, discharges, and transfer_in events (can drop all others).
# The strategy is to check, row-by-row for each patient, if the location for the current 
# row matches the location for the prior row. If they match, the patient did not move and 
# we can ultimately drop this row. If they don't match, this involves a change of 
# location.
# Pre-admit refers to the first row, which by definition can't match.
adt2 <- df_adt %>%
  filter(event_type %in% c('admit', 'discharge', 'transfer_in')) %>%
  mutate(location = as.character(location)) %>% 
  group_by(mrn, enc_id) %>%
  arrange(mrn, enc_id, adt_date) %>%
  mutate(last_loc = lag(location),
         last_loc = replace_na(last_loc, 'pre_admit'),
         last_care_level = lag(level_of_care),
         last_care_level = fct_explicit_na(last_care_level, 'pre_admit'),
         remove_this = case_when(event_type == 'admit' ~ FALSE,
                                 event_type == 'discharge' ~ FALSE,
                                 as.character(last_care_level) == as.character(level_of_care) ~ TRUE,
                                 TRUE ~ FALSE))  %>%
  ungroup() %>%
  filter(!remove_this) %>% select(-remove_this)

# Get a value for "last PICU" which was whether the prior row for this patient was a
# PICU hospitalization
# Also flag ED admissions where the next stop was directly to the PICU. (This will be
# useful if we need to use labs from the ED before going to the PICU as if they were a part of the PICU stay)
adt2 <- adt2 %>% group_by(mrn, enc_id) %>%
  mutate(ed_to_picu = if_else(level_of_care == 'ed' & lead(picu), TRUE, FALSE, FALSE),
         last_picu = lag(picu),
         last_picu = replace_na(last_picu, FALSE)) %>%
  ungroup()

# Find the start and stop dates for ICU hospitalizations. There can be multiple per patient
# If a patient was discharged from the ICU, then set this as the stop date
#   Whenever "last_picu" is false and "picu" is true, this is a transition that defines a new PICU course.
#   Whenever "last_picu" is true and "picu" is false, this transition defines the end of a new PICU course.
#     If the last event occurred within a PICU, then this is also the stop of a PICU course.
adt_icu <- adt2 %>%
  group_by(mrn, enc_id) %>%
  arrange(mrn, enc_id, adt_date) %>%
  mutate(icu_start = if_else(!last_picu & picu, TRUE, FALSE, FALSE),
         icu_stop = if_else(!picu & last_picu, TRUE, FALSE, FALSE),
         icu_stop = if_else(picu & row_number() == n(), TRUE, icu_stop, icu_stop),
         any_picu = if_else(any(picu), TRUE, FALSE)) %>%
  ungroup()

# # Save a list of ED patients who were directly admitted to the PICU. We may need this for labs
ed_to_picu_mrn <- adt_icu %>%
  filter(ed_to_picu) %>%
  select(mrn, enc_id, ed_to_picu) %>%
  distinct()

# Create a simple dataset of these ICU events
# the "values_fn = list" just suppresses an annoying warning about duplicates.
# The output format is that each row will have an MRN, an event ID, and an ICU start and the subsequent ICU stop date.
# These windows of time can be used to filter any other results.
# If any event from another table occurred between a given icu_start_date and icu_stop_date, then
# the event happened within an ICU.
# the start and stop dates are formatted as datetimes in case we need to be very specific about timepoints, but can also be
# use the "floor" for the start date, and the "ceiling" for the stop date, to ensure the entire duration is captured.
# the values_fn = list just suppresses an annoying warning about duplicates
adt_icu_simple <- adt_icu %>%
  select(mrn, enc_id, icu_start, icu_stop, adt_date) %>%
  mutate(icu_start_date = if_else(icu_start, adt_date, NA_POSIXct_),
         icu_stop_date = if_else(icu_stop, adt_date, NA_POSIXct_)) %>%
  pivot_longer(cols = c('icu_start_date', 'icu_stop_date'),
               names_to = 'icu_event', values_to = 'icu_event_date') %>%
  filter(!is.na(icu_event_date)) %>%
  select(-icu_start, -icu_stop, -adt_date) %>%
  pivot_wider(id_cols = c('mrn', 'enc_id'),
              names_from = 'icu_event',
              values_from = 'icu_event_date',
              values_fn = list) %>%
  unnest(cols = c('icu_start_date', 'icu_stop_date'))

# Flag any short stays, which we will probably discard
adt_icu_simple <- adt_icu_simple %>% 
  mutate(short_stay = if_else((icu_stop_date - icu_start_date) < dhours(24), TRUE, FALSE)) %>% 
  distinct()

# Show some summary stats
adt_icu_simple %>% filter(short_stay) %>% distinct(mrn) %>% nrow()

adt_icu_simple %>% filter(!short_stay) %>% 
  mutate(dur_icu = (icu_stop_date - icu_start_date)/ddays(1)) %>% 
  summary()

adt_icu_simple %>% filter(!short_stay) %>% 
  mutate(dur_icu = (icu_stop_date - icu_start_date)/ddays(1)) %>% 
  ggplot(aes(x = dur_icu)) + 
  geom_freqpoly(color = 'red', binwidth = 3, center = 1.5)

# Find length of hospital stay for encounters with non-short ICU stays
left_join(hospital_intervals, adt_icu_simple) %>% filter(!short_stay) %>% 
  distinct(mrn, enc_id, .keep_all = TRUE) %>% 
  mutate(hospital_los = as.duration(hosp_interval)/ddays(1)) %>% summary()

# How many distinct patients?
left_join(hospital_intervals, adt_icu_simple) %>% filter(!short_stay) %>% 
  distinct(mrn) %>% nrow()

## *****************************************************************************
## Load ICD --------------------------------------------------------------------
## *****************************************************************************

# Load the dataset
suppressWarnings(
  {
    df_icd <- read_excel(paste0(data_loc, fname_data), 
                         col_types = c('numeric', 'text', 'text', 'date', 'text'),
                         n_max = max_load, 
                         sheet = sheet_icd) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(dx_date = as_date(dx_date),
             dx_type = factor(dx_type),
             icd10 = str_to_upper(icd10),
             pat_enc_csn_id = as.character(pat_enc_csn_id)) %>% 
      rename(enc_id = pat_enc_csn_id) 
  }
)

# Check if all diagnoses were made during a hospitalization
df_icd <- df_icd %>% inner_join(hospital_intervals) %>% 
  filter(dx_date %within% hosp_interval) %>% 
  relocate(mrn) %>% 
  select(-hosp_interval)

# Some rows inexplicably have 2 or more ICD10 codes. Split into new rows
df_icd <- df_icd %>%
  separate_rows(icd10, sep = ',') %>% 
  mutate(icd10 = str_trim(icd10))

# Keep only the first unique combo of MRN/dx per encounter
df_icd <- df_icd %>% 
  arrange(mrn, enc_id, dx_date) %>% 
  distinct(enc_id, icd10, .keep_all = T)

# Most common ICD respiratory diagnoses
n_encounters <- df_icd %>% distinct(enc_id) %>% nrow()
df_icd_ranked <- df_icd %>% 
  add_count(dx_name) %>% arrange(desc(n)) %>% 
  filter(str_detect(icd10, '^J')) %>% 
  mutate(freq = n/n_encounters) %>% 
  distinct(icd10, dx_name, n, freq) %>% 
  mutate(rank = dense_rank(n),
         rank = max(rank) - rank + 1,
         dx_display_name = str_wrap(str_trunc(paste0(icd10, ': ', dx_name), 60), 30),
         dx_display_name = factor(dx_display_name))

# Plot top 20
df_icd_ranked %>% filter(rank < 20) %>% 
  ggplot(aes(y = reorder(dx_display_name, freq), x = freq)) + 
  geom_col() +
  labs(title = 'Top 20 most common respiratory diagnoses',
       x = 'Proportion of encounters with this diagnosis',
       y = NULL)

# Now group as 3-digit categories
df_icd_3digit <- df_icd %>% 
  mutate(icdcat = substr(icd10, 1, 3)) %>% 
  filter(str_detect(icd10, '^J')) %>% 
  distinct(enc_id, icdcat) %>% 
  count(icdcat) %>% arrange(desc(n)) %>% 
  mutate(freq = n/n_encounters) %>% 
  distinct(icdcat, n, freq) %>% 
  mutate(rank = dense_rank(n),
         rank = max(rank) - rank + 1)

# Get descriptions from icd library
df_icd_desc <- explain_table(df_icd_3digit$icdcat) %>% select(icdcat = code, short_desc)
df_icd_3digit <- left_join(df_icd_3digit, df_icd_desc)

# Plot top 10 3-digit categories
df_icd_3digit %>% filter(rank < 10) %>% 
  ggplot(aes(y = reorder(short_desc, freq), x = freq)) + 
  geom_col() +
  labs(title = 'Top 10 most common major ICD categories',
       x = 'Proportion of encounters with this diagnosis',
       y = NULL)

## *****************************************************************************
## Load meds -------------------------------------------------------------------
## *****************************************************************************

# ORDER_MED_ID|LINE|MRN|PAT_ENC_CSN_ID|ORDERING_DATE|MED_NAME|DOSE|DOSE_UNIT|CONCENTRATION|INFUSION_RATE|FREQ_NAME|ROUTE|TAKEN_TIME|COMMENTS|RESULT

# Column types
medfile_coltypes <- list(
                      col_number(),			  # ORDER_MED_ID
                      col_integer(),      # LINE
                      col_character(),		# MRN
                      col_character(),	  # PAT_ENC_CSN_ID
                      col_datetime(),			# ORDERING_DATE
                      col_character(),		# MED_NAME
                      col_double(), 	    # DOSE
                      col_character(),		# DOSE_UNIT
                      col_character(),       # CONCENTRATION
                      col_double(),	      # INFUSION_RATE
                      col_character(),		# FREQ_NAME
                      col_character(),		# ROUTE
                      col_datetime(),			# TAKEN_TIME
                      col_skip(),			    # COMMENTS
                      col_character()			# RESULT
)

suppressWarnings(
  {
    df_meds <- read_delim(paste0(data_loc, fname_meds), 
                               col_types = medfile_coltypes,
                               n_max = max_load, 
                               delim = '|') %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      rename(enc_id = pat_enc_csn_id) %>% 
      slice_head(n=nrow(.)-1)
  }
  
)


## *****************************************************************************
## Load vitals ---------------------------------------------------------------------
## *****************************************************************************

# Load column types
cols_vitals <- list(
  col_character(),    # PAT_ENC_CSN_ID
  col_character(),    # MRN
  col_skip(),         # DISPLAY_NAME
  col_character(),    # FLO_MEAS_NAME
  col_character(),    # MEAS_VALUE
  col_character(),    # UNITS
  col_datetime(),     # RECORDED_TIME
  col_character()    # QUERY_TYPE
)

# Load the dataset
suppressWarnings(
  {
    df_vitals <- read_delim(paste0(data_loc, fname_vitals), 
                          col_types = cols_vitals,
                          delim = '|',
                          n_max = max_load) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      rename(vitals_type = flo_meas_name,
             vitals_time = recorded_time,
             value = meas_value,
             enc_id = pat_enc_csn_id) %>% 
      mutate(vitals_type = case_when(vitals_type == 'blood pressure' ~ 'nibp',
                                     vitals_type == 'r fs map' ~ 'nimap',
                                     vitals_type == 'pulse' ~ 'hr',
                                     vitals_type == 'pulse oximetry' ~ 'spo2',
                                     vitals_type == 'respirations' ~ 'resp',
                                     vitals_type == 'temp source' ~ 'temp source',
                                     vitals_type == 'temperature' ~ 'temp',
                                     vitals_type == 'r fs arterial line blood pressure' ~ 'artbp',
                                     vitals_type == 'r fs map a-line' ~ 'artmap',
                                     vitals_type == 'r fs device cvp mean' ~ 'cvp',
                                     vitals_type == 'r fs richmond agitation sedation scale (rass)' ~ 'rass',
                                     vitals_type == 'r nyc dry (dosing) weight' ~ 'dosing_weight', 
                                     vitals_type == 'nyc ip r are the child\'s actions purposeful?' ~ 'capd_purposeful',
                                     vitals_type == 'nyc ip r delirium screen trigger row' ~ 'capd_trigger',
                                     vitals_type == 'nyc ip r delirium screen score' ~ 'capd_score',
                                     vitals_type == 'nyc ip r does it take the child a long time to respond to interactions?' ~ 'capd_responsetime',
                                     vitals_type == 'nyc ip r does the child communicate needs and wants?' ~ 'capd_communicate',
                                     vitals_type == 'nyc ip r does the child male eye contact with the caregiver?' ~ 'capd_eyecontact',
                                     vitals_type == 'nyc ip r is the child aware of his/her surroundings?' ~ 'capd_aware',
                                     vitals_type == 'nyc ip r is the child inconsolable?' ~ 'capd_inconsolable',
                                     vitals_type == 'nyc ip r is the child restless?' ~ 'capd_restless',
                                     vitals_type == 'nyc ip r is the child underactive = very little movement while awake?' ~ 'capd_underactive',
                                     TRUE ~ vitals_type)) 
  }
)

# Get a dataset of dosing weights
df_weights <- df_vitals %>% 
  filter(vitals_type == 'dosing_weight') %>% 
  select(mrn, enc_id, dosing_weight = value, units, doseweight_time = vitals_time) %>% 
  mutate(dosing_weight = as.double(dosing_weight)/1000) %>% 
  select(-units)

# Get a rass dataset for later use
df_rass <- df_vitals %>% 
  filter(vitals_type == 'rass') %>% 
  mutate(rass = case_when(value == 'unarousable' ~ -5,
                          value == 'deep sedation' ~ -4,
                          value == 'moderate sedation' ~ -3,
                          value == 'light sedation' ~ -2,
                          value == 'drowsy' ~ -1,
                          value == 'alert and calm' ~ 0,
                          value == 'restless' ~ 1,
                          value == 'agitated' ~ 2,
                          value == 'very agitated' ~ 3,
                          value == 'combative' ~ 4),
         rass = as.integer(rass)) %>% 
  select(mrn, enc_id, rass, rass_time = vitals_time) 

# Get a capd dataset for later use
df_capd <- df_vitals %>% 
  filter(str_detect(vitals_type, 'capd')) %>%
  filter(vitals_type != 'capd_trigger') %>% 
  rename(capd_type = vitals_type) %>% 
  mutate(capd_numeric = case_when(
    str_detect(capd_type, 'purposeful|communicate|aware|eyecontact') & 
      value == 'never' ~ 4,
    str_detect(capd_type, 'purposeful|communicate|aware|eyecontact') & 
      value == 'rarely' ~ 3,
    str_detect(capd_type, 'purposeful|communicate|aware|eyecontact') & 
      value == 'sometimes' ~ 2,
    str_detect(capd_type, 'purposeful|communicate|aware|eyecontact') & 
      value == 'often' ~ 1,
    str_detect(capd_type, 'purposeful|communicate|aware|eyecontact') & 
      value == 'always' ~ 0,
    str_detect(capd_type, 'restless|inconsolable|underactive|responsetime') & 
      value == 'never' ~ 0,
    str_detect(capd_type, 'restless|inconsolable|underactive|responsetime') & 
      value == 'rarely' ~ 1,
    str_detect(capd_type, 'restless|inconsolable|underactive|responsetime') & 
      value == 'sometimes' ~ 2,
    str_detect(capd_type, 'restless|inconsolable|underactive|responsetime') & 
      value == 'often' ~ 3,
    str_detect(capd_type, 'restless|inconsolable|underactive|responsetime') & 
      value == 'always' ~ 4,
    capd_type == 'capd_score' ~ as.numeric(value))) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'capd_type',
              values_from = capd_numeric) %>% 
  mutate(capd_calc = rowSums(across(c(capd_responsetime, capd_communicate, capd_eyecontact,
                            capd_aware, capd_inconsolable, capd_restless, capd_underactive)),
                            na.rm = T)) %>% 
  select(mrn, enc_id, capd_time = vitals_time, capd_score, capd_calc)
  
# Remove blank values
df_vitals <- df_vitals %>% filter(!(value == 'null' | is.na(value)))

# Create a separate dataset for temperatures, where the name of the vital
# sign inclues the measurement modality
# Remove NA values, and convert all values to celsius
df_temp <- df_vitals %>% filter(vitals_type %in% c('temp', 'temp source')) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = 'value') %>% 
  clean_names() %>% 
  mutate(temp = as.double(temp),
         temp = if_else(temp > 50, round((temp-32)*5/9, 1), temp),
         temp_source = replace_na(temp_source, 'unknown')) %>% 
  filter(!is.na(temp)) 

# Separate BP into systolic, diastolic
df_bp <- df_vitals %>% 
  filter(vitals_type %in% c('nibp', 'artbp')) %>% 
  separate(col = 'value', sep = '/', into = c('systolic', 'diastolic')) %>% 
  filter(!(systolic == 'NULL' | diastolic == 'null' | is.na(systolic) | is.na(diastolic))) %>% 
  mutate(systolic = as.integer(systolic),
         diastolic = as.integer(diastolic)) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = c('systolic', 'diastolic'),
              names_glue = '{vitals_type}_{.value}') %>% 
  relocate(artbp_systolic, artbp_diastolic, nibp_systolic, nibp_diastolic, .after = 'vitals_time')

# Now get MAP values
df_bpmap <- df_vitals %>% 
  filter(vitals_type %in% c('nimap', 'artmap')) %>% 
  pivot_wider(id_cols  = c('mrn', 'enc_id', 'vitals_time'),
              names_from = vitals_type,
              values_from = value) %>% 
  rename(artbp_map = artmap, nibp_map = nimap)

# Combine into a wide datset
df_bp <- full_join(df_bp, df_bpmap) %>% 
  relocate(artbp_map, .after = artbp_diastolic) %>% 
  relocate(nibp_map, .after = nibp_diastolic)

# Create a wider dataset of vitals with only 1 source, 
# and that do not need any processing
df_vitals <- df_vitals %>% 
  filter(vitals_type %in% c('hr', 'spo2', 'cvp', 'resp')) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = 'value')

# Bind BP 
df_vitals <- full_join(df_vitals, df_bp)


## *****************************************************************************
## Load labs ---------------------------------------------------------------------
## *****************************************************************************


# Load the dataset
suppressWarnings(
  {
    df_labs <- read_delim(paste0(data_loc, fname_labs), 
                          col_types = 'cnicTTTccccc',
                          n_max = max_load,
                          delim = '|') %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower))
  }
)

# Remove blank values
df_vitals <- df_vitals %>% filter(!(value == 'null' | is.na(value)))

# Create a separate dataset for temperatures, where the name of the vital
# sign inclues the measurement modality
# Remove NA values, and convert all values to celsius
df_temp <- df_vitals %>% filter(vitals_type %in% c('temp', 'temp source')) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = 'value') %>% 
  mutate(temp = as.double(temp),
         temp = if_else(temp > 50, round((temp-32)*5/9, 1), temp)) %>% 
  filter(!is.na(temp)) 

# Separate BP into systolic, diastolic, and MAP
df_bp <- df_vitals %>% 
  filter(vitals_type %in% c('nibp', 'artbp')) %>% 
  separate(col = 'value', sep = '/', into = c('systolic', 'diastolic')) %>% 
  filter(!(systolic == 'NULL' | diastolic == 'null' | is.na(systolic) | is.na(diastolic))) %>% 
  mutate(systolic = as.integer(systolic),
         diastolic = as.integer(diastolic)) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = c('systolic', 'diastolic'),
              names_glue = '{vitals_type}_{.value}') %>% 
  relocate(artbp_systolic, artbp_diastolic, nibp_systolic, nibp_diastolic, .after = 'vitals_time')

# Create a wider dataset of vitals with only 1 source, 
# and that do not need any processing
df_vitals <- df_vitals %>% 
  filter(vitals_type %in% c('hr', 'spo2', 'cvp', 'resp')) %>% 
  pivot_wider(id_cols = c('mrn', 'enc_id', 'vitals_time'),
              names_from = 'vitals_type',
              values_from = 'value')

# Bind BP 
df_vitals <- full_join(df_vitals, df_bp)

# Remove temps from the vitals
df_vitals <- df_vitals %>% 
  filter(!(vitals_type %in% c('temp', 'temp source'))) %>% 
  mutate(vitals_type = fct_drop(vitals_type))

# *****************************************************************************
# Load Pre-Epic data -------------------------------------------------------------------
# *****************************************************************************

## ****************************************************************************
## Load encounters ------------------------------------------------------------
## ****************************************************************************

suppressWarnings(
  {
    df_encounters <- read_excel(paste0(data_loc, fname_data), 
                                guess_max = 10000,
                                n_max = max_load, 
                                sheet = sheet_encounter) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(
        pat_enc_csn_id = as.character(pat_enc_csn_id),
        dob = as_date(dob),
        sex = factor(sex, levels = c('male', 'female', 'other')),
        zip = str_pad(zip, 5, side = 'left', pad = '0'),
        ethnicity = factor(ethnicity),
        hospital_admission_datetime = as_datetime(admission_date),
        hospital_admission_date = as_date(admission_date),
        hospital_discharge_datetime = as_datetime(discharge_date),
        hospital_discharge_date = as_date(discharge_date),
        admission_interval = interval(hospital_admission_date, hospital_discharge_date),
        agemonth_admit = floor((hospital_admission_date - dob)/dmonths(1))
      ) %>% 
      separate(pat_name, into = c('lastname', 'firstname'), sep = ',') %>% 
      rename(enc_id = pat_enc_csn_id) %>% 
      select(-age_at_admit, -admission_date, -discharge_date)
  }
)

# Create intervals to ensure all events, etc. occurred during the hospitalization
# Note that we need to add 23:59:59 to the discharge time to ensure it doesn't cut off things that
# occurred on the last day
hospital_intervals <- df_encounters %>% select(mrn, enc_id, hospital_admission_date, hospital_discharge_date) %>% 
  distinct() %>% 
  mutate(hosp_interval = interval(hospital_admission_date, hospital_discharge_date + dhours(24)-dseconds(1))) %>% 
  select(-hospital_admission_date, -hospital_discharge_date)

# How many unique?
df_encounters %>% distinct(mrn) %>% nrow()

## *****************************************************************************
## Load ADT --------------------------------------------------------------------
## *****************************************************************************


#' They key here is to define the department names, and the
#' event types. Department names are classified above.
#' Event types have already been filtered. I believe they have the following numeric types:
#'    1     Admission
#'    2     Discharge
#'    3     Transfer in
#'    4     Transfer out
#'    5/6   "Virtual" events that don't necessitate patient movement, such as for radiology studies
suppressWarnings(
  {
    df_adt_pre <- read_delim(paste0(data_loc_pre, fname_adt_pre), 
                             col_types = list(MRN = col_character(), 
                                              ENCOUNTER_NUMBER = col_character(),
                                              LOCATION_CODE = col_character(),
                                              ROOM = col_skip(),
                                              BED = col_skip(),
                                              MEDICAL_SERVICE_CODE = col_character(),
                                              EVENT_TIME = col_datetime()
                             ), 
                             delim = '|',
                             # guess_max = 10000,
                             n_max = max_load) %>%
      clean_names() %>% 
      mutate(across(where(is.character), str_to_lower)) %>% 
      mutate(event_type = case_when(event_type == 'admission' ~ 'admit',
                                    event_type == 'discharge' ~ 'discharge',
                                    event_type == 'transfer in' ~ 'transfer_in',
                                    event_type == 'transfer out' ~ 'transfer_out'),
             department_name = factor(str_to_lower(department_name)),
             appt_status = factor(appt_status),
             adt_date = as_datetime(effective_time)
      ) %>% 
      select(-pat_class, -department_id, -effective_time) %>% 
      rename(enc_id = pat_enc_csn_id, location = department_name) %>% 
      relocate(adt_date, .after = event_id)
  }
)

# *****************************************************************************
# Not currently used ----------------------------------------------------------
# *****************************************************************************
mutate(hospital_admission_date = as_date(hosp_admsn_time),
       hosp_admission_datetime = as_datetime(hosp_admsn_time),
       hosp_disch_time = as_datetime(hosp_disch_time),
       hosp_disch_time = if_else(is.na(hosp_disch_time),               # set blank times to some long future date
                                 as_datetime('2030-01-01 00:00:01'), 
                                 hosp_disch_time),
       hospital_discharge_date = as_date(hosp_disch_time),
       hosp_discharge_datetime = as_datetime(hosp_disch_time),
       dob = as_date(birth_date),
       sex = factor(sex),
       race = factor(race),
       race = fct_recode(race, White =     'WHITE',
                         AIAN =      'AMERICAN INDIAN OR ALASKA NATION',
                         Asian =     'ASIAN',
                         Black =     'BLACK OR AFRICAN AMERICAN',
                         Declined =  'DECLINED',
                         Other =     'OTHER COMBINATIONS NOT DESCRIBED'),
       ethnicity = factor(str_to_title(ethnicity)),
       ethnicity = fct_recode(ethnicity, 
                              'Hispanic/Latino' = 'Hispanic Or Latino Or Spanish Origin',
                              'Not Hispanic/Latino' = 'Not Hispanic Or Latino Or Spanish Origin'),
       height = round(as.double(height) * 2.54, 1), # convert to cm
       weight = round(as.double(weight)/2.2, 3), #convert to kg
       covid_test_type = factor(description),
       covid_test_date = as_date(specimn_taken_time),
       covid_test_result = factor(ord_value),
       hospital_dispo = factor(hospital_disposition),
       hospital_dispo = fct_recode(hospital_disposition, transfer = 'Acute / Short Term Hospital',
                                   transfer = 'Cancer Center/Children\'s Hospital',
                                   transfer = 'Cancer Center/Children\'s Hospital w/ Planned Readmission',
                                   died = 'Expired',
                                   home = 'Home-Health Care Svc',
                                   home = 'Home or Self Care',
                                   hospice = 'Hospice/Medical Facility',
                                   rehab = 'Interim / Custodial Care Facility',
                                   psych = 'Psychiatric Hospital',
                                   rehab = 'Rehab Facility',
                                   long_term = 'Skilled Nursing Facility',
                                   unknown = 'NULL')
) %>% 
  group_by(mrn) %>% 
  mutate(hosp_num = dense_rank(hospital_admission_date)) %>% ungroup() %>% 
  select(mrn, hosp_num, hospital_admission_date, hosp_admission_datetime, hospital_discharge_date, hosp_discharge_datetime,
         hospital_dispo, dob, sex, race, ethnicity, height, weight, covid_test_type, covid_test_date, covid_test_result)
