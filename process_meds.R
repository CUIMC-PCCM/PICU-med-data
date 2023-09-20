
mar_meds_invalid <- c('anesthesia volume adjustment',
                      'automatically held', 
                      'canceled entry', 
                      'due', 
                      #'held', 
                      #'held by provider', 
                      #'mar hold', 
                      #'mar unhold',
                      'medication removed',
                      'missed', 
                      'not given', 
                      'pending', 
                      'refused', 
                      'return to cabinet',
                      'see alternative',
                      #'stopped (dual sign required)',
                      'unheld by provider',
                      'verification')

mar_meds_stopped <- c('held', 
                      'held by provider',
                      'mar hold', 
                      'stopped (dual sign required)',
                      'stopped',
                      'stop infusion')

mar_meds_changed <- c('rate change',
                      'rate/dose change',
                      'rate/dose changed')

# Small sample for ease of use
meds_sample <- df_meds %>% arrange(mrn, enc_id, taken_time) %>% 
  slice_head(n = 10000) 

# Rename a few things
meds_sample <- df_meds %>% 
  rename(med = med_name, 
         mar_result = result,
         units = dose_unit,
         frequency = freq_name,
         med_time = taken_time) %>% 
  mutate(infusion_rate = if_else(infusion_rate != 'NULL', as.numeric(infusion_rate), NaN)) %>% 
  select(-line)

# Make sure that only med doses are taken from within the COVID hospitalization 
# (use the "mrn_valid_interval" dataset, which has hospital intervals)
meds_sample <- meds_sample %>% left_join(hospital_intervals) %>% 
  filter(med_time %within% hosp_interval) %>%
  select(-hosp_interval) 

# Clean up the data, remove any rows that indicate a med was ordered but not given
meds <- meds_sample %>% 
  # group_by(enc_id) %>% 
  # mutate(hosp_num = dense_rank(enc_id)) %>% ungroup() %>% 
  # select(mrn,
  #        hosp_num,
  #        enc_id,
  #        med,
  #        med_time, 
  #        frequency,
  #        dose = sig, # the "dose" column is the ordered dose. sig is the actual dose that was given. rename for consistency.
  #        units,
  #        infusion_rate,
  #        mar_result) %>% 
  mutate(mar_result = factor(mar_result),
         dose = as.numeric(dose)) %>% 
  filter(!(mar_result %in% mar_meds_invalid)) %>% 
  filter(!is.na(dose))

# Get concentrations. Define this as something matching the format "mg/ml" or X mg / Y ml"), then
# process into a numeric value (mg, units, mcg per mL). Some of the premixed bags
# need to just be customized.
meds <- meds %>% 
  mutate(med = str_to_lower(med),
         conc = str_match(med, '[:digit:]*\\.*[:digit:]+[:space:]*mg\\/[:digit:]*ml|[:digit:]*\\.*+[:space:]*mcg\\/[:digit:]*ml|[:digit:]*\\.*+[:space:]*units\\/[:digit:]*ml'),
         conc = if_else(str_detect(med, 'fentanyl') & str_detect(med, 'premix'),
                        '20 mcg/ml', conc),
         conc = str_remove_all(conc, '\\('),
         conc = str_remove_all(conc, '\\)'),
         conc_old = conc,
         conc_unit = str_remove_all(conc, '[:digit:]|\\.') %>% str_remove_all('\\/.+') %>% str_trim(),
         conc = str_remove_all(conc, '[:alpha:]| '),
         conc = str_replace(conc, '\\/$', '\\/1')) %>% 
  separate(col = conc, c('conc_num', 'conc_denom'), sep = '/') %>% 
  mutate(conc_num = as.double(conc_num),
         conc_denom = as.double(conc_denom),
         conc = conc_num / conc_denom,
         conc = if_else(conc_unit == 'mcg', conc / 1000, conc)) %>% 
  select(-conc_num, -conc_denom)

# Filter out fluids, fluid boluses
meds_cleaned <- meds %>% 
  mutate(med = str_to_lower(med),
         med = str_remove_all(med, ','),
         med = str_remove_all(med, '\\(pf\\)'),
         med = str_remove_all(med, 'INV \\(GEN\\)'),
         med = str_remove_all(med, ' \\(lip-prot-amyl\\)'),
         med = str_remove_all(med, '\\(laxative\\) \\(child\\) '),
         med = str_remove_all(med, '\\(urethral/mucosal\\) sterile '),
         med = str_remove_all(med, '\\(adult\\)'),
         med = str_remove_all(med, '\\(human\\)'),
         med = str_remove_all(med, '\\(pediatric\\)'),
         med = str_remove_all(med, '\\(cardiac\\)'),
         med = str_remove_all(med, '\\(l-arginine\\)'),
         med = str_remove_all(med, '\\(5 mg/ml\\)'),
         med = str_remove_all(med, 'in nacl'),
         #med = str_remove_all(med, 'in sodium chloride [:digit:]+ %'),
         med = str_remove_all(med, 'in sodium chloride'),
         med = str_remove_all(med, 'in dextrose'),
         med = str_remove_all(med, 'sodium succinate'),
         med = str_remove_all(med, 'sod suc'),
         med = str_remove_all(med, ' pf '),
         med = str_remove_all(med, 'concentrate'),
         med = str_replace_all(med, 'morphine', 'morphine \\(morphine\\)'),
         med = str_replace_all(med, '\\(human\\) \\(gammagard s/d\\)', '(\\gammagard\\)'),
         med = str_replace_all(med, '\\(isophane\\) \\(humulin n\\)', '(\\isophane humulin n\\)'),
         med = str_replace_all(med, '\\(isophane\\) \\(humulinnovolin\\)', '(\\isophane humulin novolin\\)'),
         med = str_replace_all(med, 'epinephrine (epipen)', 'epipen (epipen)'),
         med = str_remove_all(med, '[:space:]{2,}'),
         med = str_remove_all(med, ' sodium')) %>% 
  mutate(units = str_remove_all(units, 'of [:alpha:]+')) %>% 
  filter(!str_detect(med, 'lipid emulsion')) %>% 
  filter(!str_detect(med, 'dextrose (.+?) and sodium chloride (.+?)')) %>% 
  filter(!str_detect(med, 'tpn')) %>% 
  filter(!str_detect(med, 'sodium chloride (.+?) bolus')) %>% 
  filter(!str_detect(med, 'sodium chloride (.+?) infusion')) %>% 
  filter(!str_detect(med, 'lactated ringer\'s bolus'))  %>% 
  filter(!str_detect(med, 'lactated ringer\'s infusion')) %>% 
  filter(!str_detect(med, '^dextrose (.+?) bolus')) %>% 
  filter(!str_detect(med, '^dextrose bolus')) %>% 
  filter(!str_detect(med, '^dextrose \\([:digit:]+%\\)')) %>% 
  filter(!str_detect(med, '^dextrose [:digit:]+%')) %>% 
  filter(!str_detect(med, '^dextrose [:digit:]+ %')) %>% 
  filter(!str_detect(med, '^dextrose 12.5 %')) %>% 
  filter(!str_detect(med, 'dianeal')) %>% 
  filter(!str_detect(med, 'plasma-lyte')) %>% 
  filter(!str_detect(med, 'vaccine')) %>% 
  filter(!str_detect(med, 'vfc')) %>% 
  filter(!(str_detect(med, 'hydrocortisone') & str_detect(units, 'Application')))

enteral_med_string <- c('tablet',
                        'capsule',
                        'suspension',
                        'solution',
                        'oral',
                        'packet',
                        'soln',
                        'solid',
                        'liquid',
                        'reditabs',
                        'dissolvable',
                        'granules',
                        'tab',
                        'enteric',
                        'pack',
                        'coated',
                        'concentrate',
                        'delayed release',
                        'disintegrating',
                        'chewable',
                        'syrup',
                        'drops',
                        'elixer',
                        'lozenge'
)
enteral_med_string <- str_flatten(enteral_med_string, '|')

iv_med_string <- c(       
  'injection',
  'infusion',
  'injectable',
  'inj',
  'bolus from bag',             
  'bolus',       
  'syringe',     
  'iv syringe',
  'ivpb',
  'iv$',
  ' iv ',
  'premix',
  'nfusion',
  'dexmedetomidine'
)
iv_med_string <- str_flatten(iv_med_string, '|')

# Flag oral versus IV medications
meds_cleaned <- meds_cleaned %>% 
  mutate(route = case_when(
    str_detect(med, iv_med_string) ~ 'iv',
    str_detect(med, enteral_med_string) ~ 'enteral',
    str_detect(med, 'patch') ~ 'patch',
    TRUE ~ 'other'
  ),
  route = factor(route)) 

# Split the text "med" field into a generic and brand name.
meds_split <- meds_cleaned %>%
  mutate(med = str_replace_all(med, '([a-z])([0-9])', '\\1 \\2'),
         med = str_remove_all(med, '\\(.+\\)'),
         med = str_extract(med, '^.+?(?=\\s[:digit:])|^.+'),
         med = str_remove_all(med, '\\)'),
         med = str_remove_all(med, 'opioid]-induced pruritus'),
         med = str_remove_all(med, 'tablet'),
         med = str_remove_all(med, 'injection'),
         med = str_remove_all(med, 'infusion'),
         med = str_remove_all(med, 'injectable'),
         med = str_remove_all(med, 'inj'),
         med = str_remove_all(med, ' gel'),
         med = str_remove_all(med, 'capsule'),
         med = str_remove_all(med, 'solution'),
         med = str_remove_all(med, 'suspension'),
         med = str_remove_all(med, ' oral'),
         med = str_remove_all(med, 'powder'),
         med = str_remove_all(med, 'packet'),
         med = str_remove_all(med, 'soln'),
         med = str_remove_all(med, 'solid'),
         med = str_remove_all(med, 'liquid'),
         med = str_remove_all(med, 'powder'),
         med = str_remove_all(med, 'packet'),
         med = str_remove_all(med, 'anaphylaxis'),
         med = str_remove_all(med, 'external'),
         med = str_remove_all(med, 'reditabs'),
         med = str_remove_all(med, 'dissolvable'),
         med = str_remove_all(med, 'granules'),
         med = str_remove_all(med, 'ointment'),
         med = str_remove_all(med, 'rectal'),
         med = str_remove_all(med, 'bolus from bag'),
         med = str_remove_all(med, 'kit'),
         med = str_remove_all(med, 'suppository'),
         med = str_remove_all(med, ' tab'),
         med = str_remove_all(med, ' chemo'),
         med = str_remove_all(med, 'enteric'),
         med = str_remove_all(med, ' pack'),
         med = str_remove_all(med, 'bolus'),
         med = str_remove_all(med, 'hfa'),
         med = str_remove_all(med, 'coated'),
         med = str_remove_all(med, 'concentrate'),
         med = str_remove_all(med, 'delayed release'),
         med = str_remove_all(med, 'syringe'),
         med = str_remove_all(med, 'subcutaneous'),
         med = str_remove_all(med, 'iv syringe'),
         med = str_remove_all(med, 'ivpb'),
         med = str_remove_all(med, 'nasal'),
         med = str_remove_all(med, 'spray'),
         med = str_remove_all(med, 'disintegrating'),
         med = str_remove_all(med, 'ophthalmic'),
         med = str_remove_all(med, 'chewable'),
         med = str_remove_all(med, 'peripheral'),
         med = str_remove_all(med, 'orderable'),
         med = str_remove_all(med, 'central'),
         med = str_remove_all(med, 'bromide'), 
         med = str_remove_all(med, 'hcl'), 
         med = str_remove_all(med, 'citrate'), 
         med = str_remove_all(med, 'pca'), 
         med = str_remove_all(med, '[:space:]for.+'),
         med = str_remove_all(med, '[:space:]in[:space:].*'),
         med = str_remove_all(med, '[:space:]in.*'),
         med = str_remove_all(med, '[:space:]in$'),
         med = str_remove_all(med, '[:space:]iv[:space:].*'),
         med = str_remove_all(med, '[:space:]iv$'),
         med = str_remove_all(med, '[:space:].+[:punct:][:space]*'),
         med = str_remove_all(med, '(?<=^[:alnum:])'),
         med = str_remove_all(med, 'generic'),
         med = str_squish(med)
  ) %>% 
    select(mrn, enc_id, med, dose, units, frequency, route, conc, conc_unit, infusion_rate,
         med_time, mar_result) 

# Correct a few meds with confusing names
meds_split <- meds_split %>% mutate(
  med = str_remove_all(med, ' injection'),    # careful with this! might mess up heparin locks
  med = str_remove_all(med, 'clinician bolus prn\\: '),
  med = str_remove_all(med, 'odt'),
  med = str_remove_all(med, 'solution'),
  med = str_replace_all(med, 'aspirin.+', 'aspirin'),
  med = str_replace_all(med, 'fentanyl.+', 'fentanyl'),
  med = str_replace_all(med, 'lidocaine-epinephrine', 'lidocaine with epi'),
  med = str_replace_all(med, 'methadone.+', 'methadone'),
  med = str_replace_all(med, 'methylprednisolone.+', 'methylprednisolone'),
  med = str_replace_all(med, 'midazolam.+', 'midazolam'),
  med = str_replace_all(med, 'prednisolone.+', 'prednisolone'),
  med = str_replace_all(med, 'racepinephrine', 'racemic epi'),
  med = str_replace_all(med, 'warfarin.+', 'warfarin'),
  med = str_replace_all(med, 'hydrocortisone.+', 'hydrocortisone'),
  med = str_remove_all(med, 'status epi'),
  med = str_trim(med)
)

# Process a "concentration/rate" dose which is more accurate (but is not weight-based, will need to be back-calculated)
meds_split <- meds_split %>% 
  mutate(dose_conc = conc*infusion_rate) %>% 
  relocate(dose_conc, .after = dose)

# Just save infusions (defined as having a per time unit). 
meds_infusions <- meds_split %>% filter(str_detect(units, 'hr|hour|min|minute')) %>% 
  select(-frequency) %>% 
  mutate(dose_conc = conc*infusion_rate) %>% 
  relocate(dose_conc, .after = dose)

# Filter out when meds were stopped and save
meds_infusion_stopped <- meds_infusions %>% filter(mar_result %in% mar_meds_stopped)
# meds_infusions <- meds_infusions %>% filter(!(mar_result %in% mar_meds_stopped))

# Just save bolus
meds_bolus <- meds_split %>% filter(!str_detect(units, 'hr|hour|min|minute')) %>% 
  select(-infusion_rate)

# Will need to choose 1 dosing weight, for now just limit to the first
# # Convert all units to mg/kg/hr
pt_wt <- df_weights %>% 
  group_by(mrn, enc_id) %>% 
  arrange(mrn, enc_id, doseweight_time) %>% 
  slice_head(n=1) %>% ungroup()

# Most meds are weight based. Some are ordered adult-style, in units/hr. If this is the case, calculate 
# a weight based dose
meds_infusion_sedatives <- meds_infusions %>% 
  left_join(pt_wt) %>% 
  filter(str_detect(med, 'midazolam|dexmedetomidine|morphine|fentanyl|hydromorphone|ketamine|vecuronium|rocuronium|cisatracurium')) %>% 
  mutate(dose = signif(dose, 2),
         wt_based = if_else(str_detect(units, 'kg'), TRUE, FALSE),
         wt_based_dose = if_else(wt_based, dose, signif(dose / dosing_weight, 2)))

# define "given codes", "change", and "stop" codes -- only a certain number of
# ways that a medication infusion can be started.
# once a med is started, this begins an "infusion period"
mar_med_given <- c('continue to inpatient floor', 
                   'continued from or', 
                   'continued from pre', 
                   'given', 
                   'given by other', 
                   'given during downtime', 
                   'handoff', 
                   'handoff (dual sign required)', 
                   #'mar unhold', 
                   'new bag', 
                   'new bag/syringe/cartridge', 
                   'override pull', 
                   'rate change', 
                   'rate verify', 
                   'rate/dose change', 
                   'rate/dose changed', 
                   'rate/dose verify', 
                   'restarted', 
                   'restarted (dual sign required)', 
                   'started', 
                   'started during downtime', 
                   #'unheld by provider',
                   'verification',
                   'anesthesia volume adjustment', 
                   'rate change', 
                   'rate verify', 
                   'rate/dose change', 
                   'rate/dose changed', 
                   'rate/dose verify'
)

# Boluses need their own codes
mar_med_bolus_given <- c(
  'bolus',
  'bolus from bag',
  'bolus from bag (dual sign required)',
  'continued from pre',
  'given',
  'given by other',
  'given during downtime',
  'handoff',
  'new bag',
  'new bag/syringe/cartridge',
  'override pull',
  'rate change',
  'rate verify',
  'rate/dose verify',
  'restarted',
  'restarted (dual sign required)',
  'started',
  'started during downtime'
)

# define "stop codes" -- once a medication is started at a given rate, one of 3 things can happen:
#   1. same or equivalent order is put in (no change, just ignore this)
#   2. med rate/dose is changed -- this ends an infusion period and starts a new one
#   3. med is stopped -- this ends an infusion period
mar_med_stopped <- c('held', 
                     'held by provider',
                     'mar hold', 
                     'stopped (dual sign required)',
                     'stopped',
                     'stop infusion')

mar_med_changed <- c('anesthesia volume adjustment', 
                     'rate change', 
                     'rate verify', 
                     'rate/dose change', 
                     'rate/dose changed', 
                     'rate/dose verify')

# Process the midaz infusions to flag starts, stops, changes.
# Change stopped doses to zero
# Number each MAR administration
meds_infusion_sedatives2 <- meds_infusion_sedatives %>% 
  arrange(mrn, enc_id, med, med_time) %>% 
  group_by(mrn, enc_id, med) %>% 
  mutate(mar_num = dense_rank(med_time)) %>% 
  ungroup() %>% 
  group_by(mrn, enc_id, med) %>% 
  mutate(
    med_start = if_else(mar_num == 1, TRUE, FALSE), # Med is by definition started at the first admin
    med_given = if_else(mar_result %in% mar_med_given, TRUE, FALSE), # Meds are only given if they have certain MAR comments
    med_stop = if_else(mar_result %in% mar_med_stopped, TRUE, FALSE), # Meds are stopped if they fit certain MAR comments
    time_diff_next = round(as.numeric(difftime(lead(med_time), med_time, units = 'hours')), 1), # Difference between time of this and next row
    time_diff_next = if_else(row_number() == n(), 0, time_diff_next), # Make sure the last row has a non-NA time
    time_diff_last = round(as.numeric(difftime(med_time, lag(med_time), units = 'hours')), 1), # Difference between time of this and the last row
    time_diff_last = if_else(row_number() == 1, 0, time_diff_last), # Make sure the first row has a non-NA time
    # med_stop = if_else( (mrn == lead(mrn)) & (time_diff_last > 24), TRUE, med_stop), # Meds are stopped if nothing was charted for >24 hours
    # med_stop = if_else( mrn != lead(mrn), TRUE, med_stop), # Meds are always stopped at the last data entry per patient (even if it was still charted as being given)
    med_stop = if_else( row_number() == n(), TRUE, med_stop), # Correct the last row so it is non-NA
    med_change = if_else((wt_based_dose != lead(wt_based_dose)), TRUE, FALSE), # Meds are changed if the next dose is different from this dose (for the same patient)
    med_change = if_else(row_number() == n(), FALSE, med_change), # Correct the last row so it is non-NA
    wt_based_dose = if_else(med_stop, 0, wt_based_dose), # Dose is zero if the med was stopped
    wt_based_dose = if_else(!med_given, 0, wt_based_dose) # Dose is zero if there is an invalid MAR comment
  ) %>% 
  ungroup() %>% 
  # Account for sparse data by flagging rows for removal. (There are many rows that contain nursing administration
  # comments but no actual dosing change. Only need to keep rows with changes.)
  mutate(
    # Flag row for removal if a med was given, and not stopped, not changed, and not started
    remove_row = if_else(med_given &
                           !(med_stop | med_change | med_start),
                         TRUE, FALSE),
    # Simpler version -- flag for removal if the prior row was for the same patient, and had the exact same dose, 
    # or the med was not given (such as a bolus from the pump, which isn't an infusion)
    remove_row_simple = if_else(mrn == lag(mrn) &
                                  wt_based_dose == lag(wt_based_dose),
                                TRUE, FALSE),
    # Correct the first row
    remove_row_simple = if_else(row_number() == 1, FALSE, remove_row_simple)
  )

# Separate out paralytics (we will just track days of paralytic)
meds_infusions_paralytic <- meds_infusion_sedatives2 %>% 
  filter(str_detect(med, 'rocuronium|vecuronium|cisatracurium')) %>% 
  mutate(med_day = as.Date(med_time)) %>% 
  group_by(mrn, enc_id, med_day) %>% 
  slice_head() %>% ungroup() %>% 
  select(mrn, enc_id, med, med_day)

# 1. Remove all of the flagged rows, and recalculate the difference in time for each row
#       Since we set the last row for each patient to "stop" and a dose of zero, this means that for some patients
#       we will lose the last row of med dosing. (Patients total doses will be lower than reality if the nurse
#       didn't ever chart stopping the med.") This is ok and shouldn't be different among different patient groups.
# 2. Remove all of the rows with a dose of zero (these are just markers for when meds were stopped).
# 3. Calculate a cumulative dose for each interval, which is just the dose x the amount of time
meds_infusion_sedatives3 <- meds_infusion_sedatives2 %>% filter(!remove_row_simple) %>% 
  group_by(mrn, enc_id, med) %>% 
  mutate(
    time_diff = round(as.numeric(difftime(lead(med_time), med_time, units = 'hours')), 1),
    time_diff = if_else(row_number() == n(), 0, time_diff)
  ) %>% 
  filter(wt_based_dose > 0) %>% 
  mutate(
    interv_dose = wt_based_dose * time_diff
  ) %>% 
  ungroup()

# List of medications we will look at
med_str <- c(
  'midazolam',
  'lorazepam',
  'diazepam',
  'clonazepam',
  'alprazolam',
  'morphine',
  'fentanyl',
  'hydromorphone',
  'oxycodone',
  'clonidine',
  'ketamine',
  'pentobarbital',
  'quetiapine',
  'haloperidol',
  'risperidone',
  'olanzapine',
  'aripiprazole',
  'diphenhydramine',
  'hydroyxyzine',
  'rocuronium',
  'vecuronium',
  'cisatracurium'
)
med_str <- str_flatten(med_str, collapse = '|')

# Now process bolus doses
meds_bolus_sedatives <- left_join(meds_bolus, pt_wt, by = c('mrn', 'enc_id')) %>% 
  filter(str_detect(med, med_str)) %>% 
  filter(route != 'patch') %>%
  mutate(wt_based = if_else(str_detect(units, 'kg'), TRUE, FALSE),
         wt_based_dose = if_else(wt_based, dose, signif(dose / dosing_weight, 2)),
         dose = signif(dose, 2))

# Only keep doses that were given
meds_bolus_sedatives <- meds_bolus_sedatives %>% 
  filter(mar_result %in% mar_med_bolus_given) %>% 
  mutate(interv_dose = wt_based_dose)

# Separate out paralytics (we will just track days of paralytic)
meds_bolus_paralytic <- meds_bolus_sedatives %>% 
  filter(str_detect(med, 'rocuronium|vecuronium|cisatracurium')) %>% 
  mutate(med_day = as.Date(med_time)) %>% 
  group_by(mrn, enc_id, med_day) %>% 
  slice_head() %>% ungroup() %>% 
  select(mrn, enc_id, med, med_day)

# NEED TO EVENTUALLY LIMIT TO JUST IN-ICU DOSES
# WE WILL SUBSTITUTE ENC_ID FOR NOW BUT THIS IS THE ENTIRE HOSPITALIZATION

# Stack doses and convert to either morphine or midazolam equivalents
stack_infuse <- meds_infusion_sedatives3 %>% 
  select(mrn, enc_id, med, med_time,
         dosing_weight, interv_dose, wt_based_dose, route) %>% 
  mutate(type = 'infusion')

stack_bolus <- meds_bolus_sedatives %>% 
  select(mrn, enc_id, med, med_time,
         dosing_weight, interv_dose, wt_based_dose, route) %>% 
  mutate(type = 'bolus')

# Stack paralytic doses
meds_stack_paralytic  <-bind_rows(meds_infusions_paralytic, meds_bolus_paralytic) %>% 
  group_by(mrn, enc_id, med_day) %>% 
  arrange(med_day) %>% 
  slice_head() %>% ungroup()

sedative_all_doses <- bind_rows(stack_infuse, stack_bolus) %>% 
  mutate(
    interv_dose = case_when(
      med == 'midazolam' ~ interv_dose,
      med == 'lorazepam' ~ interv_dose / 2,
      med == 'clonazepam' ~ interv_dose / 4,
      med == 'diazepam' ~ interv_dose * 4,
      med == 'morphine' & route == 'iv' ~ interv_dose,
      med == 'morphine' & route == 'enteral' ~ interv_dose / 3,
      med == 'fentanyl' ~ interv_dose / 10,
      med == 'hydromorphone' & route == 'iv' ~ interv_dose * 4,
      med == 'hydromorphone' & route == 'enteral' ~ interv_dose * 0.8,
      med == 'oxycodone' ~ interv_dose * 0.4,
      med == 'clonidine' ~ interv_dose,
      med == 'dexmedetomidine' ~ interv_dose,
      med == 'ketamine' ~ interv_dose,
      med == 'pentobarbital' ~ interv_dose,
      med == 'rocuronium' ~ interv_dose,
      med == 'vecuronium' ~ interv_dose * 10,
      med == 'cisatracurium' ~ interv_dose * 20/3,
      TRUE ~ interv_dose
    )
  ) %>% 
  arrange(mrn, enc_id, med, med_time) %>% 
  select(-wt_based_dose)

# Remove magic mouthwash
sedative_all_doses <- sedative_all_doses %>% 
  filter(str_detect(med, 'diphenhydramine/lidocaine', negate = TRUE))

# Get a cumulative benzo dose per patient
dose_per_patient_benzo <- sedative_all_doses %>% 
  filter(str_detect(med, 'midazolam|lorazepam|diazepam|clonazepam|alprazolam')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(benzo_dose = sum(interv_dose))

# Get a cumulative opiate dose per patient
dose_per_patient_opiate <- sedative_all_doses %>% 
  filter(str_detect(med, 'morphine|fentanyl|hydromorphone|oxycodone')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(opiate_dose = sum(interv_dose))

# Get a cumulative ketamine dose per patient
dose_per_patient_ketamine <- sedative_all_doses %>% 
  filter(str_detect(med, 'ketamine')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(ketamine_dose = sum(interv_dose))

# Get a cumulative dexmedetomidine dose per patient
dose_per_patient_dex <- sedative_all_doses %>% 
  filter(str_detect(med, 'dexmedetomidine')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(dexmed_dose = sum(interv_dose))

# Get a cumulative pento dose per patient
dose_per_patient_pento <- sedative_all_doses %>% 
  filter(str_detect(med, 'pentobarbital')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(pento_dose = sum(interv_dose))

# Dose per patient of antipsychotic
dose_per_patient_antipsych <- sedative_all_doses %>% 
  filter(str_detect(med, 'quetiapine|haloperidol|risperidone|olanzapine|aripiprazol')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(antipsych_dose = sum(interv_dose))

# Anticholinergic
dose_per_patient_antichol <- sedative_all_doses %>% 
  filter(str_detect(med, 'diphenhydramine|hydroxyzine')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(antichol_dose = sum(interv_dose))

# Get a cumulative paralytic dose per patient
dose_per_patient_paral <- sedative_all_doses %>% 
  filter(str_detect(med, 'rocuronium|vecuronium|cisatracurium')) %>% 
  group_by(mrn, enc_id) %>% 
  summarize(paral_dose = sum(interv_dose))

# Get number of days of paralytic
days_of_paralytic <- meds_stack_paralytic %>% 
  group_by(mrn, enc_id) %>% 
  summarize(paral_days = n())

# Combine this into one chart
dose_per_patient_combo <- full_join(dose_per_patient_benzo, dose_per_patient_opiate, by = c('mrn', 'enc_id')) %>% 
  full_join(dose_per_patient_ketamine, by = c('mrn', 'enc_id')) %>% 
  full_join(dose_per_patient_dex, by = c('mrn', 'enc_id')) %>% 
  full_join(dose_per_patient_pento, by = c('mrn', 'enc_id')) %>% 
  full_join(dose_per_patient_antipsych, by = c('mrn', 'enc_id')) %>%
  full_join(dose_per_patient_antichol, by = c('mrn', 'enc_id')) %>% 
  full_join(dose_per_patient_paral, by = c('mrn', 'enc_id')) %>% 
  full_join(days_of_paralytic, by = c('mrn', 'enc_id'))

# Get a cumulative dose per patient, separated by individual drug
dose_per_patient_each_drug <- sedative_all_doses %>% 
  group_by(mrn, med) %>% 
  summarize(cumul_dose = sum(interv_dose)) %>% 
  pivot_wider(names_from = med,
              values_from = cumul_dose)

