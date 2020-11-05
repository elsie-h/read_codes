#### CCI ####

# Description: codes for derving the Charlson Comorbidity Index (CCI)
# cat1: CCI
# cat2: CCI component
# score: CCI weight

source('setup.R')

# codelist OPCRD Risk Model Scripts
cl_cci_codes <- read_delim("lists_in/RMS/cl_cci_codes_rms.txt", 
                           "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code, cat2 = list) %>%
  mutate(cat1 = 'cci',
         RMS = 1) %>%
  mutate_at('read_code', list(~ str_extract(., '^.{5}'))) %>%
  distinct()

# use the scores from the khan paper
khan_2010_cci <- read_csv("lists_in/Khan2010/khan_2010_cci.csv") %>%
  mutate(read_code = str_extract(`Read/OXMIS code`, '^.{5}')) %>%
  select(read_code, read_term = `Read/OXMIS term`, 
         score = `Charlson score weight`,
         cat2 = `Charlson disease category`) %>%
  mutate(cat1 = 'cci',
         Khan = 1) %>%
  distinct()

cci <- bind_rows(cl_cci_codes, khan_2010_cci) %>%
  mutate_at('cat2', list(~ case_when(.=='AIDS'|.=='HIV' ~ 'AIDS',
                                     .=='cancer'|.=='Cancer' ~ 'cancer',
                                     .=='cerebral_vascular_accident'|.=='Cerebrovascular disease' ~ 'cerebrovascular',
                                     .=='Chronic pulmonary disease'|.=='pulmonary_disease' ~ 'pulmonary',
                                     .=='Congestive heart disease'|.=='congestive_heart_failure' ~ 'congestive_heart',
                                     .=='connective_tissue_disorder'|.=='Rheumatological disease' ~ 'rheumatological',
                                     .=='dementia'|.=='Dementia' ~ 'dementia',
                                     .=='diabetes'|.=='Diabetes' ~ 'diabetes',
                                     .=='Diabetes with complications'|.=='diabetes_complications' ~ 'diabetes_complications',
                                     .=='Hemiplegia'|.=='paraplegia' ~ 'paraplegia',
                                     .=='Metastatic tumour'|.=='metastatic_cancer' ~ 'metastatic_cancer',
                                     .=='Mild liver disease'|.=='mild_liver_disease' ~ 'mild_liver',
                                     .=='Mod liver disease'|.=='severe_liver_disease' ~ 'severe_liver',
                                     .=='Myocardial infarction'|.=='Myocardial_infraction' ~ 'myocardial_infraction',
                                     .=='Peptic ulcer disease'|.=='peptic_ulcer' ~ 'peptic_ulcer',
                                     .=='Peripheral vascular disease'|.=='peripheral_vascular_disease' ~ 'peripheral_vascular',
                                     .=='Renal disease'|.=='renal_disease' ~ 'renal',
                                     TRUE ~ NA_character_))) %>%
  group_by(cat2) %>%
  mutate(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at('read_code', list(~ if_else(str_detect(., '^K458'),
                                        'K458.',
                                        .))) %>%
  # if a read code appears twice, one with a description and once without, keep the one with
  arrange(cat1, read_code, read_term) %>%  
  distinct(read_code, .keep_all = TRUE) %>%
  filter(!is.na(read_code)) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicate Read codes
cci$read_code[duplicated(cci$read_code)]

# save
saveRDS(cci, file = 'lists_out/CCI.RDS', compress = FALSE)

