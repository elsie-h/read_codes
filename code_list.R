# All Read codes in tibble

#### load libraries ####
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

#### Functions ####
read_cprd <- function(name) {
  file <- str_c('codes/CPRD_', name, '.csv')
  rc_ <- suppressMessages(read_csv(file)) %>%
    select(read_code = `Read code`,
           read_term = Descr) %>%
    mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
    distinct()
}

# function for cleaning code lists from the Nwaru2020 paper
clean_nwaru <- function(.data, cat1_string) {
  .data %>%
    gather() %>%
    select(read_code = value) %>%
    mutate_at('read_code', list(~ str_remove_all(., '\\\"'))) %>%
    mutate_at('read_code', list(~ str_trim(., side = 'both'))) %>%
    mutate(read_term = NA_character_,
           cat2 = cat1_string)
}

#### Asthma Review ####

cl_asthma_review <- read_delim("codes/cl_asthma_review_elsie.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'asthma_review') %>%
  distinct()

#### RCGP3Qs ####
rcgp3q <- read_delim("codes/rcgp3q_elsie",
                     ",",
                     escape_double = FALSE, 
                     trim_ws = TRUE) %>%
  add_row(read_code = '66Ys.',
          read_term = 'Asthma never causes night symptoms',
          cat1 = 'RCGP3Q',
          cat2 = 'sleeping',
          score = 0) %>%
  add_row(read_code = '388t.',	
          read_term = 'Royal College of Physicians asthma assessment',
          cat1 = 'RCGP3Q',
          cat2 = 'general') %>%
  add_row(read_code = '66Yp.',
          read_term = 'Asthma review using Royal College of Physicians three questions',
          cat1 = 'RCGP3Q',
          cat2 = 'general') %>%
  add_row(read_code = '663P2',	
          read_term = 'Asthma limits activities most days',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663P1',
          read_term = 'Asthma limits activities 1 to 2 times per week',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663P0',	
          read_term = 'Asthma limits activities 1 to 2 times per month',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663x.',
          read_term = 'Asthma limits walking on the flat',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1)


#### asthma plan ####
asthma_plan <- read_delim("codes/cl_asthma_plan_elsie",
                     ",",
                     escape_double = FALSE, 
                     trim_ws = TRUE) %>%
  add_row(read_code = '8CMA0',
          read_term = 'Patient has a written asthma personal action plan',
          version=2) %>%
  add_row(read_code = '8CR0.',
          read_term = 'Asthma clinical management plan',
          version=2) %>%
  add_row(read_code = '661M1',
          read_term = 'Asthma self-management plan agreed',
          version = 2) %>%
  mutate(cat1 = 'asthma_plan')
  mutate_at('cat2', as.character)

#### Asthma exacerbation ####
exacerbation <- read_delim("codes/cl_exacerbation_elsie",
                           ",",
                           escape_double = FALSE, 
                           trim_ws = TRUE) 

#### Asthma hospitalisation ####
hospitalisation <- read_delim("codes/cl_hospitalisation_elsie",
                              ",",
                              escape_double = FALSE, 
                              trim_ws = TRUE) %>%
  add_row(read_code = '663m.',
          read_term = 'Asthma accident and emergency attendance since last visit',
          cat1 = 'hospitalisation',
          cat2 = NA_character_)

#### asthma ####

asthma_V3 <- read_csv("codes/opensafely-asthma-diagnosis-2020-04-15.csv")

asthma_V3 <- asthma_V3 %>% 
  filter(CTV3Source == 'CTV3Map_Code_Only' |
           CTV3Source == 'CTV3Map_Code_And_Term') %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'asthma_other',
         cat2 = NA_character_,
         version = 3) %>%
  distinct()

CPRD_asthma <- read_cprd('asthma') %>% 
  mutate(cat1 = 'asthma_other',
         cat2 = NA_character_,
         version = 2)

asthma_other <- bind_rows(asthma_V3, CPRD_asthma) %>%
  add_row(read_code = '38DL.', read_term = 'Asthma control test',
          cat1 = 'asthma_other', cat2 = NA_character_, version = NA_real_) %>%
  add_row(read_code =  '679J.', read_term = 'Health education - asthma',
          cat1 = 'asthma_other') %>%
  add_row(read_code = '1789.',
          read_term = 'Asthma trigger - respiratory infection') %>%
  add_row(read_code = '1788.',
          read_term = 'Asthma trigger - cold air') %>%
  add_row(read_code = '178B.',
          read_term = 'Asthma trigger - exercise') %>%
  add_row(read_code = 'XaIww',
          read_term = 'Asthma trigger') %>%
  add_row(read_code = '178..',
          read_term = 'Asthma trigger') %>%
  add_row(read_code = '1781.',
          read_term = 'Asthma trigger - pollen') %>%
  add_row(read_code = '1787.',
          read_term = 'Asthma trigger - seasonal') %>%
  add_row(read_code = '1786.',
          read_term = 'Asthma trigger - animals') %>%
  add_row(read_code = '178A.',
          read_term = 'Asthma trigger - airborne dust') %>%
  add_row(read_code = 'XaIer',
          read_term = 'Asthma follow-up') %>%
  add_row(read_code = '1785.',	
          read_term = 'Asthma trigger - damp') %>%
  add_row(read_code = '1784.',
          read_term = 'Asthma trigger - emotion') %>%
  add_row(read_code = '663j.',
          read_term = 'Asthma - currently active') %>%
  add_row(read_code = '1783.',
          read_term = 'Asthma trigger - warm air') %>%
  add_row(read_code = '1782.',
          read_term = 'Asthma trigger - tobacco smoke') %>%
  anti_join(cl_asthma_review, by = 'read_code') %>%
  anti_join(asthma_plan, by = 'read_code') %>%
  anti_join(rcgp3q, by = 'read_code') %>%
  anti_join(exacerbation, by = 'read_code') %>%
  anti_join(hospitalisation, by = 'read_code') %>%
  # get rid of 'asthma resolved'
  filter(!(read_term %in% 'Asthma resolved')) %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'H330.' ~ 'Extrinsic (atopic) asthma',
                                          read_code %in% 'H3300' ~ 'Extrinsic asthma without status asthmaticus',
                                          read_code %in% 'H331.' ~ 'Intrinsic asthma',
                                          read_code %in% 'H33zz' ~ 'Asthma NOS',
                                          . %in% 'Exercise induced asthma' ~ 'Exercise-induced asthma',
                                          . %in% 'Aspirin induced asthma' ~ 'Aspirin-induced asthma',
                                          . %in% 'Late onset asthma' ~ 'Late-onset asthma',
                                          TRUE~ .))) %>%
  mutate_at('cat2', list(~  case_when(str_detect(read_term, 'Asthma control step') ~ 'asthma_control_step',
                                      str_detect(read_term, 'trigger') ~ 'trigger',
                                      str_detect(read_term, 'Health education') ~ 'health_education',
                                      TRUE ~ NA_character_)))

#### COPD ####

chronic_respiratory_disease_V3 <- read_csv("codes/opensafely-chronic-respiratory-disease-2020-04-10.csv")

copd_v3 <- chronic_respiratory_disease_V3 %>% 
  filter(CTV3Source == 'CTV3Map_Code_Only' |
           CTV3Source == 'CTV3Map_Code_And_Term') %>%
  filter(str_detect(CTV3PreferredTermDesc,
                    regex('chronic obstructive pulmonary disease|copd', 
                          ignore_case = TRUE))) %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'comorbidity',
         cat2 = 'copd',
         version = 3) %>%
  distinct()

CPRD_COPD <- read_cprd('copd') %>% 
  add_row(read_code = '66YM.',
          read_term = 'Chronic obstructive pulmonary disease annual review') %>%
  add_row(read_code = '66Yf.',	
          read_term = 'Number of chronic obstructive pulmonary disease exacerbations in past year') %>%
  add_row(read_code = '66YI.',
          read_term = 'Chronic obstructive pulmonary disease self-management plan given') %>%
  add_row(read_code = '66YB.',
          read_term  = 'Chronic obstructive pulmonary disease monitoring') %>%
  mutate(cat1 = 'comorbidity',
         cat2 = 'copd',
         version = 2) 

copd_rms <- bind_rows(read_delim("codes/cl_copd_qof_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE),
                      read_delim("codes/cl_copd_review_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE)) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'copd')

copd_all <- bind_rows(copd_v3, CPRD_COPD, copd_rms) %>%
  group_by(read_code, version) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(!(is.na(version) & n > 2)) %>%
  select(-n) 

#### Comorbidities ####
# For now these are just version 2
# will have to update with V3
comorbs <- c('allergic_rhinitis',
           'anxiety',
           'depression',
           'derm_merge',
           'GORD',
           'nasal_polyp',
           'osteoporosis')

comorbs_data <- bind_rows(lapply(comorbs, 
                              function(x) 
                                read_cprd(x) %>% 
                                mutate(cat1 = 'comorbidity',
                                       cat2 = x,
                                       version = 2)
                              )) %>%
  mutate_at('cat2', list(~ if_else(. %in% 'derm_merge', 'dermatitis', .))) 

anxiety_depression_rms <- read_delim("codes/cl_anxiety_depression_dx_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE)  %>%
  mutate(cat1 = 'comorbidity') %>%
  left_join(select(comorbs_data, read_term, cat2), by = 'read_term') %>%
  filter(!(is.na(cat2) & 
             (str_detect(read_term, regex('bipolar', ignore_case = T))|
                str_detect(read_term, regex('manic', ignore_case = T))))) %>%
  mutate_at('cat2', list(~ case_when(!is.na(.) ~ .,
                                     str_detect(read_term, regex('anxiety', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('anxious', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('fear', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('phobia', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('panic', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('depress', ignore_case = T)) ~ 'depression',
                                     # str_detect(read_term, regex('nerv', ignore_case = T)) ~ 'remove',
                                     # str_detect(read_term, regex('disturb', ignore_case = T)) ~ 'remove',
                                     # str_detect(read_term, regex('bulimia', ignore_case = T)) ~ 'remove',
                                     # str_detect(read_term, regex('psychoses', ignore_case = T)) ~ 'remove',
                                     TRUE ~ NA_character_))) %>%
  filter(!is.na(cat2))

nasal_polyps_rms <- read_delim("codes/cl_nasal_polyps_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE)  %>%
  mutate(cat1 = 'comorbidity', cat2 = 'nasal_polyp')
  
rhinitis_rms <- read_delim("codes/cl_rhinitis_dx_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'allergic_rhinitis') #should just be rhinitis

gerd_rms <- read_delim("codes/cl_gerd_dx_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'GORD')

comorbs_all <- bind_rows(comorbs_data,
                         anxiety_depression_rms,
                         nasal_polyps_rms,
                         rhinitis_rms,
                         gerd_rms) %>%
  distinct()

#### CCI ####
# use the scores from the khan paper
cl_cci_codes <- read_delim("codes/cl_cci_codes_rms.txt", # codelist OPCRD Risk Model Scripts
                               "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code, cat2 = list) %>%
  mutate(cat1 = 'cci',
         version = 3) %>%
  mutate_at('read_code', list(~ str_extract(., '^.{5}')))

khan_2010_cci <- read_csv("codes/khan_2010_cci.csv") %>%
  mutate(read_code = str_extract(`Read/OXMIS code`, '^.{5}')) %>%
  select(read_code, read_term = `Read/OXMIS term`, 
         score = `Charlson score weight`,
         cat2 = `Charlson disease category`) %>%
  mutate(cat1 = 'cci',
         version = 2)

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
  filter(!is.na(read_code))


#### BMI ####
bmi_codes <- read_csv("codes/bmi_codes_elsie.csv") %>%
  select(read_code, read_term = desc, cat2 = cat) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'bmi')

#### height and weight ####
height_and_weight <- tribble(~read_code, ~read_term, ~cat1,
                             '229..', 'height', 'height',
                             '22A..', 'weight', 'weight')

#### blood eosinophils ####
blood_eos <- tribble(~read_code, ~read_term, ~cat1,
                             '42K..', 'blood eosinophil count', 'eos')

#### smoking ####
cl_smoking <- read_delim("codes/cl_smoking_rms.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  add_row(read_code = 'Xa1bv', read_term = 'Ex-cigarette smoker', Category = 3) %>%
  add_row(read_code = '137j.', read_term = '	Ex-cigarette smoker', Category = 3) %>%
  add_row(read_code = '8IAj.', read_term = 	'Smoking cessation advice declined', Category = 2) %>%
  mutate(cat1 = 'smoking',
         score = Category,
         cat2 = case_when(score %in% 1 ~ 'never smoker',
                          score %in% 2 ~ 'current smoker',
                          score %in% 3 ~ 'ex-smoker')) %>%
  select(-Category) %>%
  distinct()

#### PEF ####
pef_codes <- read_csv("codes/pef_codes_elsie.csv") %>%
  add_row(read_code = 'XaEFL', read_term = 'Expected peak expiratory flow rate',
          cat2 = 'predicted PEFR') %>%
  add_row(read_code = 'XE2wt',	read_term = 'Expected peak flow rate',
          cat2 = 'predicted PEFR') %>%
  add_row(read_code = '339C.',	
          read_term = 'Expected peak flow rate',
          cat2 = 'predicted PEFR') %>%
  add_row(read_code = '339V.',
          read_term = 'Recorded/predicted peak expiratory flow rate ratio',
          cat2 = '% predicted PEFR') %>%
  mutate(cat1 = 'pefr')

#### ethnicity ####
# http://help.visionhealth.co.uk/reporting/1.3/Content/ExpRep%20Help%20Topics/5%20-%20Definitions/Ethnicity%20Codes.htm
rc_ethnicity_final <- read_csv("~/Docs/ethnicity_read_codes/rc_ethnicity_final.csv") %>%
  rename(cat2 = cat_final) %>%
  mutate(cat1 = 'ethnicity')

#### spirometry ####
cl_spirometry <- read_delim("codes/cl_spirometry_elsie",
                                  "|",
                                  escape_double = FALSE, 
                                  trim_ws = TRUE) %>%
  add_row(read_code = '68M..',
          read_term = 'Spirometry screening',
          cat1 = 'spirometry') %>%
  add_row(read_code = 'XaCJK',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = '339i.',	
          read_term = 'FVC/Expected FVC percent',
          cat2 = NA_character_) %>%
  add_row(read_code = '339P.',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = 'XaCJL',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') %>%
  add_row(read_code = '339Q.',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') 

##############################
#### ASTHMA PRESCRIPTIONS ####
##############################

# use the Nwaru categorisations (ICS, ICS+LABA, LTRA, LABA, SABA, SAMA, SABA+SAMA, OCS)
# but where possible left_join to the RMS codes for read_term as this is missing from the nwaru codelists

#### OCS ####
OCS <- read_delim("codes/cl_OCS_nwaru",
                              ",",
                              escape_double = FALSE, 
                              trim_ws = TRUE,
                              col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'ocs') %>%
  mutate(cat1 = 'OCS', cat2 = NA_character_) %>%
  add_row(read_code = '663a.',
          read_term = 'Oral steroids used since last appointment',
          cat1 = 'OCS', cat2 = 'since_last_appointment')

# No RMS codelist currently, will need to get Read terms elsewhere

#### omalizumab ####
omalizumab <- read_delim("codes/cl_omalizumab_nwaru",
                  ",",
                  escape_double = FALSE, 
                  trim_ws = TRUE,
                  col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'omalizumab') %>%
  mutate(cat1 = 'asthma_prescription')

# No RMS codelist currently, will need to get Read terms elsewhere

#### LTRA ####
LTRA_nwaru <- read_delim("codes/cl_LTRA_nwaru",
                         ",",
                         escape_double = FALSE, 
                         trim_ws = TRUE,
                         col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'LTRA') %>%
  mutate(cat1 = 'asthma_prescription')

LTRA_rms <- read_delim("codes/dl_ltra_rms.txt",
                         "|",
                         escape_double = FALSE, 
                         trim_ws = TRUE,
                         col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME) 

LTRA <- LTRA_nwaru %>%
  select(read_code) %>%
  left_join(LTRA_rms, by = 'read_code') %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'cA...' ~ 'Leukotriene receptor antagonist',
                                          read_code %in% 'cA1..' ~ 'Montelukast product',
                                          read_code %in% 'cA2..' ~ 'Zafirlukast',
                                          TRUE ~ .))) %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'LTRA')

# all Read terms now present

#### ICS ####
ICS_nwaru <- read_delim("codes/cl_ICS_nwaru",
                      ",",
                      escape_double = FALSE, 
                      trim_ws = TRUE,
                      col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'ICS') %>%
  mutate(cat1 = 'asthma_prescription')

ICS_rms <- read_delim("codes/dl_ics_inh_rms.txt",
                       "|",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME) 

ICS <- ICS_nwaru %>%
  select(read_code) %>%
  left_join(ICS_rms, by = 'read_code') %>% 
  mutate(cat1 = 'asthma_prescription', cat2 = 'ICS')

# Some Read terms still missing

#### methylxanthine ####
methylxanthine_nwaru <- read_delim("codes/cl_methylxanthine_nwaru",
                       ",",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'methylxanthine') %>%
  mutate(cat1 = 'asthma_prescription')

theo_rms <- read_delim("codes/dl_theo_rms.txt",
                             "|",
                             escape_double = FALSE, 
                             trim_ws = TRUE,
                             col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME)

methylxanthine <- methylxanthine_nwaru %>%
  select(read_code) %>%
  left_join(theo_rms, by = 'read_code')  %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'methylxanthine')

# Some Read terms still missing

#### SABA ####
SABA_nwaru <- read_delim("codes/cl_SABA_nwaru",
                       ",",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'saba') %>%
  mutate(cat1 = 'asthma_prescription')

SABA_rms <- read_delim("codes/dl_saba_inh_rms.txt",
                       "|",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME)

SABA <- SABA_nwaru %>%
  select(read_code) %>%
  left_join(SABA_rms, by = 'read_code') %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'SABA')

# Many Read terms still missing

#### LABA ####
LABA_nwaru <- read_delim("codes/cl_LABA_nwaru",
                   ",",
                   escape_double = FALSE, 
                   trim_ws = TRUE,
                   col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'laba') %>%
  mutate(cat1 = 'prescription')

LABALAMA_rms <- read_delim("codes/dl_laba_lama_rms.txt",
                       "|",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = TRUE) %>%
  select(read_code, read_term)

LABA <- LABA_nwaru %>%
  select(read_code) %>%
  left_join(LABALAMA_rms, by = 'read_code') %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'LABA')

# This didn't retreive any Read terms

#### SAMA ####
SAMA_nwaru <- read_delim("codes/cl_SAMA_nwaru",
                       ",",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'sama') %>%
  mutate(cat1 = 'prescription')

SAMA_rms <- read_delim("codes/dl_sama_rms.txt",
                       "|",
                       escape_double = FALSE, 
                       trim_ws = TRUE,
                       col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME)

SAMA <- SAMA_nwaru %>%
  select(read_code) %>%
  left_join(SAMA_rms, by = 'read_code') %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'SAMA')

# Some Read terms still missing

#### ICS & LABA ####
ICSLABA_nwaru <- read_delim("codes/cl_ICSLABA_nwaru",
                      ",",
                      escape_double = FALSE, 
                      trim_ws = TRUE,
                      col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'icslaba') %>%
  mutate(cat1 = 'asthma_prescription')

# Join to both ICS and LABA RMS codelists for Read terms
ICSLABA <- ICSLABA_nwaru %>%
  select(read_code) %>%
  left_join(ICS_rms, by = 'read_code') %>%
  left_join(LABALAMA_rms, by = 'read_code') 
# This didn't retrieve any Read terms

ICSLABA <- ICSLABA %>%
  select(read_code) %>%
  mutate(read_term = NA_character_, cat1 = 'asthma_prescription', cat2 = 'ICSLABA')

#### SABA & SAMA ####
SABASAMA_nwaru <- read_delim("codes/cl_SABASAMA_nwaru",
                             ",",
                             escape_double = FALSE, 
                             trim_ws = TRUE,
                             col_names = FALSE) %>%
  clean_nwaru(cat1_string = 'sabasama') %>%
  mutate(cat1 = 'prescription')

# Join to both SABA and SAMA RMS codelists for Read terms
SABASAMA <- SABASAMA_nwaru %>%
  select(read_code) %>%
  left_join(SABA_rms, by = 'read_code') %>%
  left_join(SAMA_rms, by = 'read_code') 
# This retrieved most Read terms

SABASAMA <- SABASAMA %>%
  mutate(read_term = if_else(is.na(read_term.x), read_term.y, read_term.x)) %>%
  select(read_code, read_term) %>%
  mutate(cat1 = 'asthma_prescription', cat2 = 'SABASAMA')

#### hormonal contraceptive ####
hormonal_contraceptive <- read_delim("codes/cl_hormonal_contraceptive_nwaru",
                   ",",
                   escape_double = FALSE, 
                   trim_ws = TRUE) %>%
  mutate(cat2 = 'hormonal_contraceptive') %>%
  mutate(cat1 = 'prescription')

#### Influenza vaccination ####
influenza_V3 <- read_csv("codes/opensafely-influenza-vaccination-clinical-codes-all-2020-05-19.csv")

influenza_given <- read_csv("codes/opensafely-influenza-vaccination-clinical-codes-given-2020-05-21.csv") %>%
  distinct(CTV3ID) %>% unlist() %>% unname()
influenza_notgiven <- read_csv("codes/opensafely-influenza-vaccination-clinical-codes-not-given-2020-05-19.csv") %>%
  distinct(CTV3ID) %>% unlist() %>% unname()
influenza_unclear <- read_csv("codes/opensafely-influenza-vaccination-clinical-codes-unclear-2020-05-19.csv") %>%
  distinct(CTV3ID) %>% unlist() %>% unname()


influenza_V3 <- influenza_V3 %>% 
  filter(CTV3Source == 'CTV3Map_Code_Only' |
           CTV3Source == 'CTV3Map_Code_And_Term') %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'influenza_vaccination',
         version = 3) %>%
  distinct() %>%
  mutate(cat2 = case_when(read_code %in% influenza_given ~ 'given',
                          read_code %in% influenza_notgiven ~ 'not_given',
                          read_code %in% influenza_unclear ~ 'unclear',
                          TRUE ~ NA_character_))

influenza_immunisation_v2 <- read_csv("codes/clinical-codes-influenza-immunisation.csv") %>%
  filter(coding_system %in% 'Read') %>%
  mutate_at('code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate_at('description', list(~ case_when(code %in% 'n473.' ~ 'INFLUVAC SUB-UNIT prefilled syringe 0.5mL',
                                            code %in% 'n477.' ~ 'INACTIVATED INFLUENZA VACCINE injection 0.5mL',
                                            code %in% 'n47d.' ~ 'FLUARIX VACCINE prefilled syringe',
                                            code %in% 'ZV048' ~ '[V]Influenza vaccination',
                                            TRUE ~ .))) %>%
  select(read_code = code, read_term = description) %>%
  mutate(cat2 = 'given') %>%
  add_row(read_code = '65ED.', cat2 = 'given',
          read_term = 'Seasonal influenza vaccination') %>%
  add_row(read_code = '9OX51', cat2 = 'not_given',
          read_term = 'Seasonal influenza vaccination declined') %>%
  add_row(read_code = '68NV0', cat2 = 'given',
          read_term = 'Consent given for seasonal influenza vaccination') %>%
  add_row(read_code = '9OX5.', cat2 = 'not given',
          read_term = 'Influenza vaccination declined')
  mutate(version = 2, cat1 = 'influenza_vaccination') %>%
  distinct()

influenza_vaccination <- bind_rows(influenza_V3, influenza_immunisation_v2)

#### bind rows and save code_list ####
read_codes <- bind_rows(cl_asthma_review, asthma_plan, asthma_other, 
                        copd_all, comorbs_all, 
                        bmi_codes, height_and_weight, blood_eos, cci,
                        cl_smoking, pef_codes, rc_ethnicity_final,
                        cl_spirometry, rcgp3q, hospitalisation,
                        exacerbation, OCS, omalizumab, LTRA,
                        ICSLABA, ICS, SABASAMA, methylxanthine,
                        SAMA, LABA, SABA, hormonal_contraceptive,
                        influenza_vaccination) %>%
  # make sure all have 5 characters, sometimes the final '.' or '0' gets dropped when all numeric
  mutate_at('read_code', list(~str_pad(., width=5, side='right', pad='.'))) %>%
  # replace double whitespace with single whitespace
  mutate_at('read_term', list(~ str_replace_all(., '\\s{2,}', ' '))) %>%
  # get rid of whitespace at start
  mutate_at('read_term', list(~ str_remove(., '^\\s+'))) %>%
  mutate(values = 1) %>%
  mutate_at('version', list(~ str_c('V', .))) %>%
  distinct() %>%
  pivot_wider(names_from = version, values_from = values) %>%
  select(-`NA`) %>%
  distinct()


saveRDS(read_codes, file = 'codes/read_codes.RDS', compress = FALSE)
