
source('setup.R')

#### Comorbidities ####
comorbs <- c('anxiety',
             'copd',
             'depression',
             'diabetes',
             'GORD',
             'nasal_polyp',
             'osteoporosis')

#### from CPRD codes: (Kuan 2019) ####
comorbs_data <- bind_rows(lapply(comorbs, 
                                 function(x) 
                                   read_cprd(x) %>% 
                                   mutate(cat1 = 'comorbidity',
                                          cat2 = x,
                                          version = 2))) 



################################################################################
#### Chronic Cardiac Disease ####

CCD <- read_csv("lists_in/OpenSafely/opensafely-chronic-cardiac-disease-2020-04-08.csv") %>%
  rename(read_code = CTV3ID, read_term = CTV3PreferredTermDesc)

# check mapping
# map V2 -> CTV3
CCD %>% map_V2_CTV3()
# map CTV3 -> V2
CCD %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

# add V2 codes
CCD <- CCD %>% bind_rows(
  CCD %>% map_CTV3_V2() %>%
    filter(str_detect(V2_CONCEPTID, '^1')|
             str_detect(V2_CONCEPTID, '^3')|
             str_detect(V2_CONCEPTID, '^5')|
             str_detect(V2_CONCEPTID, '^6')|
             (str_detect(V2_CONCEPTID, '^7') & !(V2_CONCEPTID %in% c('7L0..', '7L11.')))|
             str_detect(V2_CONCEPTID, '^8')|
             str_detect(V2_CONCEPTID, '^9')|
             str_detect(V2_CONCEPTID, '^G')|
             str_detect(V2_CONCEPTID, '^L')|
             (str_detect(V2_CONCEPTID, '^P') & !(V2_CONCEPTID %in% c('P....')))|
             str_detect(V2_CONCEPTID, '^Q')|
             (V2_CONCEPTID %in% c('F391B', 'SP076', 'ZV45K'))) %>%
    rename(read_code = V2_CONCEPTID, read_term = Term)) %>%
  distinct(read_code, read_term) 
  
################################################################################
#### Diabetes ####
diabetes_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'DM_COD')) 

# diabetes_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# diabetes_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

diabetes_qof <- diabetes_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

diabetes_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_diabetes_non_specific_dx_rms.txt",
                      delim = '|') %>%
  rename(read_term_rms = read_term)

diabetes_cprd <- comorbs_data %>%
  filter(cat2 %in% 'diabetes') %>%
  select(read_code, read_term_cprd = read_term)

diabetes_os <- read_csv("lists_in/OpenSafely/opensafely-diabetes-2020-04-15.csv") %>%
  filter(CTV3Source %in% c('CTV3Map_Code_And_Term',
                           'QOF',
                           'CTV3Map_Code_Only',
                           'CTV3_Children')) %>%
  select(read_code = CTV3ID, read_term_os = CTV3PreferredTermDesc)

diabetes <- diabetes_qof %>%
  rename(read_term = read_term_qof) %>%
  bind_rows(diabetes_os, diabetes_cprd) %>%
  arrange(read_code, QOF) %>%
  # if duplicate, will keep the QOF=Yes as NA goes last
  distinct(read_code, .keep_all = TRUE) %>%
  filter(!str_detect(read_term, regex('resolve', ignore_case = TRUE)))

diabetes <- diabetes_qof %>% 
  full_join(diabetes_cprd, by = 'read_code') %>%
  full_join(diabetes_rms, by = 'read_code') %>%
  full_join(diabetes_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_qof) ~ read_term_qof,
                               !is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_rms) ~ read_term_rms,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
diabetes %>% map_V2_CTV3()
# map CTV3 -> V2
diabetes %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

# add V2 codes
diabetes <- diabetes %>% bind_rows(
  diabetes %>% map_CTV3_V2() %>%
    filter(str_detect(V2_CONCEPTID, '^1')|
             (str_detect(V2_CONCEPTID, '^2') & !(V2_CONCEPTID %in% c('2G5..')))|
             str_detect(V2_CONCEPTID, '^3')|
             str_detect(V2_CONCEPTID, '^4')|
             (str_detect(V2_CONCEPTID, '^6') & !(V2_CONCEPTID %in% c('66AA.')))|
             str_detect(V2_CONCEPTID, '^7')|
             str_detect(V2_CONCEPTID, '^8')|
             (str_detect(V2_CONCEPTID, '^9') & !(V2_CONCEPTID %in% c('9....')))|
             str_detect(V2_CONCEPTID, '^K')|
             str_detect(V2_CONCEPTID, '^L')|
             (V2_CONCEPTID %in% c('C113.', 'M21yC', 'ZV6DA', 'ZV6DB'))) %>%
    rename(read_code = V2_CONCEPTID, read_term = Term)) %>%
  distinct(read_code, read_term)

################################################################################
#### Depression ####
depression_os <- read_csv("lists_in/OpenSafely/opensafely-depression-2020-07-09.csv") %>%
  rename(read_code = CTV3Code)

depression_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'DEPR_COD')) 
  
# depression_qof %>% 
#   select(contains('_v2_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# depression_qof %>% 
#   select(contains('_v3_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # v38 v3 has some extras

depression_qof <- depression_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

# all codes in CPRD, QOF and OpenSafely
# remove H/O and resolved
depression <- comorbs_data %>% 
  filter(cat2 %in% 'depression') %>%
  select(read_code, read_term_cprd = read_term) %>%
  full_join(depression_os, by = 'read_code') %>%
  full_join(depression_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_qof) ~ read_term_qof,
                               !is.na(Description) ~ Description, 
                               !is.na(read_term_cprd) ~ read_term_cprd,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term, QOF) %>%
  arrange(read_code, QOF) %>% 
  distinct(read_code, .keep_all = TRUE)
# keep resolved or deleted as this would still fit the 'ever' rule
  # filter(!str_detect(read_term, regex('resolve|H/O', ignore_case = TRUE)))

# check mapping
# map V2 -> CTV3
depression %>% map_V2_CTV3() %>% print(n=Inf)
# map CTV3 -> V2
depression %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

# XE1Xy	Mood disorder,
# X00SM	Bipolar disorder,
# X00SJ	Mania,
# XagO4	Depressive personality disorder

depression <- depression %>%
  bind_rows(tribble(~read_code, ~read_term,
                    'XaLG0',	'Depression resolved',
                    'XaItx',	'Depression management programme',
                    'XaK6d',	'Depression annual review',
                    'XaK6e',	'Depression medication review',
                    'XaK6f',	'Depression interim review',
                    'XaJWh',	'On depression register',
                    'XaK9p',	'Depression enhanced services administration',
                    'XaKAK',	'Depression enhanced service completed',
                    'XaPKm',	'On full dose long term treatment for depression',
                    'XaMGL',	'Depression monitoring administration',
                    'XaMGN',	'Depression monitoring first letter',
                    'XaMGO',	'Depression monitoring second letter',
                    'XaMGP',	'Depression monitoring third letter',
                    'XaMGQ',	'Depression monitoring verbal invite',
                    'XaMGR',	'Depression monitoring telephone invite',
                    'E204.', 'Neurotic (reactive) depression') %>%
              mutate(QOF = 'No'))

################################################################################
#### Anxiety ####
anxiety_os <- read_csv("lists_in/Elsie/elsie-horne-anxiety-2020-11-16T18-20-06.csv") %>%
  rename(read_code = id,
         read_term_os = term)
# no QOF codes for anxiety

anxiety <- comorbs_data %>%
  filter(cat2 %in% 'anxiety') %>%
  select(read_code, read_term_cprd = read_term) %>%
  full_join(anxiety_os, by = 'read_code') %>% 
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               TRUE ~ NA_character_)) %>%
  # add this as it seems appropriate
  add_row(read_code = '173f.',
          read_term = 'Anxiety about breathlessness') %>%
  select(read_code, read_term) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  # keep resolved or deleted as this would still fit the 'ever' rule
  # filter(!str_detect(read_term, regex('resolve|H/O|F/H', ignore_case = TRUE))) %>%
  mutate(QOF = 'No')

# check mapping
# map V2 -> CTV3
anxiety %>% map_V2_CTV3() %>% print(n=Inf)
# map CTV3 -> V2
anxiety %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

anxiety <- anxiety %>%
  bind_rows(tribble(~read_code, ~read_term,
                    'XaK2c',	'H/O: agoraphobia',
                    'XaIo7',	'C/O - panic attack',
                    'XaKVA',	'O/E - panic attack',
                    'XaKVN',	'O/E - fearful mood',
                    'XE1hk',	'Neurotic condition, insight present',
                    'XE1hl',	'Poor insight into neurotic condition',
                    'XE1YE',	'Neurotic disorder NOS',
                    'E2749', 'Nightmares',
                    '1Bb1.', 'Fear of getting cancer',
                    '8T23.', 'Refer fr psychl mgt of anxiety',
                    'Eu054', '[X]Organic anxiety disorder',
                    '1Bb0.', 'Fear of falling',
                    '8CAZ0', 'Patient given advice about management of anxiety') %>%
              mutate(QOF = 'No'))

# XabBS	Impaired insight,
# X00Sy	Weight fixation,
# E2112 Depressive personality disord,
# XagO4	Depressive personality disorder,

################################################################################
#### COPD ####
copd_cprd <- comorbs_data %>%
  filter(cat2 %in% 'copd')

copd_os <- read_csv("lists_in/OpenSafely/opensafely-current-copd-2020-05-06.csv") %>%
  select(read_code = CTV3ID, read_term_os = CTV3PreferredTermDesc)

copd_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>% 
  filter_at(vars(ends_with('id')), any_vars(. %in% 'COPD_COD')) 

# copd_qof %>% 
#   select(contains('_v2_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# copd_qof %>% 
#   select(contains('_v3_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

copd_qof <- copd_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

copd <- copd_cprd %>%
  select(read_code, read_term_cprd = read_term) %>%
  full_join(copd_os, by = 'read_code') %>%
  full_join(copd_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_qof) ~ read_term_qof,
                               TRUE ~ NA_character_
                               )) %>%
  select(read_code, read_term, QOF) %>%
  arrange(read_code, QOF) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  filter(!str_detect(read_term, 
                     regex('F/H', 
                           ignore_case = TRUE))) 
# keep resolved or deleted as this would still fit the 'ever' rule
  # filter(!str_detect(read_term, 
  #                    regex('resolve|history|H/O', 
  #                          ignore_case = TRUE))) 

copd <- copd %>%
  bind_rows(copd %>% map_CTV3_V2() %>% 
              filter(V2_CONCEPTID %in% c('14OX.', 'H581.') |
                       str_detect(V2_CONCEPTID, '^6') |
                       str_detect(V2_CONCEPTID, '^8') |
                       (str_detect(V2_CONCEPTID, '^9') & !(V2_CONCEPTID %in% c('9....')))) %>%
              rename(read_code = V2_CONCEPTID, read_term = Term),
            tribble(~read_code, ~read_term,
                    'XE2Pp', 'History of chronic obstructive pulmonary disease',
                    'XaIQh',	'Mediastinal emphysema',
                    'H581.', 'Interstitial emphysema')) %>%
  filter(!(read_code %in% c('X101k'))) 

#  XM1R4	H/O: bronchitis,
#  XM1Qc	Tension pneumatocele,

# check mapping
# map V2 -> CTV3
copd %>% map_V2_CTV3() %>% print(n=Inf)
# map CTV3 -> V2
copd %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

################################################################################
#### GORD ####
gord_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_gerd_dx_rms.txt",
                           delim = '|') %>%
  rename(read_term_rms = read_term)

gord_cprd <- comorbs_data %>%
  filter(cat2 %in% 'GORD') %>%
  select(read_code, read_term_cprd = read_term)

gord_os <- read_csv("lists_in/Elsie/elsie-horne-gord-2020-11-17T09-13-38.csv") %>%
  select(read_code = id, read_term_os = term) 

gord <- gord_cprd %>% 
  full_join(gord_rms, by = 'read_code') %>%
  full_join(gord_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_rms) ~ read_term_rms,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(QOF = 'No')

# check mapping
# map V2 -> CTV3
gord %>% map_V2_CTV3() %>% print(n=Inf)
# map CTV3 -> V2
gord %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

gord <- gord %>%
  add_row(read_code = 'X3009', read_term = 'Eosinophilic oesophagitis', QOF = 'No')

################################################################################
#### Nasal polyps ####
nasal_polyp_cprd <- comorbs_data %>%
  filter(cat2 %in% 'nasal_polyp') %>%
  select(read_code, read_term_cprd = read_term)

nasal_polyp_os <- read_csv("lists_in/Elsie/elsie-horne-nasal-polyp-2020-11-17T09-39-41.csv") %>%
  select(read_code = id, read_term_os = term) 

nasal_polyp <- nasal_polyp_cprd %>% 
  full_join(nasal_polyp_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(QOF = 'No')

# check mapping
# map V2 -> CTV3
nasal_polyp %>% map_V2_CTV3() %>% print(n=Inf)
# map CTV3 -> V2
nasal_polyp %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

nasal_polyp <- nasal_polyp %>%
  add_row(read_code = 'XaIwR', 
          read_term = 'Functional endoscopic sinus surgery - polypectomy of nasal sinus',
          QOF = 'No')

# ################################################################################
# #### Osteoporosis ####
# 
# osteoporosis_cprd <- comorbs_data %>%
#   filter(cat2 %in% 'osteoporosis') %>%
#   select(read_code, read_term_cprd = read_term)
# 
# osteoporosis_os <- read_csv("lists_in/Elsie/elsie-horne-osteoporosis-2020-11-17T09-50-06.csv") %>%
#   select(read_code = id, read_term_os = term) 
# 
# osteoporosis_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>% 
#   filter_at(vars(ends_with('id')), any_vars(. %in% 'OSTEO_COD')) 
# 
# # osteoporosis_qof %>% 
# #   select(contains('_v2_')) %>% 
# #   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
# #   print(n=nrow(.))
# # # all v2 the same
# # osteoporosis_qof %>% 
# #   select(contains('_v3_')) %>% 
# #   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
# #   print(n=nrow(.))
# # # all v3 the same
# 
# osteoporosis_qof <- osteoporosis_qof %>%
#   mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
#                                    !is.na(v38_v3_term) ~ v38_v3_term,
#                                    TRUE ~ NA_character_)) %>%
#   distinct(read_code, read_term_qof) %>%
#   mutate(QOF = 'Yes')
# 
# osteoporosis <- osteoporosis_cprd %>% 
#   full_join(osteoporosis_os, by = 'read_code') %>%
#   full_join(osteoporosis_qof, by = 'read_code') %>%
#   mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
#                                !is.na(read_term_os) ~ read_term_os,
#                                !is.na(read_term_qof) ~ read_term_qof,
#                                TRUE ~ NA_character_)) %>%
#   select(read_code, read_term, QOF) %>%
#   arrange(read_code, QOF) %>%
#   distinct(read_code, .keep_all = TRUE)
# 
# # check mapping
# # map V2 -> CTV3
# osteoporosis %>% map_V2_CTV3() %>% print(n=Inf)
# # map CTV3 -> V2
# osteoporosis %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

###############################################################################
comorbidities_all <- bind_rows(
  depression %>% mutate(cat2 = 'Depression'),
  anxiety %>% mutate(cat2 = 'Anxiety'),
  copd %>% mutate(cat2 = 'COPD'),
  diabetes %>% mutate(cat2 = 'Diabetes'),
  gord %>% mutate(cat2 = 'GORD'),
  nasal_polyp %>% mutate(cat2 = 'Nasal polyps'),
  CCD %>% mutate(cat2 = 'Chronic cardiac disease')) %>%
  mutate(cat1 = 'comorbidities') %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'No', .)))

# make sure all Read codes are 5 characters and fix if not
comorbidities_all %>%
  filter(str_length(read_code)<5)
comorbidities_all <- comorbidities_all %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

write_csv(comorbidities_all, 
          path = file.path(opcrd_analysis_path, 'comorbidities.csv'))
write_csv(comorbidities_all %>%
            arrange(cat2, read_code, read_term) %>%
            select(Disease = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   QOF) %>%
            mutate_at('QOF', list(~ str_extract(., '.{1}'))),
          path = 'lists_out/comorbidities.csv')
