
source('setup.R')

#### Comorbidities ####

# I want code lists for the following comorbidities:
# Eczema
# Rhinitis
# Anaphylaxis

# Anxiety
# Depression
# GORD
# Nasal polyps
# Osteoporosis


comorbs <- c('anxiety',
             'copd',
             'depression',
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


#### Depression ####
depression_os <- read_csv("lists_in/OpenSafely/opensafely-depression-2020-07-09.csv") %>%
  rename(read_code = CTV3Code)

depression_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'DEPR_COD')) 
  
depression_qof %>% 
  select(contains('_v2_')) %>% 
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# all v2 the same
depression_qof %>% 
  select(contains('_v3_')) %>% 
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# v38 v3 has some extras

depression_qof <- depression_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof)

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
  distinct(read_code, read_term) %>%
  filter(!str_detect(read_term, regex('resolve|H/O', ignore_case = TRUE)))
  
#### Anxiety ####
comorbs_data %>% 
  filter(cat2 %in% 'anxiety') %>%
  print(n=100)
# search opensafely to match this list

anxiety_os <- read_csv("lists_in/OpenSafely/elsie-horne-anxiety-2020-11-16T18-20-06.csv") %>%
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
  distinct(read_code, read_term) %>%
  filter(!str_detect(read_term, regex('resolve|H/O', ignore_case = TRUE)))

#### COPD ####
copd_cprd <- comorbs_data %>%
  filter(cat2 %in% 'copd')

copd_os <- read_csv("lists_in/OpenSafely/opensafely-current-copd-2020-05-06.csv") %>%
  select(read_code = CTV3ID, read_term_os = CTV3PreferredTermDesc)

copd_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>% 
  filter_at(vars(ends_with('id')), any_vars(. %in% 'COPD_COD')) 

copd_qof %>% 
  select(contains('_v2_')) %>% 
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# all v2 the same
copd_qof %>% 
  select(contains('_v3_')) %>% 
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# all v3 the same

copd_qof <- copd_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof)

copd <- copd_cprd %>%
  select(read_code, read_term_cprd = read_term) %>%
  full_join(copd_os, by = 'read_code') %>%
  full_join(copd_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_qof) ~ read_term_qof,
                               TRUE ~ NA_character_
                               )) %>%
  select(read_code, read_term) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  filter(!str_detect(read_term, 
                     regex('resolve|history|H/O', 
                           ignore_case = TRUE)))


#### add RMS codes if available ####
# anxiety and depression grouped together in RMS codes, so I categorise them here
anxiety_depression_rms <- read_delim("lists_in/RMS/cl_anxiety_depression_dx_rms.txt",
                                     "|", 
                                     escape_double = FALSE, 
                                     trim_ws = TRUE)  %>%
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
                                     TRUE ~ NA_character_))) %>%
  filter(!is.na(cat2))

# nasal polyps
nasal_polyps_rms <- read_delim("lists_in/RMS/cl_nasal_polyps_rms.txt", 
                               "|", 
                               escape_double = FALSE, 
                               trim_ws = TRUE)  %>%
  mutate(cat1 = 'comorbidity', cat2 = 'nasal_polyp')

# rhinitis
rhinitis_rms <- read_delim("lists_in/RMS/cl_rhinitis_dx_rms.txt", 
                           "|", 
                           escape_double = FALSE, 
                           trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'rhinitis') 

# GERD
gerd_rms <- read_delim("lists_in/RMS/cl_gerd_dx_rms.txt", 
                       "|", 
                       escape_double = FALSE,
                       trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'GORD')

rhinitis <- comorbs_data %>%
  filter(cat2 %in% 'rhinitis') %>%
  full_join(rhinitis_rms, by = c('read_term')) %>%
  select(read_code_CPRD = read_code.x,
         read_code_RMS = read_code.y,
         read_term) %>%
  filter(!(read_term %in% 'FH: Hay fever'))


comorbs_all <- bind_rows(comorbs_data,
                         anxiety_depression_rms,
                         nasal_polyps_rms,
                         rhinitis_rms,
                         gerd_rms) %>%
  distinct()

#### from Mukherjee 2016 allergy paper ####
# https://onlinelibrary.wiley.com/doi/abs/10.1111/all.12928
mukherjee <- readxl::read_excel("lists_in/Mukherjee2016/Mukherjee2016_supinfo.xlsx", 
                          skip = 1)

anaphylaxis <- mukherjee %>% filter(`Disease Area` %in% 'Anaphylaxis')

tmp %>% filter(`Disease Area` %in% 'Rhinitis')