
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
             'osteoporosis',
             'hypertension')

#### from CPRD codes: (Kuan 2019) ####
comorbs_data <- bind_rows(lapply(comorbs, 
                                 function(x) 
                                   read_cprd(x) %>% 
                                   mutate(cat1 = 'comorbidity',
                                          cat2 = x,
                                          version = 2))) 


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

diabetes <- diabetes_qof %>%
  rename(read_term = read_term_qof)

################################################################################
#### Coronary Heart Disease ####
CHD_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'CHD_COD')) 

# CHD_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# CHD_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

CHD_qof <- CHD_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

CHD <- CHD_qof %>%
  rename(read_term = read_term_qof)

################################################################################
#### Hypertension ####
hypertension_os <- read_csv("lists_in/OpenSafely/opensafely-hypertension-2020-04-28.csv") %>%
  filter(CTV3Source %in% c('CTV3Map_Code_And_Term',
                           'QOF',
                           'CTV3Map_Code_Only',
                           'CTV3_Children')) %>%
  select(read_code = CTV3ID, Description = CTV3PreferredTermDesc)

hypertension_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'HYP_COD')) 

# hypertension_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# hypertension_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

hypertension_qof <- hypertension_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

# all codes in CPRD, QOF and OpenSafely
# remove H/O and resolved
hypertension <- comorbs_data %>% 
  filter(cat2 %in% 'hypertension') %>%
  select(read_code, read_term_cprd = read_term) %>%
  full_join(hypertension_os, by = 'read_code') %>%
  full_join(hypertension_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_qof) ~ read_term_qof,
                               !is.na(Description) ~ Description, 
                               !is.na(read_term_cprd) ~ read_term_cprd,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term, QOF) %>%
  filter(!str_detect(read_term, regex('resolve|H/O|deleted', ignore_case = TRUE)))

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
  distinct(read_code, read_term, QOF) %>%
  filter(!str_detect(read_term, regex('resolve|H/O', ignore_case = TRUE)))

################################################################################
#### Anxiety ####
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
  filter(!str_detect(read_term, regex('resolve|H/O|F/H', ignore_case = TRUE))) %>%
  mutate(QOF = 'No')

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
  distinct(read_code, .keep_all = TRUE) %>%
  filter(!str_detect(read_term, 
                     regex('resolve|history|H/O|F/H', 
                           ignore_case = TRUE))) 

################################################################################
#### GORD ####
gord_cprd <- comorbs_data %>%
  filter(cat2 %in% 'GORD') %>%
  select(read_code, read_term_cprd = read_term)

gord_os <- read_csv("lists_in/OpenSafely/elsie-horne-gord-2020-11-17T09-13-38.csv") %>%
  select(read_code = id, read_term_os = term) 

gord <- gord_cprd %>% 
  full_join(gord_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  # add these version 2 read codes as they're included in the version 3 hierarchy
  add_row(read_code = 'J10y6', read_term = 'Barrett\'s oesophagus') %>%
  add_row(read_code = 'J1025', read_term = 'Barrett\'s ulcer of oesophagus') %>%
  mutate(QOF = 'No')

################################################################################
#### Nasal polyps ####
nasal_polyp_cprd <- comorbs_data %>%
  filter(cat2 %in% 'nasal_polyp') %>%
  select(read_code, read_term_cprd = read_term)

nasal_polyp_os <- read_csv("lists_in/OpenSafely/elsie-horne-nasal-polyp-2020-11-17T09-39-41.csv") %>%
  select(read_code = id, read_term_os = term) 

nasal_polyp <- nasal_polyp_cprd %>% 
  full_join(nasal_polyp_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  mutate(QOF = 'No')

################################################################################
#### Osteoporosis ####

osteoporosis_cprd <- comorbs_data %>%
  filter(cat2 %in% 'osteoporosis') %>%
  select(read_code, read_term_cprd = read_term)

osteoporosis_os <- read_csv("lists_in/OpenSafely/elsie-horne-osteoporosis-2020-11-17T09-50-06.csv") %>%
  select(read_code = id, read_term_os = term) 

osteoporosis_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>% 
  filter_at(vars(ends_with('id')), any_vars(. %in% 'OSTEO_COD')) 

# osteoporosis_qof %>% 
#   select(contains('_v2_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# osteoporosis_qof %>% 
#   select(contains('_v3_')) %>% 
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

osteoporosis_qof <- osteoporosis_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

osteoporosis <- osteoporosis_cprd %>% 
  full_join(osteoporosis_os, by = 'read_code') %>%
  full_join(osteoporosis_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_qof) ~ read_term_qof,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term, QOF)


comorbidities_all <- bind_rows(depression %>% mutate(cat2 = 'Depression'),
                               anxiety %>% mutate(cat2 = 'Anxiety'),
                               CHD %>% mutate(cat2 = 'CHD'),
                               copd %>% mutate(cat2 = 'COPD'),
                               diabetes %>% mutate(cat2 = 'diabetes'),
                               gord %>% mutate(cat2 = 'GORD'),
                               hypertension %>% mutate(cat2 = 'Hypertension'),
                               nasal_polyp %>% mutate(cat2 = 'Nasal polyps'),
                               osteoporosis %>% mutate(cat2 = 'Osteoporosis')) %>%
  mutate(cat1 = 'comorbidities') %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'No', .)))

write_csv(comorbidities_all, path = 'lists_out/comorbidities.csv')

# latex tables
comorbidities_list <- comorbidities_all %>%
  group_split(cat2)

comorbidities_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} (return to \\nameref{cha:ehr:methods:pre:comorbidities} methods)')
  label <- str_c('tab:app:rc_', category_)
  
  table <- .data %>%
    arrange(read_term) %>%
    select(`Read code` = read_code, 
           `Term` = read_term,
           QOF) %>%
    xtable(caption = caption,
           label = label,
           align=c('l',"p{2cm}","p{8cm}", 'p{2cm}')) %>%
    print_xtable_multi(filename = category_)
  
}

lapply(comorbidities_list, comorbidities_latex_table)

