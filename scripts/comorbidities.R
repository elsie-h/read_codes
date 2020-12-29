
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
             'diabetes',
             'GORD',
             'nasal_polyp',
             'osteoporosis',
             'hypertension',
             'thyroid')

#### from CPRD codes: (Kuan 2019) ####
comorbs_data <- bind_rows(lapply(comorbs, 
                                 function(x) 
                                   read_cprd(x) %>% 
                                   mutate(cat1 = 'comorbidity',
                                          cat2 = x,
                                          version = 2))) 



################################################################################
#### Cardiovascular disease ####

# CVD
cvd_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_cardiovascular_disease_dx_rms.txt",
           delim = '|')

# Ischaemic heart disease

ihd_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_ihd_dx_rms.txt",
                      delim = '|')

# Heart failure

hf_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_heart_failure_dx_rms.txt",
                      delim = '|')

# Coronary Heart Disease 
CVD_qof <- read_csv('lists_in/QOF/QOF_codes.csv')  %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% c('CHD_COD', 'AFIB_COD', 'HF_COD', 'HFLVSD_COD'))) 

# CVD_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# CVD_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # one absent from v36 so use later version

CVD_qof <- CVD_qof %>%
  mutate(read_term_qof = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term_qof) %>%
  mutate(QOF = 'Yes')

CVD <- cvd_rms %>% 
  rename(cvd = read_term) %>% 
  full_join(ihd_rms %>% rename(ihd = read_term), by = 'read_code') %>% 
  full_join(hf_rms %>% rename(hf = read_term), by = 'read_code') %>%
  full_join(CVD_qof, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(cvd) ~ cvd,
                               !is.na(ihd) ~ ihd,
                               !is.na(hf) ~ hf,
                               !is.na(read_term_qof) ~ read_term_qof,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term, QOF) 

################################################################################
#### Thyroid ####

# use this existing V2 codelist as guide
thyroid_v2 <- comorbs_data %>%
  filter(cat2 %in% 'thyroid') %>%
  mutate_at('cat2', list(~ case_when(str_detect(read_code, '^C00|^C01|^C02') ~ 'hyperthyroidism',
                                     str_detect(read_code, '^C03|^C04') ~ 'hypothyroidism',
                                     str_detect(read_term, regex('hyper|thyrotoxic|grave|basedow|exophthalmos', ignore_case = TRUE)) ~ 'hyperthyroidism',
                                     str_detect(read_term, regex('hypo|myxoedema|hashimoto', ignore_case = TRUE)) ~ 'hypothyroidism',
                                     read_term %in% c('TSH - thyroid-stimulating hormone deficiency',
                                                      'Thyroid atrophy') ~ 'hypothyroidism',
                                     TRUE ~ 'thyroid disorder'))) %>%
  rename(read_term_v2 = read_term)

# I created this codelist using te OpenSafely builder. It is over-inclusive so will have to be trimmed down.
thyroid_os <- read_csv("lists_in/OpenSafely/elsie-horne-thyroid-disorder-5bb415af.csv") %>%
  rename(read_code = code,
         read_term_os = term) %>%
  filter(!str_detect(read_term_os, regex('malignant|tumour|carcinoma|neoplasm|nodule|lump|infection|adenoma|metastasis', ignore_case = TRUE))) %>%
  filter(!(read_code %in% c('4421.'))) %>%
  filter(!str_detect(read_code, '^Xac9'))

tmp <- thyroid_v2 %>%
  full_join(thyroid_os, by = 'read_code') %>%
  # flag the CPRD codes with v2
  mutate(v2 = if_else(!is.na(cat1),
                      1, 0),
         # flag the OpenSafely codes that are also in the CPRD list with os_v2
         os_v2 = if_else(!is.na(read_term_os) & !is.na(cat1),
                                   1, 0),
         read_term = if_else(!is.na(read_term_v2),
                             read_term_v2,
                             read_term_os)) %>%
  select(read_code, read_term, v2, os_v2) %>%
  mutate_at('read_term', tolower) %>%
  mutate_at('read_term', list(~ str_replace(., 'first', '1st'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'second', '2nd'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'third', '3rd'))) %>%
  # split such as 1st element is version 3 codes and second element version 2
  group_split(v2)

# check whether there are any version 2 code sin the OpenSafely codelist that are not in the COPD one
tmp_joined <- tmp[[2]] %>%
  select(-v2) %>%
  rename(read_code_v2 = read_code) %>%
  full_join(tmp[[1]] %>%
              select(-v2, -os_v2) %>%
              rename(read_code_v3 = read_code),
            by = 'read_term')

# identify the V2 codes that are in the OS codelist but not CPRD
remove_term_list <- tmp[[1]] %>%
  filter(!str_detect(read_code, '^X|^Y')) %>%
  select(read_code, read_term) 

# do these correspond to any V3 codes?
# tmp[[1]] %>%
#   anti_join(select(remove_term_list, read_code)) %>%
#   right_join(select(remove_term_list, read_term)) %>%
#   print(n = nrow(.))
# #  nope
# # so just remove the version 2 ones

thyroid <- tmp_joined %>%
  anti_join(select(remove_term_list, read_code), 
            by = c('read_code_v3' = 'read_code')) %>%
  arrange(read_code_v2, read_code_v3) %>%
  mutate(cat2 = case_when(str_detect(read_code_v2, '^C00|^C01|^C02') ~ 'hyperthyroidism',
                          str_detect(read_code_v2, '^C03|^C04') ~ 'hypothyroidism',
                          str_detect(read_term, regex('hyper|thyrotoxic|grave|basedow|exophthalmos|eye|hashitoxicosis', ignore_case = TRUE)) ~ 'hyperthyroidism',
                          str_detect(read_term, regex('hypo|myxoedema|hashimoto|struma lymphomatosis|thyroxine rx|tsh deficiency|iodine deficiency syndrome|atrophy', ignore_case = TRUE)) ~ 'hypothyroidism',
                          read_term %in% c('tsh - thyroid-stimulating hormone deficiency') ~ 'hypothyroidism',
                          str_detect(read_term, regex('thyroiditis|goitre|^thyroid', ignore_case = TRUE)) ~ 'thyroid disorder',
                          read_term %in% c('disorder of thyroid gland',
                                           'h/o: thyroid disorder',
                                           'h/o: thyroid disorder nos') ~ 'thyroid disorder',
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(cat2))

thyroid_final <- thyroid %>%
  select(read_code = read_code_v2,
         read_term, cat2) %>%
  filter(!is.na(read_code)) %>%
  bind_rows(thyroid %>%
              select(read_code = read_code_v3,
                     read_term, cat2) %>%
              filter(!is.na(read_code)))

# I think it's fairly usual for patients to go hyper -> hypo,
# but as far as I'm aware patients don't usually go hypo -> hyper
# check this with clinician and check in results

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
  bind_rows(diabetes_os, diabetes_cprd, diabetes_rms) %>%
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
  select(read_code, read_term)

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
gord_rms <- read_delim(file = "/Users/elsiehorne/Docs/read_codes/lists_in/RMS/cl_gerd_dx_rms.txt",
                           delim = '|') %>%
  rename(read_term_rms = read_term)

gord_cprd <- comorbs_data %>%
  filter(cat2 %in% 'GORD') %>%
  select(read_code, read_term_cprd = read_term)

gord_os <- read_csv("lists_in/OpenSafely/elsie-horne-gord-2020-11-17T09-13-38.csv") %>%
  select(read_code = id, read_term_os = term) 

gord <- gord_cprd %>% 
  full_join(gord_rms, by = 'read_code') %>%
  full_join(gord_os, by = 'read_code') %>%
  mutate(read_term = case_when(!is.na(read_term_cprd) ~ read_term_cprd,
                               !is.na(read_term_os) ~ read_term_os,
                               !is.na(read_term_rms) ~ read_term_rms,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
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


comorbidities_all <- bind_rows(
  depression %>% mutate(cat2 = 'Depression'),
  anxiety %>% mutate(cat2 = 'Anxiety'),
  copd %>% mutate(cat2 = 'COPD'),
  diabetes %>% mutate(cat2 = 'Diabetes'),
  gord %>% mutate(cat2 = 'GORD'),
  # hypertension %>% mutate(cat2 = 'Hypertension'),
  nasal_polyp %>% mutate(cat2 = 'Nasal polyps'),
  # osteoporosis %>% mutate(cat2 = 'Osteoporosis'),
  # thyroid_final,
  CVD %>% mutate(cat2 = 'Cardiovascular disease')) %>%
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

