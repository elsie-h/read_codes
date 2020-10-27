#### PEFR ####

# Description: codes suggesting the patient had their PEFR measured
# cat1: asthma_review
# cat2: 
# score:

source('setup.R')

pefr_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('_id')), any_vars(. %in% 'PEFR_COD')) %>%
  mutate(read_term = case_when(!is.na(v36_v2_term) ~ v36_v2_term,
                               !is.na(v36_v3_term) ~ v36_v3_term,
                               !is.na(v37_v2_term) ~ v37_v2_term,
                               !is.na(v37_v3_term) ~ v37_v3_term,
                               !is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  mutate(cat1 = 'PEFR',
         cat2 = case_when(read_code %in% c('33950',
                                           '339g.',
                                           '339n.',
                                           '66Yc.',
                                           '66YX.',
                                           '66YY.',
                                           'X77RW',
                                           'XaIxD',
                                           'XaIxP',
                                           'XaJEg',
                                           'XaJvW',
                                           'XaXHh') ~ 'home_monitoring',
                          read_code %in% c('339A.',
                                           '339c.',
                                           'XaEHe',
                                           'XaIxS') ~ 'PEFR',
                          TRUE ~ 'post'),
         QOF = 1) 



pefr_manual <- read_csv("lists_in/Elsie/pef_codes_elsie.csv") %>%
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
  mutate(cat1 = 'pefr') %>%
  anti_join(pefr_qof, by = 'read_code') %>%
  mutate(Manual = 1)

pefr_codes <- bind_rows(pefr_qof, pefr_manual)

# check for duplicates in read_code
pefr_codes$read_code[duplicated(pefr_codes$read_code)]

saveRDS(pefr_codes, file = 'lists_out/PEFR.RDS', compress = FALSE)
