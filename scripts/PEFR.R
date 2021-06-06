#### PEFR ####

# Description: codes suggesting the patient had their PEFR measured
# cat1: asthma_review
# cat2: 
# score:

source('setup.R')

pefr_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
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
  # remove any that are also in the QOF list
  anti_join(pefr_qof, by = 'read_code') %>%
  mutate(Manual = 1)

# bind Elsie list and QOF list
pefr_codes <- bind_rows(pefr_qof, pefr_manual)

# make sure all Read codes are 5 characters and fix if not
pefr_codes %>%
  filter(str_length(read_code)<5)
pefr_codes <- pefr_codes %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
pefr_codes$read_code[duplicated(pefr_codes$read_code)]

# check mapping
# map V2 -> CTV3
pefr_codes %>% map_V2_CTV3() %>% arrange(CTV3_CONCEPTID) %>% print(n=Inf)
# map CTV3 -> V2
pefr_codes %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

pefr_codes <- pefr_codes %>%
  add_row(read_code = 'X77RP',
          read_term = 'Less than 60% of predicted peak expiratory flow rate',
          cat2 = '<60% PEFR') %>%
  add_row(read_code = 'X77RQ',
          read_term = 'Less than 60% of predicted peak expiratory flow rate',
          cat2 = '60-80% PEFR') %>%
  add_row(read_code = 'X77RR',
          read_term = 'More than 80% of predicted peak expiratory flow rate',
          cat2 = '>80% PEFR') %>%
  add_row(read_code = 'XE2ws',
          read_term = 'Peak flow rate after bronchodilation',
          cat2 = 'post') %>%
  add_row(read_code = 'XE2xQ',
          read_term = 'Peak flow rate before bronchodilation',
          cat2 = 'PEFR') %>%
  mutate(cat1 = 'PEFR')

write_csv(pefr_codes, 
          path = file.path(opcrd_analysis_path, 'PEFR.csv'))
write_csv(pefr_codes %>%
            filter(cat2 %in% 'home_monitoring') %>%
            arrange(read_code) %>%
            select(`Read code` = read_code,
                   Term = read_term) %>%
            mutate(QOF = 'Y'), 
          path = 'lists_out/PEFR_home.csv')
write_csv(pefr_codes %>%
            filter(!(cat2 %in% 'home_monitoring')) %>%
            mutate_at('cat2', factor,
                      levels = c('PEFR', 'post', 
                                 'predicted PEFR', '% predicted PEFR', 
                                 '<60% PEFR', '60-80% PEFR', '>80% PEFR')) %>%
            arrange(cat2, read_code) %>%
            select(Category = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   QOF) %>%
            mutate_at('QOF', list(~ case_when(. %in% 1 ~ 'Y',
                                              TRUE ~ 'N'))), 
          path = 'lists_out/PEFR.csv')
