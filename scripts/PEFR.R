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

# check for duplicates in read_code
pefr_codes$read_code[duplicated(pefr_codes$read_code)]

write_csv(pefr_codes, path = 'lists_out/PEFR.csv')

# table for appendix: PEFR
pefr_table <- pefr_codes %>%
  filter(!(cat2 %in% 'home_monitoring')) %>%
  mutate_at('cat2', list(~ case_when(. %in% 'predicted PEFR' ~ 'pPEFR',
                                     . %in% '% predicted PEFR' ~ 'ppPEFR',
                                     str_detect(., '% P') ~ str_replace(., '% P', '% ppP'),
                                     TRUE ~ .))) %>%
  mutate_at('cat2', factor, 
            levels = c('PEFR',
                       'pPEFR', 
                       'ppPEFR', 
                       '<60% ppPEFR', 
                       '60-80% ppPEFR',
                       '>80% ppPEFR', 
                       'post')) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'N', 'Y'))) %>%
  arrange(cat2, read_term) %>%
  select(Category = cat2,
         `Read code` = read_code, 
         Term = read_term,
         QOF) %>%
  mutate(order = as.numeric(Category)) %>%
  mutate_at('Category', as.character)

# split on value column
split_list <- pefr_table %>% 
  group_split(order)
new_col <- character()
# loop for removing duplicate labels
for (i in seq_along(unique(pefr_table$Category))) {
  tmp <- c(split_list[[i]]$Category[1], 
           rep('', length.out = length(split_list[[i]]$Category[-1])))
  new_col <- c(new_col, tmp)
}

# produce table
pefr_table <- pefr_table %>%
  mutate(Category = new_col) %>%
  arrange(order, Term) %>%
  select(-order)

pefr_table %>%
  xtable(caption = 'Read codes for identifiying PEFR events (see \\nameref{cha:ehr:methods:pre:PEFR} for methods)',
         label = 'tab:app:rc_pefr',
         align=c('l',"p{3cm}","p{2cm}","p{6cm}", "p{1cm}")) %>%
  print_xtable_multi(filename = 'pefr')

# table for appendix: PEFR home monitoring
home_table <- pefr_codes %>%
  filter(cat2 %in% 'home_monitoring') %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term,
         QOF) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'N', 'Y'))) %>%
  xtable(caption = 'Read codes indicating PEFR home monitoring (see \\nameref{cha:ehr:methods:pre:PEFRhome} for methods)',
         label = 'tab:app:rc_pefr_home',
         align=c('l',"p{2cm}","p{9cm}", "p{1cm}")) %>%
  print_xtable_multi(filename = 'pefr_home')
