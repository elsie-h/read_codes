#### Smoking ####

# Description: smoking status
# cat1: smoking_status
# cat2: never (1), current (2), ex (3)
# score: 1,2,3

source('setup.R')

# Read Nwaru categories
source('lists_in/Nwaru2020/smoking_status.R')

smoking_nwaru <- tibble(read_code = c(current_smoker, 
                                      ex_smoker, 
                                      non_smoker)) %>%
  mutate(nwaru = 1,
         cat_nwaru= case_when(read_code %in% current_smoker ~ 'current smoker',
                              read_code %in% ex_smoker ~ 'ex-smoker',
                              read_code %in% non_smoker ~ 'non-smoker',
                              read_code %in% passive_smoking ~ 'passive smoking',
                              TRUE ~ NA_character_))

smoking_QOF <- read_csv("lists_in/QOF/QOF_codes.csv") %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'SMOK_COD')) %>%
  mutate(read_term_QOF = if_else(is.na(v38_v2_term), v38_v3_term, v38_v2_term)) %>%
  distinct(read_code, read_term_QOF) %>%
  mutate(QOF = 1,
         cat_QOF = case_when(read_code %in% current_smoker ~ 'current smoker',
                             read_code %in% ex_smoker ~ 'ex-smoker',
                             read_code %in% non_smoker ~ 'non-smoker',
                             read_code %in% passive_smoking ~ 'passive smoking',
                             TRUE ~ NA_character_)) 

# manually add some extras that I've found
smoking_elsie <- tribble(~read_code, ~read_term_elsie, ~cat_elsie,
                         '137L.', "Current non-smoker", 'non-smoker',
                         '137W.', "Chews tobacco", 'current smoker',
                         '137g.', "Cigarette pack-years", 'current smoker',
                         'Ub0oq', "Non-smoker", 'non-smoker',
                         'Ub0pT', "Chews tobacco", 'current smoker',
                         'XaIuQ', "Cigarette pack-years", 'current smoker',
                         '8IAj.', "Smoking cessation advice declined", 'current smoker') %>%
  distinct(read_code, read_term_elsie, cat_elsie) 

# For the following Read codes, I think the list from Nwaru has substituted 
# the 'I's with 'l's. None of the codes with 'l's could be found in the  NHS
# Read code browser, apart from 'Ub1tl' which returned 'Savoury food intake'
# Therefore I have removed them from the list and added the following:
smoking_errors <- tribble(~read_code, ~read_term_error, ~cat_error,
                  'XaIIu', 'Smoking reduced', 'current smoker',
                  'XaItg', 'Reason for restarting smoking', 'current smoker',
                  'XaIkY', 'Not interested in stopping smoking', 'current smoker',
                  'XaIkW', 'Thinking about stopping smoking', 'current smoker',
                  'Ub1tI', 'Cigarette consumption', 'current smoker')

smoking <- smoking_QOF %>%
  full_join(smoking_nwaru, by = 'read_code') %>%
  full_join(smoking_elsie, by = 'read_code') %>%
  full_join(smoking_errors, by = 'read_code') %>%
  mutate(read_term_nwaru = case_when(
    read_code %in% c('XaFvq', '137U.') ~	'Not a passive smoker',
    read_code %in% c('XaQzw', '137K0') ~	'Recently stopped smoking',
    TRUE ~ NA_character_
  )) %>%
  mutate(read_term = case_when(!is.na(read_term_QOF) ~ read_term_QOF,
                               !is.na(read_term_nwaru) ~ read_term_nwaru,
                               !is.na(read_term_elsie) ~ read_term_elsie,
                               !is.na(read_term_error) ~ read_term_error,
                               TRUE ~ NA_character_),
         cat2 = case_when(!is.na(cat_QOF) ~ cat_QOF,
                          !is.na(cat_nwaru) ~ cat_nwaru,
                          !is.na(cat_elsie) ~ cat_elsie,
                          !is.na(cat_error) ~ cat_error,
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(read_term)) %>%
  distinct(read_code, read_term, cat2, QOF) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% c('XaIkX',
                                                      'XE0or',
                                                      'XagO3')
                                     ~ 'current smoker',
                                     TRUE ~ .))) %>%
  mutate(score = case_when(cat2 %in% 'non-smoker' ~ 1,
                           cat2 %in% 'current smoker' ~ 2,
                           cat2 %in% 'ex-smoker' ~ 3,
                           TRUE ~ NA_real_)) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'No', 'Yes'))) %>%
  distinct(read_code, read_term, cat2, score, QOF)

# make sure all Read codes are 5 characters and fix if not
smoking %>%
  filter(str_length(read_code)<5)
smoking <- smoking %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
smoking$read_code[duplicated(smoking$read_code)]

# check mapping
# map V2 -> CTV3
smoking %>% map_V2_CTV3() %>% arrange(CTV3_CONCEPTID) %>% print(n=Inf)
# map CTV3 -> V2
smoking %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

smoking <- smoking %>%
  add_row(read_code = 'XaRFh', 
          read_term = 'Smoking cessation advice declined',
          cat2 = 'current smoker', score = 2)

write_csv(smoking, 
          path = file.path(opcrd_analysis_path, 'smoking.csv'))
write_csv(smoking %>%
            arrange(cat2, read_code) %>%
            select(Category = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   QOF) %>%
            mutate_at('QOF', list(~ str_extract(., '.{1}'))), 
          path = 'lists_out/smoking.csv')
