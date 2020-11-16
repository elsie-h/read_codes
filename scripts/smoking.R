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

smoking_QOF <- readRDS("lists_in/QOF/QOF_codes.RDS") %>%
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

# check for duplicates in read_code
smoking$read_code[duplicated(smoking$read_code)]

saveRDS(smoking, file = 'lists_out/smoking.RDS', compress = FALSE)

# latex tables
smoking_list <- smoking %>%
  select(-score) %>%
  group_split(cat2)

smoking_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} status (return to \\nameref{cha:ehr:methods:pre:smoking} methods)')
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

lapply(smoking_list, smoking_latex_table)
