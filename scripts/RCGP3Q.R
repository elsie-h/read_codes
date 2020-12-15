#### RCGP3Q ####

# Description: codes suggesting the patient was asked (at least one of) the Royal College of GPs 3 Questions
# cat1: asthma_review
# cat2: general; sleeping; day; activities;
# score: NA; 0; 1

source('setup.R')

rcgp_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
  filter_at(vars(ends_with('_id')), any_vars(str_detect(., 'RCP')))

# rcgp_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# rcgp_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

rcgp_qof <- rcgp_qof %>%
  mutate(read_term = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_),
         cat2 = case_when(!is.na(v38_v2_id) ~ v38_v2_id,
                          !is.na(v38_v3_id) ~ v38_v3_id,
                          TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term, cat2) %>%
  mutate_at('cat2', list(~ case_when(. %in% 'RCPDSYMP_COD' ~ 'day',
                                     . %in% 'RCPEXCER_COD' ~ 'activities',
                                     . %in% 'RCPSLP_COD' ~ 'sleep',
                                     TRUE ~ NA_character_))) %>%
  mutate(QOF = 'Yes',
         score = if_else(str_detect(read_term, ' not | never '), 0L, 1L)) 

rcgp_elsie <- read_delim("lists_in/Elsie/rcgp3q_elsie",
                         ",",
                         escape_double = FALSE, 
                         trim_ws = TRUE) 

rcgp3q <- bind_rows(rcgp_qof, rcgp_elsie) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'No', .))) %>%
  mutate(cat1 = 'RCGP3Q')

# check for duplicates in read_code
rcgp3q$read_code[duplicated(rcgp3q$read_code)]

write_csv(rcgp3q, path = 'lists_out/RCGP3Q.csv')

# latex tables
rcgp3q_list <- rcgp3q %>%
  group_split(cat2)

rcgp3q_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  label <- str_c('tab:app:rc_', category_)
  
  if (category == 'general') {
    caption <- str_c('Read codes for RCGP 3 questions score (return to \\nameref{cha:ehr:methods:pre:rcgp3q} methods)')
    table <- .data %>%
      arrange(read_term) %>%
      select(`Read code` = read_code, 
             `Term` = read_term) %>%
      xtable(caption = caption,
             label = label,
             align=c('l',"p{2cm}","p{10cm}")) %>%
      print_xtable_multi(filename = category_)
  } else {
    caption <- str_c('Read codes for \\emph{\'', category, '\'} questions (return to \\nameref{cha:ehr:methods:pre:rcgp3q} methods)')
    table <- .data %>%
      arrange(score, read_term) %>%
      select(`Read code` = read_code, 
             `Term` = read_term,
             score) %>%
      xtable(caption = caption,
             label = label,
             align=c('l',"p{2cm}","p{8cm}", "p{2cm}")) %>%
      print_xtable_multi(filename = category_)
  }
}

lapply(rcgp3q_list, rcgp3q_latex_table)

