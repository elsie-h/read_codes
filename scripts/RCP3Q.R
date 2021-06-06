#### RCP3Q ####

# Description: codes suggesting the patient was asked (at least one of) the Royal College of Physicians 3 Questions
# cat1: asthma_review
# cat2: general; sleeping; day; activities;
# score: NA; 0; 1

source('setup.R')

rcp_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
  filter_at(vars(ends_with('_id')), any_vars(str_detect(., 'RCP')))

# rcp_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# rcp_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

rcp_qof <- rcp_qof %>%
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

rcp_elsie <- read_delim("lists_in/Elsie/rcp3q_elsie",
                         ",",
                         escape_double = FALSE, 
                         trim_ws = TRUE) 

rcp3q <- bind_rows(rcp_qof, rcp_elsie) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'No', .))) %>%
  mutate(cat1 = 'RCP3Q')

# make sure all Read codes are 5 characters and fix if not
rcp3q %>%
  filter(str_length(read_code)<5)
rcp3q <- rcp3q %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
rcp3q$read_code[duplicated(rcp3q$read_code)]

# check mapping
# map V2 -> CTV3
rcp3q %>% map_V2_CTV3() %>% arrange(CTV3_CONCEPTID) %>% print(n=Inf)
# map CTV3 -> V2
rcp3q %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)


write_csv(rcp3q, 
          path = file.path(opcrd_analysis_path, 'RCP3Q.csv'))
write_csv(rcp3q %>%
            mutate_at('cat2', list(~ factor(case_when(. %in% 'general' ~ 'numeric',
                                                      TRUE ~ .),
                                            levels = c('numeric', 'activities', 'day', 'sleep')))) %>%
           arrange(cat2, score, read_code) %>%
           select(Category = cat2,
                  `Read code` = read_code,
                  Term = read_term,
                  Score = score,
                  QOF) %>%
           mutate_at('QOF', list(~ str_extract(., '.{1}'))), 
         path = 'lists_out/RCP3Q.csv')