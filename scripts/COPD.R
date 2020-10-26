#### COPD ####

# Description: copd
# cat1: comorbidity
# cat2: copd
# score: NA

source('setup.R')

copd_v3 <- read_csv("lists_in/OpenSafely/opensafely-chronic-respiratory-disease-2020-04-10.csv") %>% 
  filter(CTV3Source == 'CTV3Map_Code_Only' |
           CTV3Source == 'CTV3Map_Code_And_Term') %>%
  filter(str_detect(CTV3PreferredTermDesc,
                    regex('chronic obstructive pulmonary disease|copd', 
                          ignore_case = TRUE))) %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) 

CPRD_COPD <- read_cprd('copd') %>% 
  add_row(read_code = '66YM.',
          read_term = 'Chronic obstructive pulmonary disease annual review') %>%
  add_row(read_code = '66Yf.',	
          read_term = 'Number of chronic obstructive pulmonary disease exacerbations in past year') %>%
  add_row(read_code = '66YI.',
          read_term = 'Chronic obstructive pulmonary disease self-management plan given') %>%
  add_row(read_code = '66YB.',
          read_term  = 'Chronic obstructive pulmonary disease monitoring') 

copd_rms <- bind_rows(read_delim("lists_in/RMS/cl_copd_qof_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE),
                      read_delim("lists_in/RMS/cl_copd_review_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE)) 

copd <- bind_rows(copd_v3, CPRD_COPD, copd_rms) %>%
  mutate(cat1 = 'comorbidity', 
         cat2 = 'COPD',
         score = NA_real_) %>%
  distinct()

# check for duplicates in read_code
copd$read_code[duplicated(copd$read_code)]

copd <- copd %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'H3120' ~ 'Chronic asthmatic bronchitis',
                                          read_code %in% 'H32y1' ~ 'Atrophic (senile) emphysema',
                                          read_code %in% c('H3...', 'H3y..', 'H3z..') ~ str_replace(., 'airways', 'pulmonary'),
                                          TRUE ~ .))) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

saveRDS(copd, file = 'lists_out/COPD.RDS', compress = FALSE)
