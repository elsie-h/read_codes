#### Smoking ####

# Description: smoking status
# cat1: smoking_status
# cat2: never (1), current (2), ex (3)
# score: 1,2,3

source('setup.R')

smoking <- read_delim("lists_in/RMS/cl_smoking_rms.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  # manually add some extras that I've found in the data
  add_row(read_code = 'Xa1bv', read_term = 'Ex-cigarette smoker', Category = 3) %>%
  add_row(read_code = '137j.', read_term = '	Ex-cigarette smoker', Category = 3) %>%
  add_row(read_code = '8IAj.', read_term = 	'Smoking cessation advice declined', Category = 2) %>%
  mutate(cat1 = 'smoking',
         score = Category,
         cat2 = case_when(score %in% 1 ~ 'never smoker',
                          score %in% 2 ~ 'current smoker',
                          score %in% 3 ~ 'ex-smoker')) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
smoking$read_code[duplicated(smoking$read_code)]

saveRDS(smoking, file = 'lists_out/smoking.RDS', compress = FALSE)
