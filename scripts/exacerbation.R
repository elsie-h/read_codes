#### Asthma exacerbation ####

# Description: asthma exacerbation
# cat1: exacerbation
# cat2: NA; number;
# score: NA

source('setup.R')

exacerbation <- read_delim("lists_in/Elsie/cl_exacerbation_elsie",
                           ",",
                           escape_double = FALSE, 
                           trim_ws = TRUE) %>%
  mutate(score = NA_real_) %>%
  distinct()

# check for duplicates in read_code
exacerbation$read_code[duplicated(exacerbation$read_code)]

exacerbation <- exacerbation %>%
  mutate_at('read_term', list(~ if_else(read_code %in% 'H33z1',
                                        'Asthma attack NOS', 
                                        .))) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

saveRDS(exacerbation, file = 'lists_out/exacerbation.RDS', compress = FALSE)
