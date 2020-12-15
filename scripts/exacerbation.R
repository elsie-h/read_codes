#### exacerbation ####

# Description: exacerbation
# cat1: exacerbation
# cat2: NA, number
# score: NA

source('setup.R')

exacerbation_elsie <- read_delim("lists_in/Elsie/cl_exacerbation_elsie",
                           ",",
                           escape_double = FALSE, 
                           trim_ws = TRUE) 

exacerbation_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-asthma-attack-2020-11-05T13-33-34.csv") %>%
  rename(read_code = id, read_term = term)

exacerbation <- exacerbation_elsie %>%
  full_join(exacerbation_v3, by = 'read_code') %>%
  mutate(read_term = if_else(is.na(read_term.x), read_term.y, read_term.x)) %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'H33z1' ~ 'Asthma attack NOS',
                                          TRUE ~ .))) %>%
  distinct(read_code, read_term, cat1, cat2) %>%
  mutate(cat1 = 'exacerbation') 

# check for duplicates in read_code
exacerbation$read_code[duplicated(exacerbation$read_code)]

write_csv(exacerbation, path = 'lists_out/exacerbation.csv')

# table for appendix
exacerbation %>% filter(cat2 %in% 'number')

exacerbation %>%
  arrange(read_term) %>%
  filter(!(cat2 %in% 'number')) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Read codes from asthma exacerbations (see \\nameref{cha:ehr:methods:pre:exacerbation} for methods)',
         label = 'tab:app:rc_exacerbation',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'exacerbation')
