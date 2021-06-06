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

exacerbation_v3 <- read_csv("lists_in/Elsie/elsie-horne-asthma-attack-2020-11-05T13-33-34.csv") %>%
  rename(read_code = id, read_term = term) %>%
  # add 'status asthmaticus' ones from the Newby paper
  add_row(read_code = 'XE0YV', read_term = 'Status asthmaticus NOS') %>%
  add_row(read_code = 'XE0YS', read_term = 'Extrinsic asthma with status asthmaticus') %>%
  add_row(read_code = 'XE0YU', read_term = 'Intrinsic asthma with status asthmaticus') 

exacerbation <- exacerbation_elsie %>%
  full_join(exacerbation_v3, by = 'read_code') %>%
  mutate(read_term = if_else(is.na(read_term.x), read_term.y, read_term.x)) %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'H33z1' ~ 'Asthma attack NOS',
                                          TRUE ~ .))) %>%
  distinct(read_code, read_term, cat1, cat2) %>%
  mutate(cat1 = 'exacerbation') 

# make sure all Read codes are 5 characters and fix if not
exacerbation %>%
  filter(str_length(read_code)<5)
exacerbation <- exacerbation %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
exacerbation$read_code[duplicated(exacerbation$read_code)]

# check mapping
# map V2 -> CTV3
exacerbation %>% map_V2_CTV3()
# map CTV3 -> V2
exacerbation %>% map_CTV3_V2()

write_csv(exacerbation, 
          path = file.path(opcrd_analysis_path, 'exacerbation.csv'))
write_csv(exacerbation %>%
            mutate_at('cat2', list(~ if_else(is.na(.), 'occurence', .))) %>%
            arrange(cat2, read_code) %>%
            select(`Read code` = read_code,
                   Term = read_term,
                   Category = cat2), 
          path = 'lists_out/exacerbation.csv')
