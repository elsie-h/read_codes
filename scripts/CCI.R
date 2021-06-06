#### CCI ####

# Description: codes for derving the Charlson Comorbidity Index (CCI)
# cat1: CCI
# cat2: CCI component
# score: CCI weight

source('setup.R')

# Read codes from Nwaru2020
cci_nwaru <- read_csv(file = 'lists_in/Nwaru2020/cl_cci_nwaru.csv')

# use the scores from the khan paper
cci <- read_csv("lists_in/Khan2010/khan_2010_cci.csv") %>%
  # because code_id in OPCRD is only 5 chatacters
  mutate(read_code = str_extract(`Read/OXMIS code`, '^.{5}')) %>%
  select(read_code, read_term = `Read/OXMIS term`, 
         score = `Charlson score weight`,
         cat2 = `Charlson disease category`) %>%
  mutate(cat1 = 'cci') %>%
  # some of the codes here are OXMIS, join to Nwaru list to only select Read codes
  right_join(cci_nwaru, by = 'read_code') %>%
  mutate_at('cat2', list(~ case_when(. %in% 'Mod liver disease' ~ 'moderate liver disease',
                                     TRUE ~ str_to_lower(.)))) %>%
  group_by(cat2) %>%
  mutate(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at('read_code', list(~ if_else(str_detect(., '^K458'),
                                        'K458.',
                                        .))) %>%
  mutate_at('read_term', list(~ str_remove_all(., '\\?'))) %>%
  filter(!is.na(read_code)) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  mutate_at('cat2', list(~ if_else(. %in% 'aids', 'AIDS', .))) %>%
  # if there are multiple terms for a Read code, select one term (the first one listed)
  distinct(read_code, score, cat1, cat2, .keep_all = TRUE)

# make sure all Read codes are 5 characters and fix if not
cci %>%
  filter(str_length(read_code)<5)
cci <- cci %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# map V2 to CTV3
cci_v3 <- cci %>%
  left_join(rctctv3map %>% 
              distinct(V2_CONCEPTID, CTV3_CONCEPTID), 
            by = c('read_code' = 'V2_CONCEPTID')) %>%
  select(-read_code) %>%
  rename(read_code = CTV3_CONCEPTID, mapped_read_term = read_term)

cci <- cci %>%
  bind_rows(cci_v3) %>%
  distinct()

# save
write_csv(cci, 
          path = file.path(opcrd_analysis_path, 'CCI.csv'))
write_csv(cci %>%
            arrange(cat2, read_code, read_term) %>%
            select(Disease = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   weight = score),
          path = 'lists_out/CCI.csv')
