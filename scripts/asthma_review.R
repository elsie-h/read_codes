#### asthma_review ####

# Description: asthma review appointments
# cat1: asthma_review
# cat2: NA
# score: NA

source('setup.R')

asthma_review <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
  filter_at(vars(ends_with('_id')), any_vars(. %in% 'REV_COD'))

asthma_review %>%
  select(read_code, contains('_v2_'))

asthma_review %>%
  select(read_code, contains('_v3_'))

# asthma review codes are consistent across QOF v36-38
# will have to assume that pre-36 were consistent too, as they're no longer available

asthma_review <- asthma_review %>%
  mutate(read_term = if_else(!is.na(v36_v2_term), v36_v2_term, v36_v3_term)) %>%
  select(read_code, read_term) %>%
  mutate(QOF = 1) %>%
  distinct() %>%
  # add those from Newby paper
  add_row(read_code = 'XM1Xb', read_term = 'Asthma monitoring', QOF = 0) %>%
  add_row(read_code = '9N1d.', read_term = 'Seen in asthma clinic', QOF = 0) %>%
  add_row(read_code = 'XaX3n', read_term = 'Asthma review using Royal College of Physicians three questions', QOF = 1) %>%
  add_row(read_code = '66Yp.', read_term = 'Asthma review using Royal College of Physicians three questions', QOF = 1) %>%
  add_row(read_code = '9NI8.', read_term = 'Asthma outreach clinic', QOF = 0) %>%
  add_row(read_code = 'XaBU2', read_term = 'Asthma monitoring call', QOF = 0) %>%
  add_row(read_code = '9OJ1.', read_term = 'Attends asthma monitoring', QOF = 0) %>%
  add_row(read_code = 'XaL05', read_term = 'Asthma outreach clinic', QOF = 0) %>%
  add_row(read_code = '663..', read_term = 'Respiratory disease monitoring', QOF = 0) %>%
  add_row(read_code = 'XE1Sw', read_term = 'Respiratory disease monitoring', QOF = 0) %>%
  add_row(read_code = 'XM1Xg', read_term = 'Chronic respiratory disease monitoring', QOF = 0) %>%
  add_row(read_code = '9OJ1.', read_term = 'Attends asthma monitoring', QOF = 0) %>%
  mutate(cat1 = 'asthma_review',
         cat2 = NA_character_,
         score = NA_real_) 

# make sure all Read codes are 5 characters and fix if not
asthma_review %>%
  filter(str_length(read_code)<5)
asthma_review <- asthma_review %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
asthma_review$read_code[duplicated(asthma_review$read_code)]

# check mapping
# map V2 -> CTV3
asthma_review %>% map_V2_CTV3()
# map CTV3 -> V2
asthma_review %>% map_CTV3_V2()

# save the code list
write_csv(asthma_review, 
          path = file.path(opcrd_analysis_path, 'asthma_review.csv'))
write_csv(asthma_review %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term) %>%
            mutate(QOF = 'Y'),
          path = 'lists_out/asthma_review.csv')
