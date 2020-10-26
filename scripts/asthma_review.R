#### asthma_review ####

# Description: asthma review appointments
# cat1: asthma_review
# cat2: NA
# score: NA

source('setup.R')

asthma_review <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('_id')), any_vars(. %in% 'REV_COD'))

asthma_review %>%
  select(read_code, contains('_v2_'))

asthma_review %>%
  select(read_code, contains('_v3_'))

# asthma review codes are consistent across QOF v36-38

asthma_review %>%
  mutate(read_term = if_else(!is.na(v36_v2_term), v36_v2_term, v36_v3_term)) %>%
  select(read_code, read_term) %>%
  mutate(cat1 = 'asthma_review',
         cat2 = NA_character_,
         score = NA_real_) %>%
  distinct()

# check for duplicates in read_code
asthma_review$read_code[duplicated(asthma_review$read_code)]

saveRDS(asthma_review, file = 'lists_out/asthma_review.RDS', compress = FALSE)