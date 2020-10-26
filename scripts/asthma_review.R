#### asthma_review ####

# Description: asthma review appointments
# cat1: asthma_review
# cat2: NA
# score: NA

source('setup.R')

asthma_review <- read_delim("lists_in/Elsie/cl_asthma_review_elsie.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'asthma_review',
         cat2 = NA_character_,
         score = NA_real_) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
asthma_review$read_code[duplicated(asthma_review$read_code)]

saveRDS(asthma_review, file = 'lists_out/asthma_review.RDS', compress = FALSE)
