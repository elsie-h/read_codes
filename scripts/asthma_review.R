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
  mutate(cat1 = 'asthma_review',
         cat2 = NA_character_,
         score = NA_real_,
         QOF = 1) %>%
  distinct()

# check for duplicates in read_code
asthma_review$read_code[duplicated(asthma_review$read_code)]

# save the code list
write_csv(asthma_review, 
          path = file.path(opcrd_analysis_path, 'asthma_review.csv'))
write_csv(asthma_review %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term) %>%
            mutate(QOF = 'Y'),
          path = 'lists_out/asthma_review.csv')

# table for appendix
asthma_review %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Asthma review Read codes from QOF business rules (see \\sectionref{cha:ehr:methods} for methods)',
         label = 'tab:app:rc_asthma_review',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'asthma_reviews')

