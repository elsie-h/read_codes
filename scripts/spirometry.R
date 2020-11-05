#### Spirometry ####

# Description: identify events with a spirometry read code
# cat1: spirometry
# cat2: NA, FEV1, FEV1FVC normal, FEV1FVC abnormal, FEV1FVC ratio or percent, ppFEV1, FEV1FVC < 70%, FEV1FVC > 70%, predicted FEV1, predicted FVC
# score: NA 

source('setup.R')

spirometry <- read_delim("lists_in/Elsie/cl_spirometry_elsie",
                            "|",
                            escape_double = FALSE, 
                            trim_ws = TRUE) %>%
  filter(!(read_code %in% '745C0')) %>%
  add_row(read_code = '68M..',
          read_term = 'Spirometry screening',
          cat1 = 'spirometry') %>%
  add_row(read_code = 'XaCJK',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = '339i.',	
          read_term = 'FVC/Expected FVC percent',
          cat2 = NA_character_) %>%
  add_row(read_code = '339P.',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = 'XaCJL',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') %>%
  add_row(read_code = '339Q.',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') %>%
  mutate(cat1 = 'spirometry',
         score = NA_real_) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% c('XaPpI') ~ 'FVC',
                                     read_code %in% c('339j.', '339l.', 'XaJ9B', 'XaJ9D') ~ 'FEV1FVC ratio or percent',
                                     read_code %in% c('339a.', '339e.', 'XaIxQ', 'XaIxU') ~ 'FEV1',
                                     read_code %in% c('33971') ~ NA_character_,
                                     TRUE ~ .))) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
spirometry$read_code[duplicated(spirometry$read_code)]

saveRDS(spirometry, file = 'lists_out/spirometry.RDS', compress = FALSE)
